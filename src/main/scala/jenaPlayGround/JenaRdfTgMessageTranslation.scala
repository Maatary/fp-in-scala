package jenaPlayGround




import cats.effect.IO
import cats.effect.unsafe.implicits.global
import circe.TgDataTypes._
import jenaPlayGround.DataTypes._
import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec, OntResource}
import org.apache.jena.rdf.model.{Literal, ModelFactory, ResourceFactory}
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.riot.system.stream.StreamManager
import org.apache.jena.sys.JenaSystem
import org.apache.jena.vocabulary.{RDF, XSD}
import org.apache.jena.datatypes.{RDFDatatype, TypeMapper}
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.datatypes.xsd.impl.{RDFLangString, XSDBaseNumericType, XSDBaseStringType, XSDDateTimeType, XSDDouble, XSDFloat}
import org.apache.jena.util.SplitIRI._

import scala.jdk.CollectionConverters._
import scribe._
import cats.syntax.all._

object JenaRdfTgMessageTranslation extends App {



  /**
   * DSL to quickly convert RDF DataTypes to TG DataType
   *
   * Assume that the RDFDataType have been created with Jena TypeMapper Global Instance.
   */
  implicit class RDFDatatypeOps(rdfDataType: RDFDatatype) {
    def asTgDataType: IO[TgDataType] = rdfDataType match {
      case _: RDFLangString       => IO.pure(STRING)
      case _: XSDBaseStringType   => IO.pure(STRING)
      case _: XSDFloat            => IO.pure(NUMBER)
      case _: XSDDouble           => IO.pure(NUMBER)
      case _: XSDBaseNumericType  => IO.pure(NUMBER)
      case _: XSDDateTimeType     => IO.pure(STRING)
      case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDboolean) => IO.pure(STRING)
      case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDanyURI) => IO.pure(STRING)
      case e                      => IO {warn(s"unknown XSD: $e defaulting to String")} *> IO.pure(STRING)
    }
  }


  /**
   * DSL to quickly convert RDF DataTypes to TG DataType
   *
   * Assume that the RDFDataType have been created with Jena TypeMapper Global Instance.
   */
  implicit class LiteralOps(literal: Literal) {
    def asJavaValue(rdfDataType: RDFDatatype): IO[Any] = {
      rdfDataType match {
        case _: RDFLangString       => IO { literal.getString }
        case _: XSDBaseStringType   => IO { literal.getString }
        case _: XSDFloat            => IO { literal.getDouble }
        case _: XSDDouble           => IO { literal.getDouble }
        case _: XSDBaseNumericType  => IO { literal.getDouble }
        case _: XSDDateTimeType     => IO { literal.getString }
        case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDboolean) => IO { literal.getString } //Not quite right but we don't support boolean yet
        case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDanyURI) => IO { literal.getString }
        case e                      => IO { warn(s"unknown XSD: $e defaulting to trying conversion to a java String")  } *> IO { literal.getString }
      }
    } onError { _ => IO { error(s"""Can't convert literal [${literal.getLexicalForm}] of DataType [${literal.getDatatypeURI}] to supplied DataType [${rdfDataType.getURI}]""") } }
  }




  def makeSingleValuedAttributeFromDataProperty(ontoResource:  OntResource, dataProperty: DataProperty): IO[Option[SingleValuedAttribute]] = {

    for {

      typeMapper                      <- IO { TypeMapper.getInstance() }
      rdfDataType                     <- IO { typeMapper.getSafeTypeByName(dataProperty.dataType) }

      maybeValue                      <- getDataPropValue(ontoResource, dataProperty.linkType, rdfDataType)

      maybeSingleValuedAttribute      <-
        maybeValue
          .fold
          {
            IO.pure[Option[SingleValuedAttribute]] { None }
          }
          { value =>
            rdfDataType.asTgDataType flatMap { dt => IO { Option(SingleValuedAttribute(localname(dataProperty.linkType), value , dt)) } }
          }

    } yield  maybeSingleValuedAttribute

  }

  def getDataPropValue(ontoResource:  OntResource, propUri: String, rdfDataType: RDFDatatype): IO[Option[String]] = {
    for {

      maybeLiteral        <- IO { Option(ontoResource.getPropertyValue(ResourceFactory.createProperty(propUri)).asLiteral()) }

      maybeStringValue    <- maybeLiteral traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield maybeStringValue
  }

  def makeMultiValuedAttributeFromDataProperty(ontoResource:  OntResource, dataProperty: DataProperty): IO[Option[MultiValuedAttribute]] = {

    for {

      typeMapper                      <- IO { TypeMapper.getInstance() }
      rdfDataType                     <- IO { typeMapper.getSafeTypeByName(dataProperty.dataType) }

      values                          <- getDataPropValues(ontoResource, dataProperty.linkType, rdfDataType)

      maybeMultiValuedAttribute       <-
        values match {
          case Nil            => IO.pure[Option[MultiValuedAttribute]] { None }
          case _              => rdfDataType.asTgDataType flatMap { dt => IO { Option(MultiValuedAttribute(localname(dataProperty.linkType), values, dt)) } }
        }

    } yield maybeMultiValuedAttribute

  }

  def getDataPropValues(ontoResource:  OntResource, propUri: String, rdfDataType: RDFDatatype): IO[List[String]] = {

    for {

      literals     <- IO { ontoResource.listPropertyValues(ResourceFactory.createProperty(propUri)).asScala.toList map(_.asLiteral()) }

      stringValues <- literals traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield stringValues
  }


  def makeAttributeFromDataProperty(ontoResource: OntResource) (dataProperty: DataProperty): IO[Option[TgAttribute]] = dataProperty match {
    case DataProperty(_,_,_, None)                   => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case DataProperty(_,_,_, Some(card)) if card > 1 => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case _                                           => makeSingleValuedAttributeFromDataProperty(ontoResource, dataProperty)
  }

  val program = for {

    _                                <- IO { info ("Started Translating Resource with Uri: https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488 ")}

    ontDoc                           <- IO { OntDocumentManager.getInstance() } // Set your global Ontology Manager without any LocationMapper, so the reliance on the StreamMndgr is ensured. The process is broken
    _                                <- IO { ontDoc.setProcessImports(false) }
    ontModel                         <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
    _                                <- IO { ontModel.read("messages/smallmol.ttl", Lang.TURTLE.getName) }

    ontResource                      <- IO { ontModel.getOntResource("https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488") }
    sDataProperty                    <- IO.pure { DataProperty("https://data.elsevier.com/lifescience/schema/resnet/flags", XSD.xdouble.getURI,  None, Some(1))}
    mDataProperty                    <- IO.pure { DataProperty("https://data.elsevier.com/lifescience/schema/resnet/alias", XSD.xstring.getURI,  None, None)}


    /*maybeSingledValueAttribute     <- makeSingleValuedAttributeFromDataProperty(ontResource, sDataProperty)
    maybeMultiValuedValueAttribute   <- makeMultiValuedAttributeFromDataProperty(ontResource, mDataProperty)
    _                                <- IO.println(maybeSingledValueAttribute)
    _                                <- IO.println(maybeMultiValuedValueAttribute)*/

    attributes                       <- (List(mDataProperty, sDataProperty) traverse makeAttributeFromDataProperty(ontResource) ) map { _.flatten }

    _                                <- IO { info (s"Message Translated with result:\n${attributes.toString()}")}


  } yield ()

  program.unsafeRunSync()

}