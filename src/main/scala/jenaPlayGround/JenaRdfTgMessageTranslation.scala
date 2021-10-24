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

import scala.collection.immutable.SortedMap
import scala.util.chaining.scalaUtilChainingOps



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
   * DSL to quickly convert Literal to its JavaValue
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

      maybeLiteral        <- IO { Option(ontoResource.getPropertyValue(ResourceFactory.createProperty(propUri))) map {_.asLiteral()}  }

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

  def inferCaseInsensitiveTypeFromUri(uri: String): String = {
    uri.split("/entity/").last.split("/")
      .pipe { array => s"https://data.elsevier.com/lifescience/schema/${array(0)}/${array(1)}"}
  }

  def makeLookUpFromFdnSchema(fdnGraphSchema: FdnGraphSchema): SortedMap[String, ObjectType] = {

    val eTypes: List[(String, ObjectType)] = fdnGraphSchema.entityTypes.map(entityType => (entityType.entityType, entityType))
    val rTypes: List[(String, ObjectType)] = fdnGraphSchema.relationTypes.map(relationType => (relationType.relationType, relationType))

    SortedMap[String, ObjectType](List(eTypes, rTypes).flatten: _*)(scala.math.Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))
  }

  def translateResourceMessage(eUri: String, messageFile: String)(lookup: SortedMap[String, ObjectType]): IO[TgMessage] = {

    for {

      insensitiveType       <- IO { inferCaseInsensitiveTypeFromUri(eUri) }
      objType               <- IO { lookup(insensitiveType) }
      _                     <- IO { info(objType.toString) }

      tgMessage             <-

        objType match {
          case eType: EntityType => translateEntity(eUri, eType, messageFile)
          case rType: RelationType => IO.raiseError(new Throwable("Unsupported Resource Type"))
        }

    } yield tgMessage

  }

  def translateEntity(eUri: String, entityType: EntityType, messageFile: String): IO[TgMessage] = {

    for {

      ontDoc                           <- IO { OntDocumentManager.getInstance() }
      _                                <- IO { ontDoc.setProcessImports(false) }

      ontModel                         <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
      _                                <- IO { ontModel.read(messageFile, Lang.TURTLE.getName) }
      ontResource                      <- IO { ontModel.getOntResource(eUri) }


      dataProperties                   <- IO.pure { entityType.dataProperties }
      attributes                       <- (dataProperties traverse makeAttributeFromDataProperty(ontResource) ) map { _.flatten }

      tgMessage                        <- IO.pure { TgMessage(List(Vertex(entityType.entityType, eUri, attributes)), List()) }

      _                                <- IO { ontModel.close() }

    } yield tgMessage

  }

  def translateRelation(): IO[TgMessage] = ???


  val program = for {

    _                                <- IO { info ("Started Translating Resource with Uri: https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488 ")}

    eUri                             <- IO.pure { "https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488" }
    messageFile                      <- IO.pure { "messages/smallmol.ttl" }

    fdnSchema                        <- fdnParser.program
    lookup                           = makeLookUpFromFdnSchema(fdnSchema)

    tgMessage                        <- translateResourceMessage(eUri, messageFile)(lookup)

    _                                <- IO { info ( tgMessage.toString ) }


  } yield ()

  program.unsafeRunSync()

/*  val program = for {


    _                                <- IO { info ("Started Translating Resource with Uri: https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488 ")}

    eUri                             <- IO.pure { "https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488" }


    ontDoc                           <- IO { OntDocumentManager.getInstance() } // Set your global Ontology Manager without any LocationMapper, so the reliance on the StreamMndgr is ensured. The process is broken
    _                                <- IO { ontDoc.setProcessImports(false) }
    ontModel                         <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
    _                                <- IO { ontModel.read("messages/smallmol.ttl", Lang.TURTLE.getName) }
    ontResource                      <- IO { ontModel.getOntResource(eUri) }




    insensitiveType                  = inferCaseInsensitiveTypeFromUri(eUri)

    fdnSchema                        <- fdnParser.program
    lookup                           = makeLookUpFromFdnSchema(fdnSchema)

    objType                          <- IO { lookup(insensitiveType) }

    _                                <- IO { info(objType.asInstanceOf[EntityType].show) }

    dataProperties                   <- IO { objType.asInstanceOf[EntityType].dataProperties}
    attributes                       <- (dataProperties traverse makeAttributeFromDataProperty(ontResource) ) map { _.flatten }

    _                                <- IO { info (s"Message Translated with result:\n${attributes.toString()}")}

  } yield ()


  program.unsafeRunSync()*/

  /*val program = for {
    _                                <- IO { info ("Started Translating Resource with Uri: https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488 ")}

    ontDoc                           <- IO { OntDocumentManager.getInstance() } // Set your global Ontology Manager without any LocationMapper, so the reliance on the StreamMndgr is ensured. The process is broken
    _                                <- IO { ontDoc.setProcessImports(false) }
    ontModel                         <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
    _                                <- IO { ontModel.read("messages/smallmol.ttl", Lang.TURTLE.getName) }

    ontResource                      <- IO { ontModel.getOntResource("https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488") }
    sDataProperty                    <- IO.pure { DataProperty("https://data.elsevier.com/lifescience/schema/resnet/flagss", XSD.xdouble.getURI,  None, Some(1))}
    mDataProperty                    <- IO.pure { DataProperty("https://data.elsevier.com/lifescience/schema/resnet/alias", XSD.xstring.getURI,  None, None)}


    //maybeSingledValueAttribute       <- makeSingleValuedAttributeFromDataProperty(ontResource, sDataProperty)
    //maybeMultiValuedValueAttribute   <- makeMultiValuedAttributeFromDataProperty(ontResource, mDataProperty)
    //_                                <- IO.println(maybeSingledValueAttribute)
    //_                                <- IO.println(maybeMultiValuedValueAttribute)

    attributes                       <- (List(mDataProperty, sDataProperty) traverse makeAttributeFromDataProperty(ontResource) ) map { _.flatten }

    _                                <- IO { info (s"Message Translated with result:\n${attributes.toString()}")}

  } yield ()

  program.unsafeRunSync()*/

}
