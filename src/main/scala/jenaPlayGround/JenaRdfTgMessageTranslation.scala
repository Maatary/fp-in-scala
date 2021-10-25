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

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import scala.collection.immutable.SortedMap
import scala.util.chaining.scalaUtilChainingOps



object JenaRdfTgMessageTranslation extends App {

  this.logger
    .withMinimumLevel(Level.Debug)
    .replace()


  implicit class OntModelOps(model: OntModel) {
    def toPrettyString: IO[String] = {
      for {
        outStream <- IO.pure { new ByteArrayOutputStream() }
        _         <- IO { model.write(outStream, Lang.TTL.getName) }
        outString <- IO { outStream.toString(StandardCharsets.UTF_8)}
      } yield outString
    }
  }

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




  def getDataPropValue(ontoResource:  OntResource, propUri: String, rdfDataType: RDFDatatype): IO[Option[String]] = {
    for {

      maybeLiteral        <- IO { Option(ontoResource.getPropertyValue(ResourceFactory.createProperty(propUri))) map {_.asLiteral()}  }

      maybeStringValue    <- maybeLiteral traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield maybeStringValue
  }

  def getDataPropValues(ontoResource:  OntResource, propUri: String, rdfDataType: RDFDatatype): IO[List[String]] = {

    for {

      literals     <- IO { ontoResource.listPropertyValues(ResourceFactory.createProperty(propUri)).asScala.toList map(_.asLiteral()) }

      stringValues <- literals traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield stringValues
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

  def makeAttributeFromDataProperty(ontoResource: OntResource) (dataProperty: DataProperty): IO[Option[TgAttribute]] = dataProperty match {
    case DataProperty(_,_,_, None)                   => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case DataProperty(_,_,_, Some(card)) if card > 1 => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case _                                           => makeSingleValuedAttributeFromDataProperty(ontoResource, dataProperty)
  }


  def getObjectPropValues(ontoResource:  OntResource, propUri: String): IO[List[String]] = {

    IO {
      ontoResource
        .listPropertyValues(ResourceFactory.createProperty(propUri))
        .asScala
        .toList map{ _.asResource().getURI }
    } onError { _ => IO { error(s"Failed on retrieving resource values for property [$propUri] for resource [$ontoResource] ")} }
  }

  def makeEdgeFromRelationPropertyTarget(relPropUri: String, sourceEntityUri: String, sourceEntityType: String, lookup: SortedMap[String, ObjectType])(targetEntityUri: String): IO[Edge] = {
    for  {
      insensitiveType       <- IO { inferCaseInsensitiveTypeFromUri(targetEntityUri) }
      targetEntityType      <- IO { lookup(insensitiveType).asInstanceOf[EntityType] }
    } yield Edge(relPropUri, sourceEntityUri, sourceEntityType, targetEntityUri, targetEntityType.entityType,List())
  }

  def makeEdgesFromRelationProperty(entityResource: OntResource, sourceEntityType: String, lookup: SortedMap[String, ObjectType])(relationProperty: RelationProperty): IO[List[Edge]] = {
    for {

      targetEntityUris <- getObjectPropValues(entityResource, relationProperty.linkType)

      edges            <- targetEntityUris traverse makeEdgeFromRelationPropertyTarget(relationProperty.linkType, entityResource.getURI, sourceEntityType,lookup)

    } yield edges

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


  def translateRelation(eUri: String, relationType: RelationType, entityResource: OntResource, lookup: SortedMap[String, ObjectType]): IO[TgMessage] = {
    IO { info (s"Resource $eUri Inferred as Relation. Starting Entity Translation Process ..." ) } *> IO.raiseError(new Throwable("Unsupported Resource Type"))
  }


  def translateEntity(entityUri: String, entityType: EntityType, entityResource: OntResource, lookup: SortedMap[String, ObjectType]): IO[TgMessage] = {

    for {

      _                       <- IO { info (s"Resource $entityUri Inferred as Entity. Starting Entity Translation Process ..." ) }

      _                       <- if (this.logger.includes(Level.Debug)) IO { debug(s"Resource $entityUri FdnSchema EntityType Description: [${entityType.show}]") } else IO.unit

      dataProperties          <- IO.pure { entityType.dataProperties }
      attributes              <- { dataProperties traverse makeAttributeFromDataProperty(entityResource) } map { _.flatten }

      relationProperties      <- IO.pure {entityType.relationProperties}
      edges                   <- { relationProperties traverse makeEdgesFromRelationProperty(entityResource, entityType.entityType, lookup) } map {_.flatten}

      tgMessage               <- IO.pure { TgMessage(List(Vertex(entityType.entityType, entityUri, attributes)), edges) }

      _                       <- IO { info (s"Entity Translation for Resource $entityUri was successful" ) }

    } yield tgMessage

  }

  def translateResourceMessage(resUri: String, messageFile: String)(lookup: SortedMap[String, ObjectType]): IO[TgMessage] = {

    for {

      _                     <- IO { info (s"Start Translating Message for Resource with Uri: $resUri")}

      ontDoc                <- IO { OntDocumentManager.getInstance() }
      _                     <- IO { ontDoc.setProcessImports(false) }

      ontModel              <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
      _                     <- IO { ontModel.read(messageFile, Lang.TURTLE.getName) }
      _                     <- if (this.logger.includes(Level.Debug)) ontModel.toPrettyString.flatMap{ m => IO { debug(s"Resource $resUri Message: [\n$m]") } } else IO.unit

      maybeResource         <- IO { Option(ontModel.getOntResource(resUri)) }
      resource              <-

        maybeResource match {

          case None    => ontModel.toPrettyString.flatMap { m => IO { error(s"Resource $resUri does not exists in message:\n$m") } } *> IO.raiseError(new Throwable(s"ontModel.getOntResource($resUri) returned Null"))

          case Some(r) => IO.pure { r }
        }

      resType               <- IO { resource.getRDFType.getURI } onError { _ => ontModel.toPrettyString.flatMap { m => IO { error( s"The Resource $resUri does not have a Type in message:\n$m" ) }} }
      _                     <- IO { info (s"Extracted Type $resType from Message for resource $resUri") }

      objType               <- IO { lookup(resType) }


      tgMessage             <-

        objType match {

          case eType: EntityType => translateEntity(resUri, eType, resource, lookup)

          case rType: RelationType => translateRelation(resUri, rType, resource, lookup)

        }

      _                     <- IO { info (s"Successfully Translated Message for Resource with Uri: $resUri")}
      _                     <- IO { ontModel.close() }

    } yield tgMessage

  }


  val program = for {

    eUri                             <- IO.pure { "https://data.elsevier.com/lifescience/entity/reaxys/bioassay/517534" }
    messageFile                      <- IO.pure { "messages/bioassay.ttl" }

    fdnSchema                        <- fdnParser.program("elsevier_entellect_proxy_schema_reaxys.ttl")
    lookup                           = makeLookUpFromFdnSchema(fdnSchema)

    tgMessage                        <- translateResourceMessage(eUri, messageFile)(lookup)

    _                                <- IO { info ( tgMessage.show ) }


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
