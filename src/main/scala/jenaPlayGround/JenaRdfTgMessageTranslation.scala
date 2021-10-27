package jenaPlayGround




import cats.effect.IO
import cats.effect.unsafe.implicits.global
import circe.TgDataTypes._
import jenaPlayGround.DataTypes._
import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec, OntResource}
import org.apache.jena.rdf.model.{Literal, ModelFactory, ResourceFactory}
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.vocabulary.{RDF, XSD}
import org.apache.jena.datatypes.{RDFDatatype, TypeMapper}
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.datatypes.xsd.impl.{RDFLangString, XSDBaseNumericType, XSDBaseStringType, XSDDateTimeType, XSDDouble, XSDFloat}
import org.apache.jena.util.SplitIRI._

import scala.jdk.CollectionConverters._
import scribe._
import cats.syntax.all._
import scribe.writer.SystemOutputWriter

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import scala.collection.immutable.SortedMap
import scala.util.chaining.scalaUtilChainingOps



object JenaRdfTgMessageTranslation extends App {



  Logger.root
    .withMinimumLevel(Level.Info).replace()

  Logger(classOf[jenaPlayGround.fdnParser.type].getName)
    .withMinimumLevel(Level.Info)
    .replace()
  Logger(classOf[JenaRdfTgMessageTranslation.type].getName)
    .withMinimumLevel(Level.Info)
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
      case e                      => IO { warn(s"unknown XSD: $e defaulting to String")} *> IO.pure(STRING)
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
        case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDboolean) => IO { literal.getString } //Not quite right but we don't support boolean yet, not even sure jena will let it fly.
        case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDanyURI) => IO { literal.getString }
        case e                      => IO { warn(s"unknown XSD: $e defaulting to trying conversion to a java String")  } *> IO { literal.getString }
      }
    } onError { _ => IO { error(s"""Can't convert literal [${literal.getLexicalForm}] of DataType [${literal.getDatatypeURI}] to supplied DataType [${rdfDataType.getURI}]""") } }
  }


  def inferCaseInsensitiveResourceTypeFromUri(rUri: String): IO[String] = {
    IO {
      if (rUri.contains("entity"))
        rUri.split("/entity/").last.split("/").pipe { array => s"https://data.elsevier.com/lifescience/schema/${ array(0) }/${ array(1) }" }
      else
        rUri.split("/taxonomy/").last.split("/").pipe { array => s"https://data.elsevier.com/lifescience/schema/${ array(0) }/${ array(1) }" }
    } onError { _ => IO { error(s"Tried to Infer Case Insensitive Type From Non Compliant Resource Uri: $rUri") } }
  }

  def makeLookUpFromFdnSchema(fdnGraphSchema: FdnGraphSchema): SortedMap[ResourceType, ObjectType] = {

    val eTypes: List[(ResourceType, ObjectType)] = fdnGraphSchema.entityTypes.map(entityType => (entityType.entityType, entityType))
    val rTypes: List[(ResourceType, ObjectType)] = fdnGraphSchema.relationTypes.map(relationType => (relationType.relationType, relationType))

    SortedMap[ResourceType, ObjectType](List(eTypes, rTypes).flatten: _*)(scala.math.Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))
  }



  def getDataPropValue(ontoResource:  OntResource, dataProperty: DataProperty, rdfDataType: RDFDatatype): IO[Option[String]] = {
    for {

      maybeLiteral        <- IO { Option ( ontoResource.getPropertyValue(ResourceFactory.createProperty(dataProperty.linkType)) ) map {_.asLiteral()}  } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)

      maybeStringValue    <- maybeLiteral traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield maybeStringValue
  }

  def getDataPropValues(ontoResource:  OntResource, dataProperty: DataProperty, rdfDataType: RDFDatatype): IO[List[String]] = {
    for {

      literals     <- IO { ontoResource.listPropertyValues(ResourceFactory.createProperty(dataProperty.linkType)).asScala.toList map(_.asLiteral()) } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)

      stringValues <- literals traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield stringValues
  }

  def makeSingleValuedAttributeFromDataProperty(ontoResource:  OntResource, dataProperty: DataProperty): IO[Option[SingleValuedAttribute]] = {

    for {

      typeMapper                      <- IO { TypeMapper.getInstance() }
      rdfDataType                     <- IO { typeMapper.getSafeTypeByName(dataProperty.dataType) }

      maybeValue                      <- getDataPropValue(ontoResource, dataProperty, rdfDataType)

      maybeSingleValuedAttribute      <-
        maybeValue
          .fold
          {
            IO.pure[Option[SingleValuedAttribute]] { None }
          }
          { value =>
            rdfDataType.asTgDataType flatMap { dt => IO.pure { Option(SingleValuedAttribute(localname(dataProperty.linkType), value , dt)) } }
          }

    } yield  maybeSingleValuedAttribute

  }

  def makeMultiValuedAttributeFromDataProperty(ontoResource:  OntResource, dataProperty: DataProperty): IO[Option[MultiValuedAttribute]] = {

    for {

      typeMapper                      <- IO { TypeMapper.getInstance() }
      rdfDataType                     <- IO { typeMapper.getSafeTypeByName(dataProperty.dataType) }

      values                          <- getDataPropValues(ontoResource, dataProperty, rdfDataType)

      maybeMultiValuedAttribute       <-
        values match {
          case Nil            => IO.pure[Option[MultiValuedAttribute]] { None }
          case _              => rdfDataType.asTgDataType flatMap { dt => IO.pure { Option(MultiValuedAttribute(localname(dataProperty.linkType), values, dt)) } }
        }

    } yield maybeMultiValuedAttribute

  }

  def makeAttributeFromDataProperty(ontoResource: OntResource) (dataProperty: DataProperty): IO[Option[TgAttribute]] = dataProperty match {
    case DataProperty(_,_,_, None)                   => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case DataProperty(_,_,_, Some(card)) if card > 1 => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case _                                           => makeSingleValuedAttributeFromDataProperty(ontoResource, dataProperty)
  }



  def getObjectPropResource(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[Option[OntResource]] = {
    IO { Option ( ontoResource.getPropertyValue(ResourceFactory.createProperty(objectProperty.linkType)) ) map {_.as(classOf[OntResource])} } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)
  }

  def getObjectPropValue(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[Option[String]] = {
    IO { Option ( ontoResource.getPropertyValue(ResourceFactory.createProperty(objectProperty.linkType)) ) map {_.asResource().getURI}  } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)
  }

  def getObjectPropValues(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[List[String]] = {
    IO { ontoResource.listPropertyValues(ResourceFactory.createProperty(objectProperty.linkType)).asScala.toList map { _.asResource().getURI } } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)
  }

  def makeSingleValuedAttributeFromObjectProperty(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[Option[SingleValuedAttribute]] = {
    for {

      maybeAnyUriValue            <- getObjectPropValue(ontoResource, objectProperty)

      maybeSingleValuedAttribute  <-
        maybeAnyUriValue
          .fold
          {
            IO.pure[Option[SingleValuedAttribute]] { None }
          }
          { anyUriValue =>
            IO.pure { Option ( SingleValuedAttribute(localname(objectProperty.linkType), anyUriValue , STRING) ) }
          }

    } yield maybeSingleValuedAttribute
  }

  def makeMultiValuedAttributeFromObjectProperty(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[Option[MultiValuedAttribute]] = {
    for {

      anyUriValues                <- getObjectPropValues(ontoResource, objectProperty)

      maybeMultiValuedAttribute   <-
        anyUriValues match {
          case Nil            => IO.pure[Option[MultiValuedAttribute]] { None }
          case _              => IO.pure { Option ( MultiValuedAttribute(localname(objectProperty.linkType), anyUriValues, STRING) ) }
        }

    } yield maybeMultiValuedAttribute
  }

  def makeAttributeFromSchemeProperty(ontoResource:  OntResource) (schemeProperty: SchemeProperty): IO[Option[SingleValuedAttribute]] = {
    makeSingleValuedAttributeFromObjectProperty(ontoResource, schemeProperty) // Because as of now for Scheme cardinality is always one
  }

  def makeAttributeFromAssociationProperty(ontoResource:  OntResource) (associationProperty: AssociationProperty): IO[Option[TgAttribute]] = associationProperty.max match {
    case None                   =>  makeMultiValuedAttributeFromObjectProperty(ontoResource, associationProperty)
    case Some(card) if card > 1 =>  makeMultiValuedAttributeFromObjectProperty(ontoResource, associationProperty)
    case _                      =>  makeSingleValuedAttributeFromObjectProperty(ontoResource, associationProperty)
  }


  def lookUpEntityType(lookup: SortedMap[ResourceType, ObjectType])(eUri: EntityUri): IO[EntityType] = {
    for {
      insensitiveResType  <- inferCaseInsensitiveResourceTypeFromUri(eUri) onError { _ => IO { error(s"Failed to Infer Case Insensitive Resource Type for Entity: $eUri")} }
      entityType          <- IO { lookup(insensitiveResType).asInstanceOf[EntityType] } onError { _ => IO { error(s"Failed to lookup EntityType for Entity [$eUri] from Its Inferred Case Insensitive Resource Type [$insensitiveResType]")} }
    } yield entityType
  }


  def translateSubEntity(compositionProperty: CompositionProperty, subEntityResource: EntityResource, subEntityType: EntityType): IO[UserDefinedAttribute] = {
    for {
      dataProperties          <- IO.pure {subEntityType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(subEntityResource) } map { _.flatten }
    } yield UserDefinedAttribute(compositionProperty.linkType, dataAttributes.map(attr => attr.aType -> attr))
  }

  def makeAttributeFromCompositionProperty(sourceEntity: EntityResource, lookup: SortedMap[ResourceType, ObjectType]) (compositionProperty: CompositionProperty): IO[Option[UserDefinedAttribute]] = {
    for {
      maybeSubEntityResource    <- getObjectPropResource(sourceEntity, compositionProperty)

      maybeSubEntityType        <- maybeSubEntityResource.map(_.getURI) traverse lookUpEntityType(lookup) // There should be no need to do the lookup, the type is in  compositionProperty.entityType

      maybeUserDefinedAttribute <- (maybeSubEntityResource, maybeSubEntityType) traverseN { (subEntityResource, SubEntityType ) => translateSubEntity(compositionProperty, subEntityResource, SubEntityType) }

    } yield maybeUserDefinedAttribute
  }

  def makeEdgeFromRelationPropertyTarget(sourceEntity: EntityResource, sourceEntityType: EntityType, relationProperty: RelationProperty, lookup: SortedMap[ResourceType, ObjectType])(targetEntityUri: String): IO[Edge] = {
    lookUpEntityType(lookup)(targetEntityUri)
      .map { eType => Edge(relationProperty.linkType, sourceEntity.getURI, sourceEntityType.entityType, targetEntityUri, eType.entityType, List())}
      .onError {_ => IO { error(s"Failed to lookup Entity ResourceType for Target Entity [$targetEntityUri] of RelationProperty [${relationProperty.linkType}] for Entity [${sourceEntity.getURI}]")} }
  }

  def makeEdgesFromRelationProperty(sourceEntity: EntityResource, entityType: EntityType, lookup: SortedMap[ResourceType, ObjectType])(relationProperty: RelationProperty): IO[List[Edge]] = {
    for {
      targetEntityUris <- getObjectPropValues(sourceEntity, relationProperty)
      edges            <- targetEntityUris traverse makeEdgeFromRelationPropertyTarget(sourceEntity, entityType, relationProperty, lookup)
    } yield edges
  }

  def getRelationTargetResourceData(relation: RelationResource, relationType: RelationType, lookup: SortedMap[ResourceType, ObjectType]): IO[Option[EntityResourceData]] = {
    for {

      targetLinkProperty              <- IO.pure { relationType.linkPropertyPairs.head.linkPropertyB }
      maybeTargetEntityUri            <- getObjectPropValue(relation, targetLinkProperty)

      maybeTargetEntityType           <- maybeTargetEntityUri traverse lookUpEntityType(lookup)
      maybeTargetEntityResourceType   <- IO.pure { maybeTargetEntityType.map(_.entityType) }


      maybeTargetEntityResourceData   <- IO.pure { maybeTargetEntityUri -> maybeTargetEntityResourceType mapN EntityResourceData }

      _                               <-

        maybeTargetEntityResourceData match {
          case Some(_)              => IO.unit
          case None                 => IO { warn(s"Failed to Retrieve SourceEntity for LinkProperty [${targetLinkProperty.linkType}] for Relation [${relation.getURI}]") }
        }

    } yield maybeTargetEntityResourceData
  }

  def getRelationSourceResourceData(relation: RelationResource, relationType: RelationType, lookup: SortedMap[ResourceType, ObjectType]): IO[Option[EntityResourceData]] = {

    for {

      sourceLinkProperty              <- IO.pure { relationType.linkPropertyPairs.head.linkPropertyA }
      maybeSourceEntityUri            <- getObjectPropValue(relation, sourceLinkProperty)

      maybeSourceEntityType           <- maybeSourceEntityUri traverse lookUpEntityType(lookup)
      maybeSourceEntityResourceType   <- IO.pure { maybeSourceEntityType.map(_.entityType) }

      maybeSourceEntityResourceData   <- IO.pure { maybeSourceEntityUri -> maybeSourceEntityResourceType mapN EntityResourceData }

      _                               <-

        maybeSourceEntityResourceData match {
          case Some(_)              => IO.unit
          case None                 => IO { warn(s"Failed to Retrieve SourceEntity for LinkProperty [${sourceLinkProperty.linkType}] for Relation [${relation.getURI}]") }
        }

    } yield maybeSourceEntityResourceData

  }



  //TODO Composition Properties
  def translateEntity(entity: EntityResource, entityUri: String, entityType: EntityType, lookup: SortedMap[ResourceType, ObjectType]): IO[TgMessage] = {

    for {

      _                       <- IO { info (s"Starting Entity Translation Process for Entity $entityUri" ) }

      _                       <- IO { debug(s"Entity $entityUri FdnSchema EntityType Description: [${entityType.show}]") }

      dataProperties          <- IO.pure { entityType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(entity) } map { _.flatten }

      schemeProperties        <- IO.pure { entityType.schemeProperties }
      schemeAttributes        <- { schemeProperties traverse makeAttributeFromSchemeProperty(entity) } map { _.flatten }

      compositionProperties   <- IO.pure { entityType.compositionProperties}
      compositionAttributes   <- { compositionProperties traverse makeAttributeFromCompositionProperty(entity,  lookup) } map { _.flatten }

      allAttributes           <- IO.pure { dataAttributes ++ schemeAttributes ++ compositionAttributes }

      relationProperties      <- IO.pure {entityType.relationProperties}
      edges                   <- { relationProperties traverse makeEdgesFromRelationProperty(entity, entityType, lookup) } map {_.flatten}

      tgMessage               <- IO.pure { TgMessage(List(Vertex(entityType.entityType, entityUri, allAttributes)), edges) }

      _                       <- IO { info (s"Entity Translation Process for Entity $entityUri was successful" ) }

    } yield tgMessage

  }


  def translateRelation(relation: RelationResource, relationUri: String, relationType: RelationType,  lookup: SortedMap[ResourceType, ObjectType]): IO[TgMessage] = {
    for {

      _                       <- IO { info (s"Starting Relation Translation Process for Relation $relationUri" ) }

      _                       <- IO { debug(s"Relation $relationUri FdnSchema RelationType Description: [${relationType.show}]") }

      dataProperties          <- IO.pure { relationType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(relation) } map { _.flatten }

      associationProperties   <- IO.pure { relationType.associationProperties }
      associationAttributes   <- { associationProperties traverse makeAttributeFromAssociationProperty(relation) } map { _.flatten }

      allAttributes           <- IO.pure { dataAttributes ++ associationAttributes }

      maybeSourceResourceData <- getRelationSourceResourceData(relation, relationType, lookup)
      maybeTargetResourceData <- getRelationTargetResourceData(relation, relationType, lookup)


      maybeTgMessage          <-

          IO.pure {
            maybeSourceResourceData -> maybeTargetResourceData mapN { (source, target) =>
              TgMessage ( List(), List(Edge(relationType.relationType, source.eUri, source.eType, target.eUri, target.eType, allAttributes)) )
            }
          }

      tgMessage               <-

          maybeTgMessage
            .fold
            {
              IO.pure { TgMessage(List(), List()) } <* IO { warn(s"Relation Translation Process for Relation $relationUri resulted in an Empty Message because of missing Mandatory LinkPair." ) }
            }
            { msg =>
              IO.pure { msg } <* IO { info (s"Relation Translation Process for Relation $relationUri was successful" ) }
            }


    } yield tgMessage
  }


  def translateResourceMessage(resUri: String, messageFile: String)(lookup: SortedMap[ResourceType, ObjectType]): IO[TgMessage] = {

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

      objType               <- IO { lookup(resType) } onError { _ => IO { error(s"Failed to find Type $resType in FdnSchema for Resource $resUri ") } }
      _                     <- IO { info (s"Successfully looked up $resType in FdnSchema") }


      tgMessage             <-
        objType match {
          case eType: EntityType    => IO { info (s"Resource $resUri Inferred as Entity" ) } *> translateEntity(resource, resUri, eType, lookup)
          case rType: RelationType  => IO { info (s"Resource $resUri Inferred as Relation" ) } *> translateRelation(resource, resUri, rType, lookup)
        }


      _                     <- IO { ontModel.close() }

      _                     <- IO { info (s"Successfully Translated Message for Resource with Uri: $resUri")}


    } yield tgMessage

  }


  val program = for {

    eUri                             <- IO.pure { "https://data.elsevier.com/lifescience/entity/reaxys/feeding/4037623626" }
    messageFile                      <- IO.pure { "messages/feeding.ttl" }

    fdnSchema                        <- fdnParser.program("elsevier_entellect_proxy_schema_reaxys.ttl")
    lookup                           =  makeLookUpFromFdnSchema(fdnSchema)

    //_                                <- IO {info(fdnSchema.show)}

    tgMessage                        <- translateResourceMessage(eUri, messageFile)(lookup)

    _                                <- IO { info ( "Got TgMessage: " + tgMessage.show ) }


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
