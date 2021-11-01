package jenaPlayGround



import cats.syntax.all._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.kernel.Monoid
import circe.TgDataTypes
import circe.TgDataTypes._
import jenaPlayGround.DataTypes._
import jenaPlayGround.SchemaLookupDataBuilder.DataTypes._
import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec, OntResource}
import org.apache.jena.rdf.model.{Literal, ModelFactory, ResourceFactory}
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.vocabulary.{RDF, XSD}
import org.apache.jena.datatypes.{RDFDatatype, TypeMapper}
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.datatypes.xsd.impl.{RDFLangString, XSDBaseNumericType, XSDBaseStringType, XSDDateTimeType, XSDDouble, XSDFloat}
import org.apache.jena.shared.PrefixMapping
import org.apache.jena.util.SplitIRI._
import scribe._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import scala.collection.immutable.SortedMap
import scala.util.chaining.scalaUtilChainingOps
import scala.jdk.CollectionConverters._


object JenaRdfTgMessageTranslation extends App {



  Logger.root
    .withMinimumLevel(Level.Info).replace()

  Logger(classOf[jenaPlayGround.fdnParser.type].getName)
    .withMinimumLevel(Level.Info)
    .replace()
  Logger(classOf[JenaRdfTgMessageTranslation.type].getName)
    .withMinimumLevel(Level.Info)
    .replace()



  implicit class EmptyMonoidOps(dataType: TgDataType) {

    def empty: String = dataType match {
      case NUMBER => Monoid[Double].empty.toString
      case STRING => Monoid[String].empty
    }

  }



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

  implicit class TgTypeOps(resourceType: ResourceType) {
    def asTgType(pm: PrefixMapping): ObjectType = {
      pm.shortForm(resourceType).split(':') pipe { array => s"${array(0).capitalize}_${array(1)}" }
    }
  }

  def inferCaseInsensitiveResourceTypeFromUri(rUri: String): IO[String] = {
    IO {
      if (rUri.contains("entity"))
        rUri.split("/entity/").last.split("/").pipe { array => s"https://data.elsevier.com/lifescience/schema/${ array(0) }/${ array(1) }" }
      else
        rUri.split("/taxonomy/").last.split("/").pipe { array => s"https://data.elsevier.com/lifescience/schema/${ array(0) }/${ array(1) }" }
    } onError { _ => IO { error(s"Tried to Infer Case Insensitive Type From Non Compliant Resource Uri: $rUri") } }
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

  def makeSingleValuedAttributeFromDataProperty(ontoResource:  OntResource, dataProperty: DataProperty, isForSubEntity: Boolean = false): IO[Option[SingleValuedAttribute]] = {

    for {

      typeMapper                      <- IO { TypeMapper.getInstance() }
      rdfDataType                     <- IO { typeMapper.getSafeTypeByName(dataProperty.dataType) }

      maybeValue                      <- getDataPropValue(ontoResource, dataProperty, rdfDataType)

      maybeSingleValuedAttribute      <-
        (maybeValue, isForSubEntity) match {
          case (None, false)        => IO.pure[Option[SingleValuedAttribute]] { None }
          case (None, true)         => rdfDataType.asTgDataType flatMap { dt => IO.pure { Option(SingleValuedAttribute(localname(dataProperty.linkType), dt.empty , dt)) } }
          case (Some(value), _)     => rdfDataType.asTgDataType flatMap { dt => IO.pure { Option(SingleValuedAttribute(localname(dataProperty.linkType), value , dt)) } }
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
          case Nil   => IO.pure[Option[MultiValuedAttribute]] { None }
          case  _    => rdfDataType.asTgDataType flatMap { dt => IO.pure { Option(MultiValuedAttribute(localname(dataProperty.linkType), values, dt)) } }
        }

    } yield maybeMultiValuedAttribute

  }

  def makeAttributeFromDataProperty(ontoResource: OntResource, isForSubEntity: Boolean = false) (dataProperty: DataProperty): IO[Option[TgAttribute]] = dataProperty match {
    case DataProperty(_,_,_, None)                   => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case DataProperty(_,_,_, Some(card)) if card > 1 => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case _                                           => makeSingleValuedAttributeFromDataProperty(ontoResource, dataProperty, isForSubEntity)
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


  def makeSingleValuedAttributeFromObjectProperty(ontoResource:  OntResource, objectProperty: ObjectProperty, isForSubEntity: Boolean = false): IO[Option[SingleValuedAttribute]] = {
    for {

      maybeAnyUriValue            <- getObjectPropValue(ontoResource, objectProperty)

      maybeSingleValuedAttribute  <-
        (maybeAnyUriValue, isForSubEntity) match {
          case (None, false)            => IO.pure[Option[SingleValuedAttribute]] { None }
          case (None, true)             => IO.pure { Option ( SingleValuedAttribute(localname(objectProperty.linkType), STRING.empty , STRING) ) }
          case (Some(anyUriValue), _)   => IO.pure { Option ( SingleValuedAttribute(localname(objectProperty.linkType), anyUriValue , STRING) ) }
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

  //TODO comment subEntity case
  def makeAttributeFromSchemeProperty(ontoResource:  OntResource, isForSubEntity: Boolean = false) (schemeProperty: SchemeProperty): IO[Option[SingleValuedAttribute]] = {
    makeSingleValuedAttributeFromObjectProperty(ontoResource, schemeProperty, isForSubEntity) // Because as of now for Scheme cardinality is always one
  }
  //TODO comment subEntity case
  def makeAttributeFromRelationProperty(ontoResource:  OntResource)(relationProperty: RelationProperty): IO[Option[SingleValuedAttribute]] = {
    makeSingleValuedAttributeFromObjectProperty(ontoResource, relationProperty, isForSubEntity = true) // This is for subEntity Only, and cardinality of SubEntity properties are always 1
  }

  def makeAttributeFromAssociationProperty(ontoResource:  OntResource) (associationProperty: AssociationProperty): IO[Option[TgAttribute]] = associationProperty.max match {
    case None                   =>  makeMultiValuedAttributeFromObjectProperty(ontoResource, associationProperty)
    case Some(card) if card > 1 =>  makeMultiValuedAttributeFromObjectProperty(ontoResource, associationProperty)
    case _                      =>  makeSingleValuedAttributeFromObjectProperty(ontoResource, associationProperty)
  }


  def lookUpEntityType(lookup: SortedMap[ResourceType, IndividualType])(eUri: EntityUri): IO[EntityType] = {
    for {
      insensitiveResType  <- inferCaseInsensitiveResourceTypeFromUri(eUri) onError { _ => IO { error(s"Failed to Infer Case Insensitive Resource Type for Entity: $eUri")} }
      entityType          <- IO { lookup(insensitiveResType).asInstanceOf[EntityType] } onError { _ => IO { error(s"Failed to lookup EntityType for Entity [$eUri] from Its Inferred Case Insensitive Resource Type [$insensitiveResType]")} }
    } yield entityType
  }


  def translateSubEntity(compositionProperty: CompositionProperty, subEntity: EntityResource, subEntityType: EntityType): IO[UserDefinedAttribute] = {
    for {
      dataProperties          <- IO.pure {subEntityType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(subEntity, isForSubEntity = true) } map { _.flatten }
      schemeProperties        <- IO.pure { subEntityType.schemeProperties }
      schemeAttributes        <- { schemeProperties traverse makeAttributeFromSchemeProperty(subEntity, isForSubEntity = true) } map { _.flatten }
      relationProperties      <- IO.pure { subEntityType.relationProperties }
      relationAttributes      <- { relationProperties traverse makeAttributeFromRelationProperty(subEntity)} map { _.flatten }
      allAttributes           <- IO.pure { dataAttributes ++ schemeAttributes ++ relationAttributes }
    } yield UserDefinedAttribute(compositionProperty.linkType, allAttributes.map(attr => attr.aType -> attr))
  }

  def makeAttributeFromCompositionProperty(sourceEntity: EntityResource, lookup: SortedMap[ResourceType, IndividualType])(compositionProperty: CompositionProperty): IO[Option[UserDefinedAttribute]] = {
    for {
      maybeSubEntityResource    <- getObjectPropResource(sourceEntity, compositionProperty)

      maybeSubEntityType        <- maybeSubEntityResource.map(_.getURI) traverse lookUpEntityType(lookup) // There should be no need to do the lookup, the type is in compositionProperty.entityType

      maybeProps                <- IO { maybeSubEntityResource map {_.listProperties().asScala.toList} collect { case l if l.nonEmpty =>  l } }  //Hack For Reaxys, which can produce empty subEntity. We only create subEntities if they have props.

      maybeUserDefinedAttribute <- (maybeSubEntityResource, maybeSubEntityType, maybeProps) traverseN { (subEntityResource, SubEntityType, _ ) => translateSubEntity(compositionProperty, subEntityResource, SubEntityType) }

    } yield maybeUserDefinedAttribute
  }

  def makeEdgeFromRelationPropertyTarget(sourceEntity: EntityResource, sourceEntityType: EntityType, relationProperty: RelationProperty, lookup: SortedMap[ResourceType, IndividualType])(targetEntityUri: String): IO[Edge] = {
    lookUpEntityType(lookup)(targetEntityUri)
      .map { eType => Edge(relationProperty.linkType, sourceEntity.getURI, sourceEntityType.entityType, targetEntityUri, eType.entityType, List())}
      .onError {_ => IO { error(s"Failed to lookup Entity ResourceType for Target Entity [$targetEntityUri] of RelationProperty [${relationProperty.linkType}] for Entity [${sourceEntity.getURI}]")} }
  }

  def makeEdgesFromRelationProperty(sourceEntity: EntityResource, entityType: EntityType, lookup: SortedMap[ResourceType, IndividualType])(relationProperty: RelationProperty): IO[List[Edge]] = {
    for {
      targetEntityUris <- getObjectPropValues(sourceEntity, relationProperty)
      edges            <- targetEntityUris traverse makeEdgeFromRelationPropertyTarget(sourceEntity, entityType, relationProperty, lookup)
    } yield edges
  }

  def getRelationTargetResourceData(relation: RelationResource, relationType: RelationType, lookup: SortedMap[ResourceType, IndividualType]): IO[Option[EntityResourceData]] = {
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

  def getRelationSourceResourceData(relation: RelationResource, relationType: RelationType, lookup: SortedMap[ResourceType, IndividualType]): IO[Option[EntityResourceData]] = {

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



  def translateEntity(entity: EntityResource, entityUri: String, entityType: EntityType, lookupData: SchemaLookupData): IO[TgMessage] = {

    for {

      _                       <- IO { info (s"Starting Entity Translation Process for Entity $entityUri" ) }

      _                       <- IO { debug(s"Entity $entityUri FdnSchema EntityType Description: [${entityType.show}]") }

      dataProperties          <- IO.pure { entityType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(entity) } map { _.flatten }

      schemeProperties        <- IO.pure { entityType.schemeProperties }
      schemeAttributes        <- { schemeProperties traverse makeAttributeFromSchemeProperty(entity) } map { _.flatten }

      compositionProperties   <- IO.pure { entityType.compositionProperties}
      compositionAttributes   <- { compositionProperties traverse makeAttributeFromCompositionProperty(entity,  lookupData.lookup) } map { _.flatten }

      allAttributes           <- IO.pure { dataAttributes ++ schemeAttributes ++ compositionAttributes }

      relationProperties      <- IO.pure {entityType.relationProperties}
      edges                   <- { relationProperties traverse makeEdgesFromRelationProperty(entity, entityType, lookupData.lookup) } map {_.flatten}

      tgMessage               <- IO.pure { TgMessage(List(Vertex(entityType.entityType.asTgType(lookupData.prefixMapping), entityUri, allAttributes)), edges) }

      _                       <- IO { info (s"Entity Translation Process for Entity $entityUri was successful" ) }

    } yield tgMessage

  }


  def translateRelation(relation: RelationResource, relationUri: String, relationType: RelationType, lookupData: SchemaLookupData): IO[TgMessage] = {
    for {

      _                       <- IO { info (s"Starting Relation Translation Process for Relation $relationUri" ) }

      _                       <- IO { debug(s"Relation $relationUri FdnSchema RelationType Description: [${relationType.show}]") }

      dataProperties          <- IO.pure { relationType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(relation) } map { _.flatten }

      associationProperties   <- IO.pure { relationType.associationProperties }
      associationAttributes   <- { associationProperties traverse makeAttributeFromAssociationProperty(relation) } map { _.flatten }

      allAttributes           <- IO.pure { dataAttributes ++ associationAttributes }

      maybeSourceResourceData <- getRelationSourceResourceData(relation, relationType, lookupData.lookup)
      maybeTargetResourceData <- getRelationTargetResourceData(relation, relationType, lookupData.lookup)


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


  def translateResourceMessage(lookupData: SchemaLookupData)(resUri: String, messageFile: String): IO[TgMessage] = {

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

      objType               <- IO { lookupData.lookup(resType) } onError { _ => IO { error(s"Failed to find Type $resType in FdnSchema for Resource $resUri ") } }
      _                     <- IO { info (s"Successfully looked up $resType in FdnSchema") }


      tgMessage             <-
        objType match {
          case eType: EntityType    => IO { info (s"Resource $resUri Inferred as Entity" ) } *> translateEntity(resource, resUri, eType, lookupData)
          case rType: RelationType  => IO { info (s"Resource $resUri Inferred as Relation" ) } *> translateRelation(resource, resUri, rType, lookupData)
        }


      _                     <- IO { ontModel.close() }

      _                     <- IO { info (s"Successfully Translated Message for Resource with Uri: $resUri")}


    } yield tgMessage

  }


  val program = for {

    eUri                             <- IO.pure { "https://data.elsevier.com/lifescience/entity/reaxys/bioassay/517534" }
    messageFile                      <- IO.pure { "messages/bioassay.ttl" }

    chemblFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_chembl.ttl")
    ppplusFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_ppplus.ttl")
    resnetFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_resnet.ttl")
    reaxysFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_reaxys.ttl")
    lookupData                       <-  SchemaLookupDataBuilder.makeLookUpFromFdnSchemas(List(resnetFdnSchema, reaxysFdnSchema, ppplusFdnSchema, chemblFdnSchema))

    // _                                <- IO {info(fdnSchema.show)}

    tgMessage                        <- translateResourceMessage(lookupData)(eUri, messageFile)

    _                                <- IO { info ( "Got TgMessage: " + tgMessage.show ) }


  } yield ()

  program.unsafeRunSync()


}
