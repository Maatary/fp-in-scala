package jenaPlayGround



import cats.syntax.all._
import cats.effect.{IO, IOApp}
import cats.effect.unsafe.implicits.global
import circe.TgDataTypes._
import jenaPlayGround.DataTypes._
import jenaPlayGround.SchemaLookupDataBuilder.DataTypes._
import org.apache.jena.ontology.{OntModel, OntResource}
import org.apache.jena.rdf.model._
import org.apache.jena.datatypes.{RDFDatatype, TypeMapper}
import org.apache.jena.util.SplitIRI._
import scribe._

import scala.collection.immutable.SortedMap
import scala.util.chaining.scalaUtilChainingOps
import scala.jdk.CollectionConverters._


object JenaRdfTgMessageTranslation extends IOApp.Simple {

  Logger.root
    .withMinimumLevel(Level.Info).replace()

  Logger(classOf[jenaPlayGround.fdnParser.type].getName)
    .withMinimumLevel(Level.Info)
    .replace()
  Logger(classOf[JenaRdfTgMessageTranslation.type].getName)
    .withMinimumLevel(Level.Info)
    .replace()


  import fs2._
  import fs2.io.file._

  val program = for {

    eUri                             <- IO.pure { "https://data.elsevier.com/lifescience/entity/resnet/directregulation/216172782114755708" }
    message                          <- Files[IO].readAll(Path("src/main/resources/messages/directregulation.ttl")).through(text.utf8.decode).compile.string

    chemblFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_chembl.ttl")
    ppplusFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_ppplus.ttl")
    resnetFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_resnet.ttl")
    reaxysFdnSchema                  <- fdnParser.program("elsevier_entellect_proxy_schema_reaxys.ttl")
    lookupData                       <-  SchemaLookupDataBuilder.makeLookupDataFromFdnSchemas(List(resnetFdnSchema, reaxysFdnSchema, ppplusFdnSchema, chemblFdnSchema))

    // _                                <- IO {info(fdnSchema.show)}

    tgMessage                        <- translateResourceMessage(lookupData)(eUri, message)

    _                                <- IO { info ( "Got TgMessage: " + tgMessage.show ) }



  } yield ()

  def run = program




  def translateResourceMessage(lookupData: SchemaLookupData)(resUri: ResourceUri, resMessage: ResourceMessage): IO[TgMessage] = {

    for {

      _                     <- IO { info (s"Start Translating Message for Resource with Uri: $resUri")}


      ontModel              <- resMessage.asOntModel
      _                     <- if (this.logger.includes(Level.Debug)) ontModel.toPrettyString.flatMap{ m => IO { debug(s"Resource $resUri Message: [\n$m]") } } else IO.unit


      maybeResource         <- IO { Option(ontModel.getOntResource(resUri)) }
      resource              <-
        maybeResource match {

          case None    => ontModel.toPrettyString.flatMap { m => IO { error(s"Resource $resUri does not exists in message:\n$m") } } *> IO.raiseError(new Throwable(s"ontModel.getOntResource($resUri) returned Null"))
          case Some(r) => IO.pure { r }
        }


      resType               <- IO { resource.getRDFType.getURI } onError { _ => ontModel.toPrettyString.flatMap { m => IO { error( s"The Resource $resUri does not have a Type in message:\n$m" ) }} }
      _                     <- IO { info (s"Extracted Type $resType from Message for resource $resUri") }

      individualType        <- IO { lookupData.lookup(resType) } onError { _ => IO { error(s"Failed to find Type $resType in FdnSchema for Resource $resUri ") } }
      _                     <- IO { info (s"Successfully looked up $resType in FdnSchema") }


      tgMessage             <-
        individualType match {
          case eType: EntityType    => IO { info (s"Resource $resUri Inferred as Entity" ) } *> translateEntity(resource, resUri, eType, lookupData)
          case rType: RelationType  => IO { info (s"Resource $resUri Inferred as Relation" ) } *> translateRelation(resource, resUri, rType, lookupData)
        }


      _                     <- IO { ontModel.close() }

      _                     <- IO { info (s"Successfully Translated Message for Resource with Uri: $resUri")}


    } yield tgMessage

  }

  def translateRelation(relation: RelationResource, relationUri: ResourceUri, relationType: RelationType, lookupData: SchemaLookupData): IO[TgMessage] = {
    for {

      _                       <- IO { info (s"Starting Relation Translation Process for Relation $relationUri" ) }

      _                       <- IO { debug(s"Relation $relationUri FdnSchema RelationType Description: [${relationType.show}]") }

      lookup                  <- IO.pure { lookupData.lookup }
      prefixMapping           <- IO.pure { lookupData.prefixMapping }

      dataProperties          <- IO.pure { relationType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(relation) } map { _.flatten }

      associationProperties   <- IO.pure { relationType.associationProperties }
      associationAttributes   <- { associationProperties traverse makeAttributeFromAssociationProperty(relation) } map { _.flatten }

      allAttributes           <- IO.pure { dataAttributes ++ associationAttributes }

      maybeSourceResourceData <- getRelationSourceResourceData(relation, relationType, lookup)
      maybeTargetResourceData <- getRelationTargetResourceData(relation, relationType, lookup)


      maybeLegalPair          <- findSchemaPair(relationUri, relationType, maybeSourceResourceData, maybeTargetResourceData)

      maybeTgMessage          <-

        IO.pure {
          (maybeSourceResourceData, maybeTargetResourceData, maybeLegalPair) mapN { (source, target, _) =>
            TgMessage (
              List(),
              List(Edge(relationType.relationType.asTgType(prefixMapping), source.eUri, source.eType.asTgType(prefixMapping), target.eUri, target.eType.asTgType(prefixMapping), allAttributes))
            )
          }
        }

      tgMessage               <-

        maybeTgMessage
          .fold
          {
            IO.pure { TgMessage(List(), List()) } <* IO { warn(s"Relation Translation Process for Relation $relationUri resulted in an Empty Message because the Relation is Invalid." ) }
          }
          { msg =>
            IO.pure { msg } <* IO { info (s"Relation Translation Process for Relation $relationUri was successful" ) }
          }


    } yield tgMessage
  }

  protected def findSchemaPair(relationUri: ResourceUri, relationType: RelationType, maybeSourceResourceData: Option[EntityResourceData], maybeTargetResourceData: Option[EntityResourceData]): IO[Option[LinkPropertyPair]] = {

    (maybeSourceResourceData, maybeTargetResourceData) match {

      case (Some(EntityResourceData(_, sourceResourceType)) , Some(EntityResourceData(_, targetResourceType))) =>

        relationType.linkPropertyPairs.find{ linkPair => linkPair.linkPropertyA.entityType == sourceResourceType && linkPair.linkPropertyB.entityType == targetResourceType }
        .fold[IO[Option[LinkPropertyPair]]]
        {
            IO { warn(s"The Relation $relationUri of Type ${relationType.relationType} will be skipped because it has an invalid Source -> Target Pair : [$sourceResourceType -> $targetResourceType]") } *> IO.pure { None }
        }
        { linkPair =>
            IO.pure { Option(linkPair) }
        }

      case _                                        =>

        IO { warn(s"The Relation $relationUri of Type ${relationType.relationType} will be skipped because its Mandatory LinkPair is incomplete") } *> IO.pure { None }

    }

  }

  def translateEntity(entity: EntityResource, entityUri: String, entityType: EntityType, lookupData: SchemaLookupData): IO[TgMessage] = {

    for {

      _                       <- IO { info (s"Starting Entity Translation Process for Entity $entityUri" ) }

      _                       <- IO { debug(s"Entity $entityUri FdnSchema EntityType Description: [${entityType.show}]") }

      lookup                  <- IO.pure { lookupData.lookup }
      prefixMapping           <- IO.pure { lookupData.prefixMapping }

      dataProperties          <- IO.pure { entityType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(entity) } map { _.flatten }

      schemeProperties        <- IO.pure { entityType.schemeProperty }
      schemeAttributes        <- { schemeProperties traverse makeAttributeFromSchemeProperty(entity) } map { _.flatten } //To Review now that updated to option

      compositionProperties   <- IO.pure { entityType.compositionProperties}
      compositionAttributes   <- { compositionProperties traverse makeAttributeFromCompositionProperty(entity,  lookup) } map { _.flatten }

      allAttributes           <- IO.pure { dataAttributes ++ schemeAttributes ++ compositionAttributes }

      relationProperties      <- IO.pure {entityType.relationProperties}
      edges                   <- { relationProperties traverse makeEdgesFromRelationProperty(entity, entityType, lookupData) } map {_.flatten}

      tgMessage               <- IO.pure { TgMessage(List(Vertex(entityType.entityType.asTgType(prefixMapping), entityUri, allAttributes)), edges) }

      _                       <- IO { info (s"Entity Translation Process for Entity $entityUri was successful" ) }

    } yield tgMessage

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


  def makeEdgesFromRelationProperty(sourceEntity: EntityResource, entityType: EntityType, lookupData: SchemaLookupData)(relationProperty: RelationProperty): IO[List[Edge]] = {
    for {
      targetEntityUris <- getObjectPropValues(sourceEntity, relationProperty)
      edges            <- targetEntityUris traverse makeEdgeFromRelationPropertyTarget(sourceEntity, entityType, relationProperty, lookupData)
    } yield edges
  }

  def makeEdgeFromRelationPropertyTarget(sourceEntity: EntityResource, sourceEntityType: EntityType, relationProperty: RelationProperty, lookupData: SchemaLookupData)(targetEntityUri: String): IO[Edge] = {
    for {
      lookup   <- IO.pure { lookupData.lookup }
      pm       <- IO.pure { lookupData.prefixMapping }
      eType    <- lookUpEntityType(lookup)(targetEntityUri) onError { _ => IO { error(s"Failed to lookup Entity ResourceType for Target Entity [$targetEntityUri] of RelationProperty [${relationProperty.linkType}] for Entity [${sourceEntity.getURI}]")} }
      edge     = Edge(relationProperty.linkType.asTgType(pm), sourceEntity.getURI, sourceEntityType.entityType.asTgType(pm), targetEntityUri, eType.entityType.asTgType(pm), List())
    } yield edge
  }

  def makeAttributeFromCompositionProperty(sourceEntity: EntityResource, lookup: SortedMap[ResourceType, IndividualType])(compositionProperty: CompositionProperty): IO[Option[UserDefinedAttribute]] = {
    for {
      maybeSubEntityResource    <- getObjectPropResource(sourceEntity, compositionProperty)

      maybeSubEntityType        <- maybeSubEntityResource.map(_.getURI) traverse lookUpEntityType(lookup) // There should be no need to do the lookup, the type is in compositionProperty.entityType

      maybeProps                <- IO { maybeSubEntityResource map {_.listProperties().asScala.toList} collect { case l if l.nonEmpty =>  l } }  //Hack For Reaxys, which can produce empty subEntity. We only create subEntities if they have props.

      maybeUserDefinedAttribute <- (maybeSubEntityResource, maybeSubEntityType, maybeProps) traverseN { (subEntityResource, SubEntityType, _ ) => translateSubEntity(compositionProperty, subEntityResource, SubEntityType) }

    } yield maybeUserDefinedAttribute
  }

  def translateSubEntity(compositionProperty: CompositionProperty, subEntity: EntityResource, subEntityType: EntityType): IO[UserDefinedAttribute] = {
    for {
      dataProperties          <- IO.pure {subEntityType.dataProperties }
      dataAttributes          <- { dataProperties traverse makeAttributeFromDataProperty(subEntity, isForSubEntity = true) } map { _.flatten }
      schemeProperties        <- IO.pure { subEntityType.schemeProperty }
      schemeAttributes        <- { schemeProperties traverse makeAttributeFromSchemeProperty(subEntity, isForSubEntity = true) } map { _.flatten } // To Review now that adjusted to option
      relationProperties      <- IO.pure { subEntityType.relationProperties }
      relationAttributes      <- { relationProperties traverse makeAttributeFromRelationProperty(subEntity)} map { _.flatten }
      allAttributes           <- IO.pure { dataAttributes ++ schemeAttributes ++ relationAttributes }
    } yield UserDefinedAttribute(compositionProperty.linkType, allAttributes.map(attr => attr.aType -> attr))
  }



  def makeAttributeFromAssociationProperty(ontoResource:  OntResource) (associationProperty: AssociationProperty): IO[Option[TgAttribute]] = associationProperty.max match {
    case None                   =>  makeMultiValuedAttributeFromObjectProperty(ontoResource, associationProperty)
    case Some(card) if card > 1 =>  makeMultiValuedAttributeFromObjectProperty(ontoResource, associationProperty)
    case _                      =>  makeSingleValuedAttributeFromObjectProperty(ontoResource, associationProperty)
  }
  //TODO comment subEntity case
  def makeAttributeFromRelationProperty(ontoResource:  OntResource)(relationProperty: RelationProperty): IO[Option[SingleValuedAttribute]] = {
    makeSingleValuedAttributeFromObjectProperty(ontoResource, relationProperty, isForSubEntity = true) // This is for subEntity Only, and cardinality of SubEntity properties are always 1
  }
  //TODO comment subEntity case
  def makeAttributeFromSchemeProperty(ontoResource:  OntResource, isForSubEntity: Boolean = false) (schemeProperty: SchemeProperty): IO[Option[SingleValuedAttribute]] = {
    makeSingleValuedAttributeFromObjectProperty(ontoResource, schemeProperty, isForSubEntity) // Because as of now for Scheme cardinality is always one
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


  def getObjectPropValues(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[List[String]] = {
    IO { ontoResource.listPropertyValues(ResourceFactory.createProperty(objectProperty.linkType)).asScala.toList map { _.asResource().getURI } } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)
  }

  def getObjectPropValue(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[Option[String]] = {
    IO { Option ( ontoResource.getPropertyValue(ResourceFactory.createProperty(objectProperty.linkType)) ) map {_.asResource().getURI}  } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)
  }

  def getObjectPropResource(ontoResource:  OntResource, objectProperty: ObjectProperty): IO[Option[OntResource]] = {
    IO { Option ( ontoResource.getPropertyValue(ResourceFactory.createProperty(objectProperty.linkType)) ) map {_.as(classOf[OntResource])} } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)
  }


  def makeAttributeFromDataProperty(ontoResource: OntResource, isForSubEntity: Boolean = false) (dataProperty: DataProperty): IO[Option[TgAttribute]] = dataProperty match {
    case DataProperty(_,_,_, None)                   => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case DataProperty(_,_,_, Some(card)) if card > 1 => makeMultiValuedAttributeFromDataProperty(ontoResource, dataProperty)
    case _                                           => makeSingleValuedAttributeFromDataProperty(ontoResource, dataProperty, isForSubEntity)
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


  def getDataPropValues(ontoResource:  OntResource, dataProperty: DataProperty, rdfDataType: RDFDatatype): IO[List[String]] = {
    for {

      literals     <- IO { ontoResource.listPropertyValues(ResourceFactory.createProperty(dataProperty.linkType)).asScala.toList map(_.asLiteral()) } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)

      stringValues <- literals traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield stringValues
  }

  def getDataPropValue(ontoResource:  OntResource, dataProperty: DataProperty, rdfDataType: RDFDatatype): IO[Option[String]] = {
    for {

      maybeLiteral        <- IO { Option ( ontoResource.getPropertyValue(ResourceFactory.createProperty(dataProperty.linkType)) ) map {_.asLiteral()}  } // Should be Pure can't fail (would only fail if the data or the ontology are invalid)

      maybeStringValue    <- maybeLiteral traverse { _.asJavaValue(rdfDataType).map(_.toString) }

    } yield maybeStringValue
  }



  def lookUpEntityType(lookup: SortedMap[ResourceType, IndividualType])(eUri: EntityUri): IO[EntityType] = {
    for {
      insensitiveResType  <- inferCaseInsensitiveResourceTypeFromUri(eUri) onError { _ => IO { error(s"Failed to Infer Case Insensitive Resource Type for Entity: $eUri")} }
      entityType          <- IO { lookup(insensitiveResType).asInstanceOf[EntityType] } onError { _ => IO { error(s"Failed to lookup EntityType for Entity [$eUri] from Its Inferred Case Insensitive Resource Type [$insensitiveResType]")} }
    } yield entityType
  }

  def inferCaseInsensitiveResourceTypeFromUri(rUri: String): IO[String] = {
    IO {
      if (rUri.contains("entity"))
        rUri.split("/entity/").last.split("/").pipe { array => s"https://data.elsevier.com/lifescience/schema/${ array(0) }/${ array(1) }" }
      else
        rUri.split("/taxonomy/").last.split("/").pipe { array => s"https://data.elsevier.com/lifescience/schema/${ array(0) }/${ array(1) }" }
    } onError { _ => IO { error(s"Tried to Infer Case Insensitive Type From Non Compliant Resource Uri: $rUri") } }
  }




















}
