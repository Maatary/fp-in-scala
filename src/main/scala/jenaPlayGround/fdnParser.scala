package jenaPlayGround

import cats.Show
import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.{Model, ModelFactory, ResourceFactory}
import org.apache.jena.atlas.logging.LogCtl
import org.apache.jena.graph.{Graph, Node}
import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec}
import org.apache.jena.riot.Lang
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.riot.out.NodeFormatterTTL_MultiLine
import org.apache.jena.riot.system.PrefixMapAdapter
import org.apache.jena.shacl.ShaclValidator
import org.apache.jena.shacl.Shapes
import org.apache.jena.shacl.ValidationReport
import org.apache.jena.shacl.engine.{ShaclPaths, constraint}
import org.apache.jena.shacl.engine.constraint.{ClassConstraint, ConstraintOp, DatatypeConstraint, MaxCount, MinCount, NodeKindConstraint, QualifiedValueShape, ShOr}
import org.apache.jena.shacl.lib.ShLib
import org.apache.jena.shacl.parser.{NodeShape, PropertyShape, Shape}
import org.apache.jena.shacl.vocabulary.SHACL
import org.apache.jena.sparql.path.P_Link
import org.apache.jena.vocabulary.{OWL, RDFS, SKOS, XSD}

import scala.jdk.CollectionConverters._
import scala.util.chaining.scalaUtilChainingOps
import scribe._


object DataTypes {

  /**
   *  FoundationGraphSchema DataTypes
   */

  type Schema            = OntModel
  type SchemaWithImports = OntModel
  type OntPrefix         = String
  type OntUri            = String


  case class FdnGraphSchema(ontUri: OntUri, ontPrefix: OntPrefix, entityTypes:  List[EntityType],  relationTypes: List[RelationType])

  sealed trait FdnGraphSchemaElt

  sealed trait ObjectType extends FdnGraphSchemaElt
  final case class EntityType(entityType: String, dataProperties: List[DataProperty], relationProperties: List[RelationProperty], compositionProperties: List[CompositionProperty], schemeProperties: List[SchemeProperty]) extends ObjectType
  final case class RelationType(relationType: String, linkPropertyPairs: List[LinkPropertyPair], dataProperties: List[DataProperty], associationProperties: List[AssociationProperty]) extends ObjectType

  sealed trait PropertyType extends FdnGraphSchemaElt
  final case class DataProperty(linkType: String, dataType: String, min: Option[Int], max: Option[Int]) extends PropertyType

  sealed abstract class ObjectProperty(val linkType: String) extends PropertyType
  final case class AssociationProperty(override val linkType: String, entityType: String, min: Option[Int], max: Option[Int]) extends ObjectProperty(linkType)
  final case class RelationProperty(override val linkType: String, entityTypes: List[String], min: Option[Int], max: Option[Int]) extends ObjectProperty(linkType)
  final case class CompositionProperty(override val linkType: String, entityType: String, min: Option[Int], max: Option[Int]) extends ObjectProperty(linkType)
  final case class SchemeProperty(override val linkType: String, entityType: String) extends ObjectProperty(linkType)

  sealed abstract class LinkProperty(override val linkType: String, val entityType: String) extends ObjectProperty(linkType)
  final case class DirectionalLinkProperty(override val linkType: String, override val entityType: String) extends LinkProperty(linkType, entityType)
  final case class NonDirectionalLinkProperty(override val linkType: String, override val entityType: String) extends LinkProperty(linkType, entityType)


  final case class LinkPropertyPair(linkPropertyA: NonDirectionalLinkProperty, linkPropertyB: LinkProperty) extends FdnGraphSchemaElt



  implicit val showFdnGraphSchema:  Show[FdnGraphSchema] = (fdnGraphSchema: FdnGraphSchema) => {
    s"""
       |FdnGraphSchema: Uri ${fdnGraphSchema.ontUri} Prefix ${fdnGraphSchema.ontPrefix}
       |${fdnGraphSchema.entityTypes.show}
       |${fdnGraphSchema.relationTypes.show}
       |""".stripMargin
  }

  implicit val showEntityType: Show[EntityType] = (eType: EntityType) => {
    s"""
       |EntityType: [ ${eType.entityType}
       |DataProperties: ${eType.dataProperties.map(_.show).mkString("\n", "\n", "")}
       |relationProperties: ${eType.relationProperties.map(_.show).mkString("\n", "\n", "")}
       |compositionProperties: ${eType.compositionProperties.map(_.show).mkString("\n", "\n", "")}
       |schemeProperties: ${eType.schemeProperties.map(_.show).mkString("\n", "\n", "")}
       |]""".stripMargin
  }

  implicit val showRelationType: Show[RelationType] = (rType: RelationType) => {
    s"""
       |RelationType: [ ${rType.relationType}
       |LinkPropertyPairs: ${rType.linkPropertyPairs.map(_.show).mkString("\n", "\n", "")}
       |DataProperties: ${rType.dataProperties.map(_.show).mkString("\n", "\n", "")}
       |AssociationProperties: ${rType.associationProperties.map(_.show).mkString("\n", "\n", "")}
       |]""".stripMargin
  }

  implicit val showObjectType: Show[ObjectType] = {
    case eType: EntityType   => eType.show
    case rType: RelationType => rType.show
  }

  implicit val showDataProperty: Show[DataProperty] = (dataProperty: DataProperty) => {
    s"""DataProperty: [linkType: ${dataProperty.linkType}, dataType: ${dataProperty.dataType}, minCount: ${dataProperty.min}, maxCount: ${dataProperty.max}]""".stripMargin
  }

  implicit val showSchemeProperty: Show[SchemeProperty] = (schemeProperty: SchemeProperty) => {
    s"""SchemeProperty: [linkType: ${schemeProperty.linkType}, dataType: ${schemeProperty.entityType}]""".stripMargin
  }

  implicit val showRelationProperty: Show[RelationProperty] = (relProperty: RelationProperty) => {
    s"""RelationProperty: [linkType: ${relProperty.linkType}, entityTypes: [${relProperty.entityTypes.mkString(" | ")}], minCount: ${relProperty.min}, maxCount: ${relProperty.max}]""".stripMargin
  }

  implicit val showCompositionProperty: Show[CompositionProperty] = (compProperty: CompositionProperty) => {
    s"""CompositionProperty: [linkType: ${compProperty.linkType}, entityType: ${compProperty.entityType}, minCount: ${compProperty.min}, maxCount: ${compProperty.max}]""".stripMargin
  }

  implicit val showAssociationProperty: Show[AssociationProperty] = (assocProperty: AssociationProperty) => {
    s"""AssociationProperty: [linkType: ${assocProperty.linkType}, entityType: ${assocProperty.entityType}, minCount: ${assocProperty.min}, maxCount: ${assocProperty.max}]""".stripMargin
  }

  implicit val showDirectionalLinkProperty: Show[DirectionalLinkProperty] = (dProperty: DirectionalLinkProperty) => {
    s"""DirectionalLinkProperty: [linkType: ${dProperty.linkType}, entityType: ${dProperty.entityType}""".stripMargin
  }

  implicit val showNonDirectionalLinkProperty: Show[NonDirectionalLinkProperty] = (ndProperty: NonDirectionalLinkProperty) => {
    s"""NonDirectionalLinkProperty: [linkType: ${ndProperty.linkType}, entityType: ${ndProperty.entityType}""".stripMargin
  }

  implicit val showLinkProperty: Show[LinkProperty] = {
    case dProperty@DirectionalLinkProperty(_, _)     => dProperty.show
    case ndProperty@NonDirectionalLinkProperty(_, _) => ndProperty.show
  }

  implicit val showLinkPropertyPair: Show[LinkPropertyPair] = (lPair: LinkPropertyPair) => {
    s"""LinkPropertyPair: [linkPropertyA: ${lPair.linkPropertyA.show} ,  linkPropertyB: ${lPair.linkPropertyB.show}]"""
  }


}


object fdnParser extends App {


import DataTypes._

  Logger(classOf[jenaPlayGround.fdnParser.type].getName).withMinimumLevel(Level.Info).replace()

  def program(proxyFile: String = "elsevier_entellect_proxy_schema_resnet.ttl") = for {

    _                    <- setGlobalDocManagerProperties()

    schemaPair           <- loadSchema("elsevier_entellect_upper_schema_foundation.ttl", "elsevier_entellect_external_schema_skos.ttl", "elsevier_entellect_external_schema_skosxl.ttl", proxyFile)

    (schema, schemaWithImports) = schemaPair

    prefAndUri           <- getSchemaPrefixAndUri(schema)
    (ontPrefix, ontUri) = prefAndUri

    _                    <- IO { info(s"Reading all shapes for Ontology $ontPrefix")  }

    shapes               <- IO { Shapes.parse(schema.getGraph) }

    _                    <- IO { info(s"Reading all shapes for Ontology $ontPrefix successful")  }

    nodeShapes           <- IO { shapes.iteratorAll().asScala.toList.collect { case shape: NodeShape => shape }  }

    relationShapes       <- getRelationNodeShapes(nodeShapes, schemaWithImports)


    entityShapes         <- getEntityNodeShapes(nodeShapes, schemaWithImports)

    _                    <- IO { info(s"Extracting all EntityTypes") }

    eTypes               <- entityShapes traverse makeEntityType(schemaWithImports)

    _                    <- IO { info(s"Successfully Extracted all ${eTypes.size} EntityTypes") }

    _                    <- IO { info(s"Extracting all RelationTypes") }

    relTypes             <- relationShapes traverse makeRelationType(schemaWithImports)

    _                    <- IO { info(s"Successfully Extracted all ${relTypes.size} RelationTypes") }




  } yield FdnGraphSchema(ontUri, ontPrefix,  eTypes, relTypes)

  println(program().unsafeRunSync().show)



  /**
   * Get All the NonEmpty NodeShape that Describe a Relation
   */
  def getRelationNodeShapes(nodeShapes: List[NodeShape], schemaWithImports: SchemaWithImports): IO[List[NodeShape]] = {

    for {

      relation           <- IO { schemaWithImports.getOntClass("https://data.elsevier.com/lifescience/schema/foundation/Relation") }

      relationShapes     <- IO {  nodeShapes.filter { ns => schemaWithImports.getOntClass(ns.getShapeNode.getURI).hasSuperClass(relation)} }

      nonEmptyRelShapes  = relationShapes.filterNot(_.getPropertyShapes.asScala.toList.isEmpty)

    } yield nonEmptyRelShapes

  }


  /**
   * Get All the NonEmpty NodeShape that Describe a RelationType
   */
  def getEntityNodeShapes(nodeShapes: List[NodeShape], schemaWithImports: SchemaWithImports): IO[List[NodeShape]] = {

    for {

      entity           <- IO { schemaWithImports.getOntClass("https://data.elsevier.com/lifescience/schema/foundation/Entity") }

      entityShapes     <- IO { nodeShapes.filter { ns => schemaWithImports.getOntClass(ns.getShapeNode.getURI).hasSuperClass(entity)} }

      nonEmptyEShapes  = entityShapes.filterNot(_.getPropertyShapes.asScala.toList.isEmpty)

    } yield nonEmptyEShapes

  }


  /**
   * Read/Parse a NodeShape Representing a Relation to make a RelationType
   */
  def makeRelationType(schemaWithImports: SchemaWithImports)(rShape: NodeShape):  IO[RelationType] = {

    for {

      //_                                <- IO {println(rShape)}

      directProperties                 <- rShape.getPropertyShapes.asScala.toList traverse makeProperty(schemaWithImports)

      linkPropertyPairs                <- getOrConstraintsLinkPropertyPairsFromRelationShape(rShape, schemaWithImports)

      directLinkProperties             <- getLinkProperties(directProperties)

      linkPropertyPairs                <- if (linkPropertyPairs.isEmpty) toLinkPropertyPair(directLinkProperties) map (List(_)) else IO.pure(linkPropertyPairs)

      dataProperties                   <- getDataProperties(directProperties)

      associationProperties            <- getAssociationProperties(directProperties)

      rType                            <- IO.pure(rShape.getShapeNode.getURI)

    } yield RelationType(rType, linkPropertyPairs, dataProperties, associationProperties)

  }


  /**
   * Read/Parse a NodeShape Representing a Entity to make an EntityType
   *
   *  === temp hack 1  ===
   * We filter Relation Properties that point to an OWL:Thing as it represents a faulty modeling.
   * That case is present is Reaxys  for  `SynampticaID  hasSynapticaConcept`.
   *
   * {{{
   *   filterNot(_.entityTypes == List(OWL.Thing.getURI))
   * }}}
   * Generally speaking SynapticaID is badly is faulty and should be ignored at load time.
   * Its `hasConcept` properties can point to any sub-class of concept.
   * This does not respect the proxy standard.
   * Moreover, in TigerGraph we need to know what pair of Vertice Type an  Edge Type Connect.
   *
   * === temp hack 2  ===
   *  We do distinct on Relation Properties because of Reaxys which has modelling issues where
   *  for a properties that comes from different fields, it duplicates the properties constraints.
   *
   *  {{{
   *    map(_.distinct)
   *  }}}
   *
   * TODO  - Remove those hacks by fixing Ontologies (i.e.reaxys)
   */
  def makeEntityType(schemaWithImports: SchemaWithImports)(eShape: NodeShape): IO[EntityType] = {
    for {

      //_                                <- IO {println(rShape)}

      directProperties       <- eShape.getPropertyShapes.asScala.toList traverse makeProperty(schemaWithImports)

      dataProperties         <- getDataProperties(directProperties)

      compositionProperties  <- getCompositionProperties(directProperties)

      relationProperties     <- getRelationProperties(directProperties).map(_.filterNot(_.entityTypes == List(OWL.Thing.getURI))).map(_.distinct)

      schemeProperties       <- getSchemeProperties(directProperties)

      eType                  <- IO.pure(eShape.getShapeNode.getURI)

    } yield EntityType (eType, dataProperties, relationProperties, compositionProperties, schemeProperties)
  }

  /**
   * Build the List of LinkPropertyPair from a Relation NodeShape OrConstraint.
   */
  def getOrConstraintsLinkPropertyPairsFromRelationShape(rShape: NodeShape, schemaWithImports: SchemaWithImports): IO[List[LinkPropertyPair]]  = {

    for {

      maybeLinkPropertyOrConstraint    <- IO  { rShape.getConstraints.asScala.toList.find(_.isInstanceOf[ShOr]).asInstanceOf[Option[ShOr]] }

      aNonLinkPropertyPairNodeShapes   <- IO  { maybeLinkPropertyOrConstraint.map(_.getOthers.asScala.toList.asInstanceOf[List[NodeShape]]).fold{List[NodeShape]()}{ identity }}

      aNonShapesDirectProperties       <- aNonLinkPropertyPairNodeShapes traverse ( _.getPropertyShapes.asScala.toList traverse makeProperty (schemaWithImports) )

      aNonShapesLinkProperties         <- IO { aNonShapesDirectProperties map { _.collect { case lp: LinkProperty => lp} } }

      linkPropertyPairs                <-  aNonShapesLinkProperties traverse toLinkPropertyPair

    } yield linkPropertyPairs

  }


  /**
   *  Take a PropertyShape and convert to its corresponding Property i.e.
   *  LinkProperty, DataProperty, AssociationProperty, RelationProperty, SchemeProperty
   *
   *  TODO - BUG in Resnet Ontology, source is both a DataProp and an ObjectProp, because there is a field in Edge Table sProperty_Values source.
   *  TODO - Fix Ontology & Remove hack check - isOfKind
   *  TODO - Fix in code - AnnotationProperty are assumed to range on data Values which is not always true, but currently what we have
   *
   */
  def makeProperty(schemaWithImports: SchemaWithImports)(propertyShape: PropertyShape): IO[PropertyType] = {

    for {

      _                      <- IO { debug(s"Making Property from : ${propertyShape.toString}") }

      directionalLinkWith    <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/directionalLinkWith") }

      nonDirectionalLinkWith <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/nondirectionalLinkWith") }

      composedOf             <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/composedOf") }

      associatedTo           <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/associatedTo") }

      linkType               <- IO { propertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }.onError(t => IO {error(missingErrorMsg("path", propertyShape, t.getMessage)) })

      linkTypeObjectProperty <- IO { schemaWithImports.getOntProperty(linkType) }

      property               <- linkTypeObjectProperty match {

                                  case prop if prop.hasSuperProperty(directionalLinkWith, false) || prop.hasSuperProperty(nonDirectionalLinkWith, false) => makeLinkProperty(propertyShape, schemaWithImports)

                                  case prop if prop.hasSuperProperty(associatedTo, false) && isOfKind(propertyShape, SHACL.IRI)                          => makeAssociationProperty(propertyShape) //TODO - to Fix at Ontology Level - isOfKind is a hack Check because resnet:source is both DataTypeProperty and ObjectProperty

                                  case prop if prop.isDatatypeProperty && isOfKind(propertyShape, SHACL.Literal)                                         => makeDataProperty(propertyShape) //TODO - to Fix at Ontology Level - isOfKind is a hack Check because resnet:source is both DataTypeProperty and ObjectProperty

                                  case prop if prop.isAnnotationProperty /*&& prop.hasSuperProperty(RDFS.label, false) */                                => makeDataProperty(propertyShape) //TODO - to Fix at ontology level - Order matter cause we have wrongfully some dataProperty subPropertyOf Annotation Property e.g. Chembl hasPreferredName

                                  case prop if prop.hasSuperProperty(composedOf, false)                                                                  => makeCompositionProperty(propertyShape)

                                  case prop if prop.equals(SKOS.inScheme)                                                                                => makeSchemeProperty(propertyShape)

                                  case _                                                                                                                 => makeRelationProperty(propertyShape,schemaWithImports) //Must be the last case, just an object property

                                }

    } yield  property

  }

  /**
   * Take a list of LinkProperty of expected Size 2 (not checked yet) and convert it into a LinkPropertyPair where:
   *
   *  --  LinkPropertyA is always NonDirectional
   *
   *  --  LinkPropertyB can either be Directional or NonDirectional
   */
  def toLinkPropertyPair(linkProperties: List[LinkProperty]): IO[LinkPropertyPair] = {

    for {

      linkProperty0       <- IO { linkProperties(0) }

      linkProperty1       <- IO { linkProperties(1) }

      linkPropertyPair    <- (linkProperty0, linkProperty1) match {
        case (linkProperty0: NonDirectionalLinkProperty, _) => IO.pure { LinkPropertyPair(linkProperty0, linkProperty1) }
        case (_, linkProperty1: NonDirectionalLinkProperty) => IO.pure { LinkPropertyPair(linkProperty1, linkProperty0) }
        case  _                                             => IO.raiseError(new Throwable(s"Got forbidden bidirectional relation  with ${linkProperty0.toString} and ${linkProperty1.toString}") )
      }

    } yield linkPropertyPair

  }

  /**
   * Make a LinkProperty from a PropertyShape describing a Link i.e.
   *
   * DirectionalLinkProperty, NonDirectionalLinkProperty
   *
   */
  def makeLinkProperty(linkPropertyShape: PropertyShape, schemaWithImports: SchemaWithImports): IO[LinkProperty] = {

    for {
      linkType         <- IO { linkPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      directionalLinkWith    = "https://data.elsevier.com/lifescience/schema/foundation/directionalLinkWith"
      nondirectionalLinkWith = "https://data.elsevier.com/lifescience/schema/foundation/nondirectionalLinkWith"

      linkDirection    <- IO { schemaWithImports.getOntProperty(linkType).listSuperProperties().asScala.toList.filter(ontProp => ontProp.getURI == directionalLinkWith || ontProp.getURI == nondirectionalLinkWith ).head.getURI}

      vType            <- getLinkPropertyEntityType(linkPropertyShape)

      linkProperty     <- IO.pure { if (linkDirection ==  directionalLinkWith) DirectionalLinkProperty(linkType, vType)  else  NonDirectionalLinkProperty(linkType, vType) }

    } yield linkProperty
  }

  /**
   * A LinkPropertyShape can have Its linked EntityType expressed as an sh:class directly or an sh:class in a QualifiedValueShape.
   */
  def getLinkPropertyEntityType(linkPropertyShape: PropertyShape): IO [String] = {

    for {

      maybeQVSClassShape  <- IO { linkPropertyShape.getConstraints.asScala.toList.find(_.isInstanceOf[QualifiedValueShape]).asInstanceOf[Option[QualifiedValueShape]]  map (_.getSub) }


      eType               <- maybeQVSClassShape
                            .fold {

                              IO { linkPropertyShape.getConstraints.asScala.toList.filter(_.isInstanceOf[ClassConstraint]).head.asInstanceOf[ClassConstraint].getExpectedClass.getURI; }

                            } { classShape =>

                              IO { classShape.getConstraints.asScala.toList.head.asInstanceOf[ClassConstraint].getExpectedClass.getURI; }

                            }
    } yield eType

  }

  /**
   *  Make an AssociationProperty from a PropertyShape describing an Association
   */
  def makeAssociationProperty(associationPropertyShape: PropertyShape): IO[AssociationProperty] = {

    for {

      _                <- IO { debug(s"Making AssociationProperty from : ${associationPropertyShape.toString}") }

      linkType         <- IO { associationPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }.onError(t => IO { error(missingErrorMsg("path", associationPropertyShape, t.getMessage)) })

      eType            <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case cc: ClassConstraint => cc }.map(_.getExpectedClass.getURI).head }

      min              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

    } yield AssociationProperty(linkType, eType, min, max)
  }

  def makeCompositionProperty(compositionPropertyShape: PropertyShape):  IO[CompositionProperty] = {

    for {

      _        <- IO { debug(s"Making CompositionProperty from : ${compositionPropertyShape.toString}") }

      linkType         <- IO { compositionPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }.onError(t => IO { error(missingErrorMsg("path", compositionPropertyShape, t.getMessage)) })

      eType            <- IO { compositionPropertyShape.getConstraints.asScala.toList.collect { case cc: ClassConstraint => cc }.map(_.getExpectedClass.getURI).head }

      min              <- IO { compositionPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max              <- IO { compositionPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

    } yield CompositionProperty(linkType, eType, min, max)

  }

  /**
   * Take a DataProperty shape and create a DataProperty
   */
  def makeDataProperty(dataPropertyShape: PropertyShape): IO[DataProperty] = {

    for {

      _        <- IO { debug(s"Making DataProperty from : ${dataPropertyShape.toString}") }

      linkType <- IO { dataPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }.onError(t => IO { error(missingErrorMsg("path", dataPropertyShape, t.getMessage)) })

      dataType <- IO { dataPropertyShape.getConstraints.asScala.toList.collect{case dc: DatatypeConstraint => dc}.map(_.getDatatypeURI ).head }.onError { t => IO { error(missingErrorMsg("datatype", dataPropertyShape, t.getMessage)) } }

      min      <- IO { dataPropertyShape.getConstraints.asScala.toList.collect{case dc: MinCount => dc}.map(_.getMinCount).headOption }

      max      <- IO { dataPropertyShape.getConstraints.asScala.toList.collect{case dc: MaxCount => dc}.map(_.getMaxCount).headOption }

    } yield DataProperty(linkType, dataType, min, max)

  }
  /**
   * Make an RelationProperty from a PropertyShape describing an RelationProperty (non-reified relation)
   *
   * '''Note that a RelationProperty can not connect to a Reified Relation''' i.e. a Relation as per the Foundation Ontology.
   * So we filter that out.
   *
   * It is a use case not currently supported, however it is  present in resnet where for e.g. a PathWay may contain edges.
   *
   * Investigation is necessary to decide how to handle this use case if at all, as this part of the  model of resnet is not formal
   * and therefore not loaded in TigerGraph
   *
   * '''The Only thing that connect Relation are LinkProperty'''
   */
  def makeRelationProperty(relationPropertyShape: PropertyShape, schemaWithImports: SchemaWithImports): IO[RelationProperty] = {

    for {

      _                                 <- IO { debug(s"Making RelationProperty from : ${relationPropertyShape.toString}") }

      linkType                          <- IO { relationPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }.onError(t => IO { error(missingErrorMsg("path", relationPropertyShape, t.getMessage)) })

      min                               <- IO { relationPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max                               <- IO { relationPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

      maybeEntityTypeOrConstraint       <- IO { relationPropertyShape.getConstraints.asScala.toList.collectFirst { case shOr: ShOr => shOr } }

      maybeANonEntityTypeNodesShapes    <- IO { maybeEntityTypeOrConstraint.map(_.getOthers.asScala.toList.asInstanceOf[List[NodeShape]]) }

      allETypes                            <- maybeANonEntityTypeNodesShapes
        .fold {

          IO { relationPropertyShape.getConstraints.asScala.toList.collect {case cc: ClassConstraint => cc}.head.getExpectedClass.getURI } map { List(_) }

        } { aNonEntityTypeNodeShapes =>

          IO { aNonEntityTypeNodeShapes.map(_.getConstraints.asScala.toList.collect {case cc: ClassConstraint => cc}.head.getExpectedClass.getURI) }

        }

      eTypes                          <- IO.pure { allETypes.filterNot(schemaWithImports.getOntClass(_).hasSuperClass(schemaWithImports.getOntClass("https://data.elsevier.com/lifescience/schema/foundation/Relation"))) }

    } yield RelationProperty(linkType, eTypes, min, max)

  }

  def makeSchemeProperty(relationPropertyShape: PropertyShape): IO[SchemeProperty] = {
    IO.pure{ SchemeProperty(SKOS.inScheme.getURI, SKOS.ConceptScheme.getURI)  }
  }

  def getDataProperties(properties: List[PropertyType]): IO[List[DataProperty]] = IO {
    properties.collect { case dp: DataProperty => dp }
  }

  def getLinkProperties(properties: List[PropertyType]): IO[List[LinkProperty]] = IO {
    properties.collect { case lp: LinkProperty => lp }
  }

  def getAssociationProperties(properties: List[PropertyType]): IO[List[AssociationProperty]] = IO {
    properties.collect { case ap: AssociationProperty => ap }
  }

  def getRelationProperties(properties: List[PropertyType]): IO[List[RelationProperty]] = IO {
    properties.collect { case rp: RelationProperty => rp }
  }

  def getCompositionProperties(properties: List[PropertyType]): IO[List[CompositionProperty]] = IO {
    properties.collect { case cp: CompositionProperty => cp }
  }

  def getSchemeProperties(properties: List[PropertyType]): IO [List[SchemeProperty]] = IO {
    properties.collect { case sp: SchemeProperty => sp }
  }

  def isOfKind(propertyShape: PropertyShape, kind: Node): Boolean = {

    propertyShape.getConstraints.asScala.toList.collectFirst{ case kind: NodeKindConstraint => kind }
      .fold{
        throw new Throwable(s"isOfKind: Missing Kind in PropertyShape ${propertyShape.toString} while trying to compare kind")
      } { kindConstraint =>
        kindConstraint.getKind.equals(kind)
      }

  }

  def setGlobalDocManagerProperties(): IO[Unit] = {
    for {
      ontDoc       <- IO { OntDocumentManager.getInstance() } // Set your global Ontology Manager without any LocationMapper, so the reliance on the StreamMndgr is ensured.
      _            <- IO { ontDoc.setProcessImports(false) }
    } yield ()
  }

  def loadSchema(fdnOntology: String, skosOntology: String, skosXlOntology: String, schemaOntology: String) : IO[(Schema, SchemaWithImports)] = {
    for {
      fdnModel               <- IO { ModelFactory.createDefaultModel().read(fdnOntology) }
      skosModel              <- IO { ModelFactory.createDefaultModel().read(skosOntology) }
      skosXlModel            <- IO { ModelFactory.createDefaultModel().read(skosXlOntology) }

      schemaModel            <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
      _                      <- IO { schemaModel.read(schemaOntology) }

      schemaWithImportsModel <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM_TRANS_INF, schemaModel) }
      _                      <- IO { schemaWithImportsModel.addSubModel(fdnModel) }
      _                      <- IO { schemaWithImportsModel.addSubModel(skosModel) }
      _                      <- IO { schemaWithImportsModel.addSubModel(skosXlModel) }

    } yield (schemaModel, schemaWithImportsModel)
  }

  def getSchemaPrefixAndUri(schemaModel: Schema): IO[(OntPrefix, OntUri)] = {

    for {

      ontology  <- IO { schemaModel.listOntologies().asScala.toList.head }

      ontUri    <- IO.pure { ontology.getURI }

      ontPrefix <- IO { schemaModel.getNsURIPrefix(s"$ontUri/") }

      _         <- IO { info(s"Got Ontology: [$ontUri],  with Prefix [$ontPrefix]" ) }

    } yield (ontPrefix, ontUri)
  }

  def missingErrorMsg(missing: String, shape: Shape, cause: String) = {
    s"Missing $missing in shape: [${shape.toString}], for cause: [$cause]"
  }

}




