package jenaPlayGround

import cats.Show
import cats.effect.{IO, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.{Model, ModelFactory, ResourceFactory}
import org.apache.jena.atlas.logging.LogCtl
import org.apache.jena.graph.Graph
import org.apache.jena.ontology.{OntDocumentManager, OntModel, OntModelSpec}
import org.apache.jena.riot.Lang
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.riot.out.NodeFormatterTTL_MultiLine
import org.apache.jena.riot.system.PrefixMapAdapter
import org.apache.jena.shacl.ShaclValidator
import org.apache.jena.shacl.Shapes
import org.apache.jena.shacl.ValidationReport
import org.apache.jena.shacl.engine.{ShaclPaths, constraint}
import org.apache.jena.shacl.engine.constraint.{ClassConstraint, ConstraintOp, DatatypeConstraint, MaxCount, MinCount, QualifiedValueShape, ShOr}
import org.apache.jena.shacl.lib.ShLib
import org.apache.jena.shacl.parser.{NodeShape, PropertyShape, Shape}
import org.apache.jena.sparql.path.P_Link

import scala.jdk.CollectionConverters._
import scala.util.chaining.scalaUtilChainingOps




object DataTypes {

  /**
   *  FoundationGraphSchema DataTypes
   */

  type Schema            = OntModel
  type SchemaWithImports = OntModel


  case class FdnGraphSchema(entityTypes:  List[EntityType],  relationTypes: List[RelationType])

  sealed trait FdnGraphSchemaElt
  final case class EntityType(entityType: String, dataProperties: List[DataProperty], relationProperties: List[RelationProperty]) extends FdnGraphSchemaElt
  final case class RelationType(relationType: String, linkPropertyPairs: List[LinkPropertyPair], dataProperties: List[DataProperty], associationProperties: List[AssociationProperty]) extends FdnGraphSchemaElt

  sealed trait Property extends FdnGraphSchemaElt
  final case class DataProperty(linkType: String, dataType: String, min: Option[Int], max: Option[Int]) extends Property
  final case class AssociationProperty(linkType: String, entityType: String, min: Option[Int], max: Option[Int]) extends Property
  final case class RelationProperty(linkType: String, entityTypes: List[String], min: Option[Int], max: Option[Int]) extends Property
  final case class CompositionProperty() extends Property
  final case class SchemeProperty() extends Property

  sealed trait LinkProperty extends Property
  final case class DirectionalLinkProperty(linkType: String, entityType: String) extends LinkProperty
  final case class NonDirectionalLinkProperty(linkType: String, entityType: String) extends LinkProperty


  final case class LinkPropertyPair(linkPropertyA: NonDirectionalLinkProperty, linkPropertyB: LinkProperty) extends FdnGraphSchemaElt

  implicit val showEntityType: Show[EntityType] = (eType: EntityType) => {
    s"""
       |EntityType: [ ${eType.entityType}
       |DataProperties: ${eType.dataProperties.map(_.show).mkString("\n", "\n", "")}
       |relationProperties: ${eType.relationProperties.map(_.show).mkString("\n", "\n", "")}
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

  implicit val showDataProperty: Show[DataProperty] = (dataProperty: DataProperty) => {
    s"""DataProperty: [linkType: ${dataProperty.linkType}, dataType: ${dataProperty.dataType}, minCount: ${dataProperty.min}, maxCount: ${dataProperty.max}]""".stripMargin
  }

  implicit val showRelationProperty: Show[RelationProperty] = (relProperty: RelationProperty) => {
    s"""RelationProperty: [linkType: ${relProperty.linkType}, entityTypes: [${relProperty.entityTypes.mkString(" | ")}], minCount: ${relProperty.min}, maxCount: ${relProperty.max}]""".stripMargin
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



  val program = for {

    _                    <- setGlobalDocManagerProperties()

    schemaPair           <- loadSchema("elsevier_entellect_foundation_schema.ttl", "proxyInferenceModel.ttl")

    (schema, schemaWithImports) = schemaPair


    shapes               <- IO { Shapes.parse(schema.getGraph) }

    nodeShapes           <- IO { shapes.iteratorAll().asScala.toList.collect { case shape: NodeShape => shape }  }

    relationShapes       <- getRelationNodeShapes(nodeShapes, schemaWithImports)

    entityShapes         <- getEntityNodeShapes(nodeShapes, schemaWithImports)


    //Need Filtering because those shape have error, they miss their LinkPair
    filteredRelShapes       = relationShapes.filterNot(shape => shape.getShapeNode.getURI == "https://data.elsevier.com/lifescience/schema/resnet/ClinicalTrial")
                                         .filterNot(shape => shape.getShapeNode.getURI == "https://data.elsevier.com/lifescience/schema/resnet/GeneticChange")
                                         .filterNot(shape => shape.getShapeNode.getURI == "https://data.elsevier.com/lifescience/schema/resnet/ProtModification")

    relTypes             <- relationShapes traverse parseRelationNodeShape(schemaWithImports)

    eTypes               <- entityShapes traverse parseEntityNodeShape(schemaWithImports)


  } yield (relTypes, eTypes)

  println(program.unsafeRunSync().show)



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
   * Read/Parse a NodeShape Representing a Relation make a Relation
   */
  def parseRelationNodeShape(schemaWithImports: SchemaWithImports)(rShape: NodeShape):  IO[RelationType] = {

    for {

      //_                                <- IO {println(rShape)}

      directProperties                 <- rShape.getPropertyShapes.asScala.toList traverse makeProperty(schemaWithImports)

      linkPropertyPairs                <- getOrConstraintsLinkPropertyPairsFromRelationShape(rShape, schemaWithImports) //To Change

      directLinkProperties             <- getLinkProperties(directProperties) //To Change

      linkPropertyPairs                <- if (linkPropertyPairs.isEmpty) toLinkPropertyPair(directLinkProperties) map (List(_)) else IO.pure(linkPropertyPairs)

      dataProperties                   <- getDataProperties(directProperties)

      associationProperties            <- getAssociationProperties(directProperties)

      rType                            <- IO.pure(rShape.getShapeNode.getURI)

    } yield RelationType(rType, linkPropertyPairs, dataProperties, associationProperties)

  }


  /**
   * Read/Parse a NodeShape Representing a Entity make an EntityType
   */
  def parseEntityNodeShape(schemaWithImports: SchemaWithImports)(eShape: NodeShape): IO[EntityType] = {
    for {

      //_                                <- IO {println(rShape)}

      directProperties      <- eShape.getPropertyShapes.asScala.toList traverse makeProperty(schemaWithImports)

      dataProperties        <- getDataProperties(directProperties)

      relationProperties    <- getRelationProperties(directProperties)

      eType                 <- IO.pure(eShape.getShapeNode.getURI)

    } yield EntityType (eType, dataProperties, relationProperties)
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
   *  TODO - BUG in Resnet Ontology, source is generated as both a DataProp and an ObjectProp, because there is a field in Edge Table sProperty_Values source.
   *  TODO - Cheating here by catching source as Association before it makes it to DataProperty
   *
   */
  def makeProperty(schemaWithImports: SchemaWithImports)(propertyShape: PropertyShape): IO[Property] = {

    for {

      directionalLinkWith    <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/directionalLinkWith") }

      nonDirectionalLinkWith <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/nondirectionalLinkWith") }

      composedOf             <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/composedOf") }

      associatedTo           <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/associatedTo") }

      linkType               <- IO { propertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      linkTypeObjectProperty <- IO { schemaWithImports.getOntProperty(linkType) }

      property               <- linkTypeObjectProperty match {

                                  case prop if prop.hasSuperProperty(directionalLinkWith, false) || prop.hasSuperProperty(nonDirectionalLinkWith, false) => makeLinkProperty(propertyShape, schemaWithImports)

                                  case prop if prop.hasSuperProperty(associatedTo, false)                                                                => makeAssociationProperty(propertyShape)

                                  case prop if prop.isDatatypeProperty                                                                                   => makeDataProperty(propertyShape)

                                  case prop if prop.hasSuperProperty(composedOf, false)                                                                  => makeCompositionProperty()

                                  case _                                                                                                                 => makeRelationProperty(propertyShape) //Must be the last case, just an object property

                                 // case _                               => throw new Throwable(s"linkTypeObjectProperty ${linkTypeObjectProperty.toString} not a foundation Ontology Property")

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
   *
   *  TODO - Need Fix - There is a structural bug in the Ontology, source is both dataProperty and ObjectProperty
   *  TODO - When Parsing Entity, we capture an association Property (because of order in makeProperty) that does not corresponds to the property shape in the Entity where it is a dataPropertyShape.
   */
  def makeAssociationProperty(associationPropertyShape: PropertyShape): IO[AssociationProperty] = {

    (for {

      linkType         <- IO { associationPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      vType            <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case cc: ClassConstraint => cc }.map(_.getExpectedClass.getURI).head }

      min              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

    } yield AssociationProperty(linkType, vType, min, max)).handleError(_ => AssociationProperty("Silent Error", "Silent Error", None, None ))
  }

  /**
   * Make an RelationProperty from a PropertyShape describing an RelationProperty (non-reified relation)
   */
  def makeRelationProperty(relationPropertyShape: PropertyShape): IO[RelationProperty] = {

    for {

      linkType                          <- IO { relationPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      min                               <- IO { relationPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max                               <- IO { relationPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

      maybeEntityTypeOrConstraint       <- IO { relationPropertyShape.getConstraints.asScala.toList.collectFirst { case shOr: ShOr => shOr } }

      maybeANonEntityTypePropertyShapes <- IO { maybeEntityTypeOrConstraint.map(_.getOthers.asScala.toList.asInstanceOf[List[NodeShape]]) }

      eTypes                            <- maybeANonEntityTypePropertyShapes
                                          .fold {

                                            IO { relationPropertyShape.getConstraints.asScala.toList.collect {case cc: ClassConstraint => cc}.head.getExpectedClass.getURI } map { List(_) }

                                          } { aNonEntityTypePropertyShapes =>

                                            IO { aNonEntityTypePropertyShapes.map(_.getConstraints.asScala.toList.collect {case cc: ClassConstraint => cc}.head.getExpectedClass.getURI) }

                                          }

    } yield RelationProperty(linkType, eTypes, min, max)

  }

  def makeCompositionProperty(): IO[CompositionProperty] = IO.pure(CompositionProperty())


  /**
   * Take a DataProperty shape and create a DataProerty
   */
  def makeDataProperty(DataPropertyShape: PropertyShape): IO[DataProperty] = {

    for {

      linkType <- IO { DataPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      dataType <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: DatatypeConstraint => dc}.map(_.getDatatypeURI ).head } //TODO handle missing datatype constraint or fix ontology

      min <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: MinCount => dc}.map(_.getMinCount).headOption }

      max <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: MaxCount => dc}.map(_.getMaxCount).headOption }

    } yield DataProperty(linkType, dataType, min, max)

  }


  def getDataProperties(properties: List[Property]): IO[List[DataProperty]] = IO {
    properties.collect { case dp: DataProperty => dp }
  }

  def getLinkProperties(properties: List[Property]): IO[List[LinkProperty]] = IO {
    properties.collect { case lp: LinkProperty => lp }
  }

  def getAssociationProperties(properties: List[Property]): IO[List[AssociationProperty]] = IO {
    properties.collect { case ap: AssociationProperty => ap }
  }

  def getRelationProperties(properties: List[Property]): IO[List[RelationProperty]] = IO {
    properties.collect { case rp: RelationProperty => rp }
  }


  def setGlobalDocManagerProperties(): IO[Unit] = {
    for {
      ontDoc       <- IO { OntDocumentManager.getInstance() } // Set your global Ontology Manager without any LocationMapper, so the reliance on the StreamMndgr is ensured.
      _            <- IO { ontDoc.setProcessImports(false) }
    } yield ()
  }

  def loadSchema(fdnOntology: String, schemaOntology: String) : IO[(Schema, SchemaWithImports)] = {
    for {
      fdnModel               <- IO { ModelFactory.createDefaultModel().read(fdnOntology) }

      schemaModel            <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
      _                      <- IO { schemaModel.read(schemaOntology) }

      schemaWithImportsModel <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM_TRANS_INF, schemaModel) }
      _                      <- IO { schemaWithImportsModel.addSubModel(fdnModel) }

    } yield (schemaModel, schemaWithImportsModel)
  }

}




