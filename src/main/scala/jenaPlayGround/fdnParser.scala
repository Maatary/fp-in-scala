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


  case class FdnGraphSchema(entityTypes:  List[EntityType],  relationTypes: List[RelationType])

  sealed trait FdnGraphSchemaElt
  final case class EntityType(entityType: String, dataProperties: List[DataProperty], relationProperties: List[RelationProperty], compositionProperties: List[CompositionProperty], schemeProperties: List[SchemeProperty]) extends FdnGraphSchemaElt
  final case class RelationType(relationType: String, linkPropertyPairs: List[LinkPropertyPair], dataProperties: List[DataProperty], associationProperties: List[AssociationProperty]) extends FdnGraphSchemaElt

  sealed trait Property extends FdnGraphSchemaElt
  final case class DataProperty(linkType: String, dataType: String, min: Option[Int], max: Option[Int]) extends Property
  final case class AssociationProperty(linkType: String, entityType: String, min: Option[Int], max: Option[Int]) extends Property
  final case class RelationProperty(linkType: String, entityTypes: List[String], min: Option[Int], max: Option[Int]) extends Property
  final case class CompositionProperty(linkType: String, entityType: String, min: Option[Int], max: Option[Int]) extends Property
  final case class SchemeProperty(linkType: String, dataType: String) extends Property

  sealed trait LinkProperty extends Property
  final case class DirectionalLinkProperty(linkType: String, entityType: String) extends LinkProperty
  final case class NonDirectionalLinkProperty(linkType: String, entityType: String) extends LinkProperty

  final case class LinkPropertyPair(linkPropertyA: NonDirectionalLinkProperty, linkPropertyB: LinkProperty) extends FdnGraphSchemaElt


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

  implicit val showDataProperty: Show[DataProperty] = (dataProperty: DataProperty) => {
    s"""DataProperty: [linkType: ${dataProperty.linkType}, dataType: ${dataProperty.dataType}, minCount: ${dataProperty.min}, maxCount: ${dataProperty.max}]""".stripMargin
  }

  implicit val showSchemeProperty: Show[SchemeProperty] = (schemeProperty: SchemeProperty) => {
    s"""SchemeProperty: [linkType: ${schemeProperty.linkType}, dataType: ${schemeProperty.dataType}]""".stripMargin
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



  val program = for {

    _                    <- setGlobalDocManagerProperties()

    schemaPair           <- loadSchema("elsevier_entellect_foundation_schema.ttl", "elsevier_entellect_external_schema_skos.ttl", "elsevier_entellect_proxy_schema_reaxys.ttl")

    (schema, schemaWithImports) = schemaPair


    _                    <- IO  {info("Reading shapes start")}

    shapes               <- IO { Shapes.parse(schema.getGraph) }

    _                    <- IO  {info("Reading shapes done")}

    nodeShapes           <- IO { shapes.iteratorAll().asScala.toList.collect { case shape: NodeShape => shape }  }

    relationShapes       <- getRelationNodeShapes(nodeShapes, schemaWithImports)

    entityShapes         <- getEntityNodeShapes(nodeShapes, schemaWithImports)


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
   * Read/Parse a NodeShape Representing a Relation to make a RelationType
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
  def parseEntityNodeShape(schemaWithImports: SchemaWithImports)(eShape: NodeShape): IO[EntityType] = {
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
   *
   */
  def makeProperty(schemaWithImports: SchemaWithImports)(propertyShape: PropertyShape): IO[Property] = {

    for {

      directionalLinkWith    <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/directionalLinkWith") }

      nonDirectionalLinkWith <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/nondirectionalLinkWith") }

      composedOf             <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/composedOf") }

      associatedTo           <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/associatedTo") }

      linkType               <- IO { propertyShape.getPath.asInstanceOf[P_Link].getNode.getURI } flatTap { uri => IO { info(s"makeProperty: $uri") } }

      linkTypeObjectProperty <- IO { schemaWithImports.getOntProperty(linkType) }

      property               <- linkTypeObjectProperty match {

                                  case prop if prop.hasSuperProperty(directionalLinkWith, false) || prop.hasSuperProperty(nonDirectionalLinkWith, false) => makeLinkProperty(propertyShape, schemaWithImports)

                                  case prop if prop.hasSuperProperty(associatedTo, false) && isOfKind(propertyShape, SHACL.IRI)                          => makeAssociationProperty(propertyShape) //TODO - to Fix at Ontology Level - isOfKind is a hack Check because resnet:source is both DataTypeProperty and ObjectProperty

                                  case prop if prop.isDatatypeProperty && isOfKind(propertyShape, SHACL.Literal)                                         => makeDataProperty(propertyShape) //TODO - to Fix at Ontology Level - isOfKind is a hack Check because resnet:source is both DataTypeProperty and ObjectProperty

                                  case prop if prop.isAnnotationProperty && prop.hasSuperProperty(RDFS.label, false)                                     => makeDataProperty(propertyShape)

                                  case prop if prop.hasSuperProperty(composedOf, false)                                                                  => makeCompositionProperty(propertyShape)

                                  case prop if prop.equals(SKOS.inScheme)                                                                                => makeSchemeProperty(propertyShape)

                                  case _                                                                                                                 => makeRelationProperty(propertyShape) //Must be the last case, just an object property

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

      linkType         <- IO { associationPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      eType            <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case cc: ClassConstraint => cc }.map(_.getExpectedClass.getURI).head }

      min              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

    } yield AssociationProperty(linkType, eType, min, max)
  }

  def makeCompositionProperty(compositionPropertyShape: PropertyShape):  IO[CompositionProperty] = {

    for {

      linkType         <- IO { compositionPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      eType            <- IO { compositionPropertyShape.getConstraints.asScala.toList.collect { case cc: ClassConstraint => cc }.map(_.getExpectedClass.getURI).head }

      min              <- IO { compositionPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max              <- IO { compositionPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

    } yield CompositionProperty(linkType, eType, min, max)

  }

  /**
   * Take a DataProperty shape and create a DataProperty
   */
  def makeDataProperty(DataPropertyShape: PropertyShape): IO[DataProperty] = {

    for {

      _       <- IO.println(s"makeDataProperty: ${DataPropertyShape.toString}")

      linkType <- IO { DataPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      dataType <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: DatatypeConstraint => dc}.map(_.getDatatypeURI ).head } //TODO handle missing datatype constraint or fix ontology

      min <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: MinCount => dc}.map(_.getMinCount).headOption }

      max <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: MaxCount => dc}.map(_.getMaxCount).headOption }

    } yield DataProperty(linkType, dataType, min, max)

  }

  /**
   * Make an RelationProperty from a PropertyShape describing an RelationProperty (non-reified relation)
   */
  def makeRelationProperty(relationPropertyShape: PropertyShape): IO[RelationProperty] = {

    for {

      _                                 <- IO.println(s"makeRelationProperty:  ${relationPropertyShape.toString}")

      linkType                          <- IO { relationPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      min                               <- IO { relationPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max                               <- IO { relationPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

      maybeEntityTypeOrConstraint       <- IO { relationPropertyShape.getConstraints.asScala.toList.collectFirst { case shOr: ShOr => shOr } }

      maybeANonEntityTypeNodesShapes    <- IO { maybeEntityTypeOrConstraint.map(_.getOthers.asScala.toList.asInstanceOf[List[NodeShape]]) }

      eTypes                            <- maybeANonEntityTypeNodesShapes
                                          .fold {

                                            IO { relationPropertyShape.getConstraints.asScala.toList.collect {case cc: ClassConstraint => cc}.head.getExpectedClass.getURI } map { List(_) }

                                          } { aNonEntityTypeNodeShapes =>

                                            IO { aNonEntityTypeNodeShapes.map(_.getConstraints.asScala.toList.collect {case cc: ClassConstraint => cc}.head.getExpectedClass.getURI) }

                                          }

    } yield RelationProperty(linkType, eTypes, min, max)

  }

  def makeSchemeProperty(relationPropertyShape: PropertyShape): IO[SchemeProperty] = {
    IO.pure{ SchemeProperty(SKOS.inScheme.getURI, XSD.xstring.getURI)  }
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

  def getCompositionProperties(properties: List[Property]): IO[List[CompositionProperty]] = IO {
    properties.collect { case cp: CompositionProperty => cp }
  }

  def getSchemeProperties(properties: List[Property]): IO [List[SchemeProperty]] = IO {
    properties.collect { case sp: SchemeProperty => sp }
  }

  def isOfKind(propertyShape: PropertyShape, kind: Node): Boolean = {

    propertyShape.getConstraints.asScala.toList.collectFirst{ case kind: NodeKindConstraint => kind }
      .fold{
        throw new Throwable(s"isOfKind: Missing Kind in propertyshape ${propertyShape.toString} while trying to compare kind")
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

  def loadSchema(fdnOntology: String, skosOntology: String, schemaOntology: String) : IO[(Schema, SchemaWithImports)] = {
    for {
      fdnModel               <- IO { ModelFactory.createDefaultModel().read(fdnOntology) }
      skosModel              <- IO { ModelFactory.createDefaultModel().read(skosOntology) }

      schemaModel            <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
      _                      <- IO { schemaModel.read(schemaOntology) }

      schemaWithImportsModel <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM_TRANS_INF, schemaModel) }
      _                      <- IO { schemaWithImportsModel.addSubModel(fdnModel) }
      _                      <- IO { schemaWithImportsModel.addSubModel(skosModel) }

    } yield (schemaModel, schemaWithImportsModel)
  }

}




