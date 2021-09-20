package jenaPlayGround

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
import org.apache.jena.shacl.engine.constraint.{ClassConstraint, DatatypeConstraint, MaxCount, MinCount, QualifiedValueShape, ShOr}
import org.apache.jena.shacl.lib.ShLib
import org.apache.jena.shacl.parser.{NodeShape, PropertyShape, Shape}
import org.apache.jena.sparql.path.P_Link

import scala.jdk.CollectionConverters._
import scala.util.chaining.scalaUtilChainingOps



object JenaShacl extends App {


  type Schema            = OntModel
  type SchemaWithImports = OntModel


  case class FdnGraphSchema()

  sealed trait FdnGraphSchemaElt
  final case class EntityType() extends FdnGraphSchemaElt
  final case class RelationType(edgeType: String, linkPropertyPairs: List[LinkPropertyPair], dataProperties: List[DataProperty], associationProperties: List[AssociationProperty]) extends FdnGraphSchemaElt

  sealed trait Property extends FdnGraphSchemaElt
  final case class DataProperty(linkType: String, dataType: String, min: Option[Int], Max: Option[Int]) extends Property
  final case class CompositionProperty() extends Property
  final case class AssociationProperty(linkType: String, dataType: String, min: Option[Int], Max: Option[Int]) extends Property
  final case class RelationProperty() extends Property
  final case class SchemeProperty() extends Property

  sealed trait LinkProperty extends Property
  final case class DirectionalLinkProperty(linkType: String, entityType: String) extends LinkProperty
  final case class NonDirectionalLinkProperty(linkType: String, entityType: String) extends LinkProperty

  final case class LinkPropertyPair(linkPropertyA: NonDirectionalLinkProperty, linkPropertyB: LinkProperty) extends FdnGraphSchemaElt


  val program = for {

    _                    <- setGlobalDocManagerProperties()

    schemaPair           <- loadSchema("elsevier_entellect_foundation_schema.ttl", "proxyInferenceModel.ttl")

    (schema, schemaWithImports) = schemaPair


    shapes               <- IO { Shapes.parse(schema.getGraph) }

    nodeShapes           <- IO { shapes.iteratorAll().asScala.toList.filter(_.isNodeShape)  }

    bindingShape = nodeShapes.filter(shape => shape.getShapeNode.getURI == "https://data.elsevier.com/lifescience/schema/resnet/PromoterBinding").head


    _                    <- parseRelationNodeShape(bindingShape.asInstanceOf[NodeShape], schemaWithImports) flatMap { IO.println(_) }


  } yield ()

  program.unsafeRunSync().show


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



  def parseEntityNodeShape(eShape: NodeShape): IO[EntityType] = {
    ???
  }

  def parseRelationNodeShape(rShape: NodeShape, schemaWithImports: SchemaWithImports):  IO[RelationType] = {

    for {

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

                                  case _                                                                                                                 => makeRelationProperty() //Must be the last case, just an object property

                                 // case _                               => throw new Throwable(s"linkTypeObjectProperty ${linkTypeObjectProperty.toString} not a foundation Ontology Property")

                                }

    } yield  property

  }

  /**
   * Make a LinkProperty from a LinkPropertyShape i.e. a PropertyShape that holds a Link i.e.
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

  def makeAssociationProperty(associationPropertyShape: PropertyShape): IO[AssociationProperty] = {

    for {

      linkType         <- IO { associationPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      vType            <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case cc: ClassConstraint => cc }.map(_.getExpectedClass.getURI).head }

      min              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MinCount => dc }.map(_.getMinCount).headOption }

      max              <- IO { associationPropertyShape.getConstraints.asScala.toList.collect { case dc: MaxCount => dc }.map(_.getMaxCount).headOption }

    } yield AssociationProperty(linkType, vType, min, max)
  }

  def makeRelationProperty(): IO[RelationProperty] = IO.pure(RelationProperty())

  def makeCompositionProperty(): IO[CompositionProperty] = IO.pure(CompositionProperty())

  def makeDataProperty(DataPropertyShape: PropertyShape): IO[DataProperty] = {

    for {

      linkType <- IO { DataPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI tap {println(_)}}

      dataType <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: DatatypeConstraint => dc}.map(_.getDatatypeURI tap {println(_)} ).head } //TODO handle missing datatype constraint or fix ontology

      min <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: MinCount => dc}.map(_.getMinCount).headOption }

      max <- IO { DataPropertyShape.getConstraints.asScala.toList.collect{case dc: MaxCount => dc}.map(_.getMaxCount).headOption }

    } yield DataProperty(linkType, dataType, min, max)

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
   * A LinkPropertyShape can have Its linked EntityType expressed as an sh:class directly or an sh:class in a QualifiedValueShape.
   */
  def getLinkPropertyEntityType(linkPropertyShape: PropertyShape): IO [String] = {

    for {

      maybeClassShape <- IO { linkPropertyShape.getConstraints.asScala.toList.find(_.isInstanceOf[QualifiedValueShape]).asInstanceOf[Option[QualifiedValueShape]]  map (_.getSub) }


      vType           <- maybeClassShape
                        .fold {

                          IO { linkPropertyShape.getConstraints.asScala.toList.filter(_.isInstanceOf[ClassConstraint]).head.asInstanceOf[ClassConstraint].getExpectedClass.getURI; }

                        } { classShape =>

                          IO { classShape.getConstraints.asScala.toList.head.asInstanceOf[ClassConstraint].getExpectedClass.getURI; }

                        }
    } yield vType

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


}




