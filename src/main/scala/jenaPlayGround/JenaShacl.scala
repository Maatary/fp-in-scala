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
import org.apache.jena.shacl.engine.ShaclPaths
import org.apache.jena.shacl.engine.constraint.{ClassConstraint, QualifiedValueShape, ShOr}
import org.apache.jena.shacl.lib.ShLib
import org.apache.jena.shacl.parser.{NodeShape, PropertyShape, Shape}
import org.apache.jena.sparql.path.P_Link

import scala.jdk.CollectionConverters._



object JenaShacl extends App {


  type Schema            = OntModel
  type SchemaWithImports = OntModel


  case class FdnGraphSchema()

  sealed trait FdnGraphSchemaElt
  final case class EntityType() extends FdnGraphSchemaElt
  final case class RelationType(edgeType: String, linkPropertyPairs: List[LinkPropertyPair]) extends FdnGraphSchemaElt

  sealed trait Property extends FdnGraphSchemaElt
  final case class DataProperty() extends Property
  final case class CompositionProperty() extends Property
  final case class AssociationProperty() extends Property
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


    _                    <- parseEdgeNodeShape(bindingShape.asInstanceOf[NodeShape], schemaWithImports) flatMap { IO.println(_) }


  } yield ()

  program.unsafeRunSync()


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

  def parseEdgeNodeShape(rShape: NodeShape, schemaWithImports: SchemaWithImports):  IO[RelationType] = {

    for {

      linkPropertyPairs                <- getOrConstraintsLinkPropertyPairsFromRelationShape(rShape, schemaWithImports) // TODO should be maybe linkPropertyPairs

      directProperties                 <- rShape.getPropertyShapes.asScala.toList traverse toProperty(schemaWithImports)

      linkProperties                   <- IO { directProperties.collect { case lp: LinkProperty => lp} }

      linkPropertyPairs                <- if (linkPropertyPairs.isEmpty) toLinkPropertyPairFromLinkProperties(linkProperties) map (List(_)) else IO.pure(linkPropertyPairs)

      rType                            <- IO.pure(rShape.getShapeNode.getURI)

    } yield RelationType(rType, linkPropertyPairs)

  }

  def getOrConstraintsLinkPropertyPairsFromRelationShape(rShape: NodeShape, schemaWithImports: SchemaWithImports): IO[List[LinkPropertyPair]]  = {

    for {

      maybeLinkPropertyOrConstraint    <- IO  { rShape.getConstraints.asScala.toList.find(_.isInstanceOf[ShOr]).asInstanceOf[Option[ShOr]] }

      aNonLinkPropertyPairNodeShapes   <- IO  { maybeLinkPropertyOrConstraint.map(_.getOthers.asScala.toList.asInstanceOf[List[NodeShape]]).fold{List[NodeShape]()}{ identity }}

      linkPropertyPairs                <- aNonLinkPropertyPairNodeShapes map {_.getPropertyShapes.asScala.toList} traverse toLinkPropertyPair(schemaWithImports)

    } yield linkPropertyPairs

  }



  def toProperty(schemaWithImports: SchemaWithImports)(propertyShape: PropertyShape): IO[Property] = {

    for {

      directionalLinkWith    <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/directionalLinkWith") }

      nonDirectionalLinkWith <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/nondirectionalLinkWith") }

      composedOf             <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/composedOf") }

      associatedTo           <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/associatedTo") }

      linkType               <- IO { propertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      linkTypeObjectProperty <- IO { schemaWithImports.getOntProperty(linkType) }

      property               <- linkTypeObjectProperty match {

                                  case prop if prop.hasSuperProperty(directionalLinkWith, false) || prop.hasSuperProperty(nonDirectionalLinkWith, false) => makeLinkProperty(propertyShape, schemaWithImports)

                                  case prop if prop.isDatatypeProperty                                                                                   => makeDataProperty()

                                  case prop if prop.hasSuperProperty(composedOf, false)                                                                  => makeCompositionProperty()

                                  case prop if prop.hasSuperProperty(associatedTo, false )                                                               => makeAssociationProperty()

                                  case _                                                                                                                 => makeRelationProperty()

                                 // case _                               => throw new Throwable(s"linkTypeObjectProperty ${linkTypeObjectProperty.toString} not a foundation Ontology Property")

                                }

    } yield  property

  }

  def makeLinkProperty(linkPropertyShape: PropertyShape, schemaWithImports: SchemaWithImports): IO[LinkProperty] = toLinkProperty(linkPropertyShape, schemaWithImports)
  def makeDataProperty(): IO[DataProperty] = IO.pure(DataProperty())
  def makeCompositionProperty(): IO[CompositionProperty] = IO.pure(CompositionProperty())
  def makeAssociationProperty(): IO[AssociationProperty] = IO.pure(AssociationProperty())
  def makeRelationProperty(): IO[RelationProperty] = IO.pure(RelationProperty())


  def getDirectLinkPropertyPairFromRelationShape(rShape: NodeShape, schemaWithImports: SchemaWithImports): IO[LinkPropertyPair] = {

    for {
      linkPropertyShapes   <- IO {rShape.getPropertyShapes.asScala.toList.filter(isLinkPropertyShape(_, schemaWithImports).unsafeRunSync()) } //TODO Change the fuck that !!!!

      linkPropertyPair     <- toLinkPropertyPair(schemaWithImports)(linkPropertyShapes) // TODO if linkPropertyShapes are instead Link Property then the all logic need to change

    } yield linkPropertyPair
  }


  def isLinkPropertyShape(linkPropertyShape: PropertyShape, schemaWithImports: SchemaWithImports): IO[Boolean]  = { //TODO Change the fuck That

    for {

      directionalLinkWith    <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/directionalLinkWith") }

      nonDirectionalLinkWith <- IO { schemaWithImports.getObjectProperty("https://data.elsevier.com/lifescience/schema/foundation/nondirectionalLinkWith") }

      linkType               <- IO { linkPropertyShape.getPath.asInstanceOf[P_Link].getNode.getURI }

      linkTypeObjectProperty <- IO { schemaWithImports.getOntProperty(linkType) }


    } yield linkTypeObjectProperty.hasSuperProperty(directionalLinkWith, false) || linkTypeObjectProperty.hasSuperProperty(nonDirectionalLinkWith, false)

  }




  def toLinkPropertyPairFromLinkProperties(linkProperties: List[LinkProperty]): IO[LinkPropertyPair] = {

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


  def toLinkPropertyPair(schemaWithImports: SchemaWithImports)(linkPropertyShapes:  List[PropertyShape]): IO[LinkPropertyPair] = {

    for {

      linkPropertyShapes  <-  IO.pure { linkPropertyShapes }

      s0::s1::Nil         = linkPropertyShapes

      linkProperty0       <- toLinkProperty(s0, schemaWithImports)

      linkProperty1       <- toLinkProperty(s1, schemaWithImports)

      linkPropertyPair                  <- (linkProperty0, linkProperty1) match {
                                              case (linkProperty0: NonDirectionalLinkProperty, _) => IO.pure { LinkPropertyPair(linkProperty0, linkProperty1) }
                                              case (_, linkProperty1: NonDirectionalLinkProperty) => IO.pure { LinkPropertyPair(linkProperty1, linkProperty0) }
                                              case  _                                             => IO.raiseError(new Throwable(s"Got forbidden bidirectional relation  with ${linkProperty0.toString} and ${linkProperty1.toString}") )
                                            }
    } yield linkPropertyPair

  }


  def toLinkProperty(linkPropertyShape: PropertyShape, schemaWithImports: SchemaWithImports): IO[LinkProperty] = {

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

      maybeClassShape <- IO { linkPropertyShape.getConstraints.asScala.toList.find(_.isInstanceOf[QualifiedValueShape]).asInstanceOf[Option[QualifiedValueShape]]  map (_.getSub) }


      vType           <- maybeClassShape
                        .fold {

                          IO { linkPropertyShape.getConstraints.asScala.toList.filter(_.isInstanceOf[ClassConstraint]).head.asInstanceOf[ClassConstraint].getExpectedClass.getURI; }

                        } { classShape =>

                          IO { classShape.getConstraints.asScala.toList.head.asInstanceOf[ClassConstraint].getExpectedClass.getURI; }

                        }
    } yield vType

  }


}



/*  def getQVShapesVShapesFromAnonNodeShape(nShape: NodeShape): IO[List[Shape]] = {

    for {

      vShapes <- nShape.getPropertyShapes.asScala.toList traverse getQVShapeVShapeFromPropertyShape

    } yield vShapes
  }

  def getQVShapeVShapeFromPropertyShape(pShape: PropertyShape): IO[Shape] =  {
    for {

      qVShape <- IO { pShape.getConstraints.asScala.toList.filter(_.isInstanceOf[QualifiedValueShape]).head.asInstanceOf[QualifiedValueShape] }

      vShape = qVShape.getSub

    } yield vShape
  }*/


/*bindingConstraints <- IO {bindingShape.getConstraints.asScala.toList}

orAnonNodeShapes   <- bindingConstraints traverse { constraint => IO { constraint.asInstanceOf[ShOr].getOthers.asScala.toList.asInstanceOf[List[NodeShape]] } } map (_.flatten)

// _                  <- orAnonNodeShapes traverse  { shape => IO { shape.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model))) } }


 res                <- orAnonNodeShapes traverse  { shape => { IO.pure(shape) -> getQVShapesVShapesFromAnonNodeShape(shape) }.tupled }

 res                <-  res traverse (aNon => IO {
                                                  aNon._1.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model)))
                                                  aNon._2.foreach { _.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model))) }

                                                }
                                    )*/

//_                  <- nodeshapes traverse(shape => IO {shape.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model))) })


//_ <- IO {model.write(System.out, Lang.TTL.getName)}