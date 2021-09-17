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


  sealed trait GraphElmt

  final case class Vertex() extends GraphElmt
  final case class Edge(edgeType: String, connections: List[ConnectionPair]) extends GraphElmt

  sealed trait Connection extends GraphElmt
  final case class From(vertexType: String, withSemanticLinkType: Option[String]) extends Connection
  final case class To(vertexType: String, withSemanticLinkType: Option[String]) extends Connection

  final case class ConnectionPair(connectionA: Connection, connectionB: Connection) extends GraphElmt


  val program = for {

    _                    <- setGlobalDocManagerProperties()

    schemaPair           <- loadSchema("elsevier_entellect_foundation_schema.ttl", "proxyInferenceModel.ttl")

    (schema, schemaWithImports) = schemaPair


    shapes               <- IO { Shapes.parse(schema.getGraph) }

    nodeShapes           <- IO { shapes.iteratorAll().asScala.toList.filter(_.isNodeShape)  }

    bindingShape = nodeShapes.filter(shape => shape.getShapeNode.getURI == "https://data.elsevier.com/lifescience/schema/resnet/PromoterBinding").head


    _                    <- parseEdgeNodeShape(bindingShape.asInstanceOf[NodeShape], schemaWithImports) flatMap { IO.println(_) }




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



  def parseEntityNodeShape(eShape: NodeShape): IO[Vertex] = {
    ???
  }

  def parseEdgeNodeShape(eShape: NodeShape, schemaWithImports: SchemaWithImports):  IO[Edge] = {

    for {

      maybeConnectionsOrConstraint     <- IO  { eShape.getConstraints.asScala.toList.filter(_.isInstanceOf[ShOr]).headOption.asInstanceOf[Option[ShOr]] }

      aNonConnectionPairNodeShapes     <- IO  { maybeConnectionsOrConstraint.map(_.getOthers.asScala.toList.asInstanceOf[List[NodeShape]]).fold{List[NodeShape]()}{ identity }}

      connectionPairs                  <- aNonConnectionPairNodeShapes traverse toConnectionPair(schemaWithImports)

      vType                            <- IO.pure(eShape.getShapeNode.getURI)

    } yield Edge(vType, connectionPairs)

  }

  def toConnectionPair(schemaWithImports: SchemaWithImports) (connectionPairNodeShape: NodeShape): IO[ConnectionPair] = {

    for {

      connectionShapes    <-  IO { connectionPairNodeShape.getPropertyShapes.asScala.toList }

      s0::s1::Nil         = connectionShapes

      connectionA         <- toConnection(s0, schemaWithImports)

      connectionB         <- toConnection(s1, schemaWithImports)

    } yield ConnectionPair(connectionA, connectionB)

  }

  def toConnection(connectionShape: PropertyShape, schemaWithImports: SchemaWithImports): IO[Connection] = {

    for {
      semanticLinkType <- IO { connectionShape.getPath.asInstanceOf[P_Link].getNode.getURI }
      directionalLinkWith    = "https://data.elsevier.com/lifescience/schema/foundation/directionalLinkWith"
      nondirectionalLinkWith = "https://data.elsevier.com/lifescience/schema/foundation/nondirectionalLinkWith"

      linkDirection    <- IO { schemaWithImports.getOntProperty(semanticLinkType).listSuperProperties().asScala.toList.filter(ontProp => ontProp.getURI == directionalLinkWith || ontProp.getURI == nondirectionalLinkWith ).head.getURI}

      vType            <- getConnectionVertexType(connectionShape)

      connection       <- IO.pure( if (linkDirection ==  directionalLinkWith) To(vType, semanticLinkType.some)  else  From(vType, semanticLinkType.some) )

    } yield connection
  }

  def getConnectionVertexType(connectionShape: PropertyShape): IO [String] = {

    for {

      classShape <- IO { connectionShape.getConstraints.asScala.toList.filter(_.isInstanceOf[QualifiedValueShape]).head.asInstanceOf[QualifiedValueShape] } map (_.getSub)

      vType      <- IO { classShape.getConstraints.asScala.toList.head.asInstanceOf[ClassConstraint].getExpectedClass.getURI }

    } yield vType

  }





  def getQVShapesVShapesFromAnonNodeShape(nShape: NodeShape): IO[List[Shape]] = {

    for {

      vShapes <- nShape.getPropertyShapes.asScala.toList traverse getQVShapeVShapeFromPropertyShape

    } yield vShapes
  }

  def getQVShapeVShapeFromPropertyShape(pShape: PropertyShape): IO[Shape] =  {
    for {

      qVShape <- IO { pShape.getConstraints.asScala.toList.filter(_.isInstanceOf[QualifiedValueShape]).head.asInstanceOf[QualifiedValueShape] }

      vShape = qVShape.getSub

    } yield vShape
  }

}
