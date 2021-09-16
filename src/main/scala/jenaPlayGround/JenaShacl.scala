package jenaPlayGround

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.atlas.logging.LogCtl
import org.apache.jena.graph.Graph
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

import scala.jdk.CollectionConverters._

object JenaShacl extends App {


  sealed trait GraphElmt

  final case class Vertex() extends GraphElmt
  final case class Edge(edgeType: String, connections: List[ConnectionPair]) extends GraphElmt

  sealed trait Connection extends GraphElmt
  final case class From(vertexType: String, withSemanticLinkType: Option[String]) extends Connection
  final case class To(vertexType: String, withSemanticLinkType: Option[String]) extends Connection

  final case class ConnectionPair(connectionA: Connection, connectionB: Connection) extends GraphElmt


  val program = for {

    model       <- IO { ModelFactory.createDefaultModel().read("proxyInferenceModel.ttl") }

    shapes      <- IO { Shapes.parse( model.getGraph) }

    nodeshapes  <- IO { shapes.iteratorAll().asScala.toList.filter(_.isNodeShape)  }

    bindingShape = nodeshapes.filter(shape => shape.getShapeNode.getURI == "https://data.elsevier.com/lifescience/schema/resnet/PromoterBinding").head


    _           <- parseEdgeNodeShape(bindingShape.asInstanceOf[NodeShape]) flatMap { IO.println(_) }




    bindingConstraints <- IO {bindingShape.getConstraints.asScala.toList}

    orAnonNodeShapes   <- bindingConstraints traverse { constraint => IO { constraint.asInstanceOf[ShOr].getOthers.asScala.toList.asInstanceOf[List[NodeShape]] } } map (_.flatten)

   // _                  <- orAnonNodeShapes traverse  { shape => IO { shape.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model))) } }


     res                <- orAnonNodeShapes traverse  { shape => { IO.pure(shape) -> getQVShapesVShapesFromAnonNodeShape(shape) }.tupled }

     res                <-  res traverse (aNon => IO {
                                                      aNon._1.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model)))
                                                      aNon._2.foreach { _.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model))) }

                                                    }
                                        )

    //_                  <- nodeshapes traverse(shape => IO {shape.print(System.out, new NodeFormatterTTL_MultiLine(null, new PrefixMapAdapter(model))) })


    //_ <- IO {model.write(System.out, Lang.TTL.getName)}


  } yield ()

  program.unsafeRunSync()




  def parseEntityNodeShape(eShape: NodeShape): IO[Vertex] = {
    ???
  }

  def parseEdgeNodeShape(eShape: NodeShape):  IO[Edge] = {

    for {

      maybeConnectionsOrConstraint     <- IO  { eShape.getConstraints.asScala.toList.filter(_.isInstanceOf[ShOr]).headOption.asInstanceOf[Option[ShOr]] }

      aNonConnectionPairNodeShapes     <- IO  { maybeConnectionsOrConstraint.map(_.getOthers.asScala.toList.asInstanceOf[List[NodeShape]]).fold{List[NodeShape]()}{ identity }}

      connectionPairs                  <- aNonConnectionPairNodeShapes traverse toConnectionPair

      vType                            <- IO.pure(eShape.getShapeNode.getURI)

    } yield Edge(vType, connectionPairs)

  }

  def toConnectionPair(connectionPairNodeShape: NodeShape): IO[ConnectionPair] = {

    for {

      connectionShapes    <-  IO { connectionPairNodeShape.getPropertyShapes.asScala.toList }

      s0::s1::Nil         = connectionShapes

      connectionA         <- toConnection(s0)

      connectionB         <- toConnection(s1)

    } yield ConnectionPair(connectionA, connectionB)

  }

  def toConnection(connectionShape: PropertyShape): IO[Connection] = {

    for {
      semanticLinkType <- IO.pure  { ShaclPaths.pathToString(connectionShape.getPath) }
      vType            <- getConnectionVertexType(connectionShape)
    } yield From(vType, semanticLinkType.some)
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
