package circe


import io.circe.Json._
import io.circe._
import io.circe.syntax._
import scala.util.chaining.scalaUtilChainingOps

//import cats.syntax.functor._

object TgDataTypes {


  /**
   *
   *  TG DATA TYPES
   *
   */

  case class TgMessage(vertices: List[Vertex], edges: List[Edge])

  sealed trait TgObject
  final case class Vertex(vType: String, id: String, attributes: List[TgAttribute]) extends TgObject
  final case class Edge(eType:String, sourceVertexId: String, sourceVertexType: String, targetVertexId: String, targetVertexType: String, attributes: List[TgAttribute]) extends TgObject


  sealed trait TgDataType
  case object NUMBER extends TgDataType
  case object STRING extends TgDataType


  sealed trait TgAttribute
  final case class SingleValuedAttribute(aType: String, value: String, dataType: TgDataType) extends TgAttribute
  final case class MultiValuedAttribute(aType: String, values: List[String], dataType: TgDataType) extends TgAttribute


  case class EdgeGroupKey(sourceVertexType: String, sourceVertexId: String, eType:String, targetVertexType: String)





  /**
   * SingleValuedAttribute
   */
  implicit val encodeSingleValuedAttribute: Encoder[SingleValuedAttribute] = new Encoder[SingleValuedAttribute] {
    override def apply(a: SingleValuedAttribute): Json = {
      obj(
        a.aType -> obj("value" -> (if (a.dataType == NUMBER) a.value.toDouble.asJson else a.value.asJson ) )
      )
    }
  }

  /**
   * MultiValuedAttribute
   */
  implicit val encodeMultiValuedAttribute: Encoder[MultiValuedAttribute] = new Encoder[MultiValuedAttribute] {
    override def apply(a: MultiValuedAttribute): Json = {
      obj(
        a.aType -> obj("value" -> (if (a.dataType == NUMBER) a.values.map(_.toDouble).asJson else a.values.asJson ) )
      )
    }
  }

  /**
   * TgAttribute
   */
  implicit val encodeTgAttribute: Encoder[TgAttribute] = new Encoder[TgAttribute] {
    override def apply(a: TgAttribute):Json = a match {
      case s@SingleValuedAttribute(_, _, _) => s.asJson
      case m@MultiValuedAttribute(_, _, _) => m.asJson
    }

  }

  /**
   * TGAttributeList
   */
  implicit val encodeTgAttributeList: Encoder[List[TgAttribute]] = new Encoder[List[TgAttribute]] {
    override def apply(a: List[TgAttribute]): Json = {
      a.map(_.asJson).foldLeft(obj()){_.deepMerge(_)}
    }
  }





  /**
   * Vertex Standalone (not used - need to be shadowed)
   */
  implicit val encodeStandAloneVertex: Encoder[Vertex] = new Encoder[Vertex] {
    override def apply(a: Vertex): Json = {
      a.attributes.asJson pipe { attributes => obj( a.vType -> obj(a.id -> attributes) ) }
    }
  }


  /**
   * Vertex Nested
   */
  implicit val encodeNestedVertex: Encoder[Vertex] = new Encoder[Vertex] {
    override def apply(a: Vertex): Json = {
      a.attributes.asJson pipe { attributes => obj(a.id -> attributes)}
    }
  }




  /**
   * Edge StandAlone (not used - just for debug)
   */

  implicit val encodeStandAloneEdge: Encoder[Edge] = new Encoder[Edge] {
    override def apply(a: Edge): Json = {
      obj(
        a.sourceVertexType -> obj(a.sourceVertexId -> obj(a.eType -> obj(a.targetVertexType -> obj(a.targetVertexId -> a.attributes.asJson))))
      )
    }
  }

  /**
   * TgMessageWithNestedObjects
   */
  implicit val encodeTgMessageWithNestedObjects: Encoder[TgMessage] = new Encoder[TgMessage] {

    override def apply(a: TgMessage): Json = {
      obj(
        "vertices" -> encodeVerticesGrouped(a.vertices),
        "edges" -> encodeEdgesGrouped(a.edges)
      )
    }

    private def encodeVerticesGrouped(vertices: List[Vertex]): Json = {
      vertices.groupMapReduce(_.vType)(_.asJson(encodeNestedVertex))(_.deepMerge(_)) pipe (_.asJson)
    }

    private def encodeEdgesGrouped(edges: List[Edge]): Json = {
      val edgeGroupMap: List[(EdgeGroupKey, Json)] =
        edges.groupMapReduce
        { edge => EdgeGroupKey(edge.sourceVertexType, edge.sourceVertexId, edge.eType, edge.targetVertexType) }
        { edge => obj(edge.targetVertexId -> edge.attributes.asJson ) }
        { _.deepMerge(_) }.toList

      val jsonEdgeGroupList: List[Json] =
        edgeGroupMap map { case (egk, jsonGroup) =>
          obj(egk.sourceVertexType -> obj(egk.sourceVertexId -> obj(egk.eType -> obj(egk.targetVertexType -> jsonGroup))))
        }
      jsonEdgeGroupList.foldLeft(obj())(_.deepMerge(_))
    }

  }

}
