package circe


import io.circe.Json._
import io.circe._
import io.circe.syntax._
import scala.util.chaining.scalaUtilChainingOps
import cats.Show
import cats.syntax.all._


object TgDataTypes {


  /**
   *
   * TYPES ALIASES
   *
   */
  type VertexType       = String
  type EdgeType         = String
  type AttributeType    = String
  type VertexId         = String
  type AttributeValue   = String


  /**
   *
   *  TG DATA TYPES
   *
   */

  case class TgMessage(vertices: List[Vertex], edges: List[Edge])

  sealed trait TgObject
  final case class Vertex(vType: VertexType, id: VertexId, attributes: List[TgAttribute]) extends TgObject
  final case class Edge(eType: EdgeType, sourceVertexId: VertexId, sourceVertexType: VertexType, targetVertexId: VertexId, targetVertexType: VertexType, attributes: List[TgAttribute]) extends TgObject


  sealed trait TgDataType
  case object NUMBER extends TgDataType
  case object STRING extends TgDataType


  sealed abstract class TgAttribute(val aType: AttributeType)
  final case class SingleValuedAttribute(override val aType: AttributeType, value: AttributeValue, dataType: TgDataType) extends TgAttribute(aType)
  final case class MultiValuedAttribute(override val aType: AttributeType, values: List[AttributeValue], dataType: TgDataType) extends TgAttribute(aType)
  final case class UserDefinedAttribute(override val aType: AttributeType, values: List[(AttributeType, TgAttribute)]) extends TgAttribute(aType)


  /**
   *  SHOW TYPE CLASSES
   */

  implicit val showTgMessage: Show[TgMessage] = (tgMsg: TgMessage) => {
    s"""
       |TgMessage: [\n
       |Vertices: ${tgMsg.vertices.map(_.show).mkString("\n","\n","\n")}
       |Edges: ${tgMsg.edges.map(_.show).mkString("\n","\n","\n")}
       |]""".stripMargin
  }

  implicit val showVertex: Show[Vertex] = (vertex: Vertex) => {
    s"""
       |Vertex: [
       |Id: ${vertex.id}
       |VertexType: ${vertex.vType}
       |Attributes: ${vertex.attributes.map(_.toString).mkString("\n ", "\n ", "")}
       |]""".stripMargin
  }

  implicit val showEdge: Show[Edge] = (edge: Edge) => {
    s"""
       |Edge: [
       |EdgeType: ${edge.eType}
       |SourceVertex: [Id: ${edge.sourceVertexId} | Type: ${edge.sourceVertexType}]
       |TargetVertex: [Id: ${edge.targetVertexId} | Type: ${edge.targetVertexType}]
       |Attributes: ${edge.attributes.map(_.toString).mkString("\n", "\n", "")}
       |]""".stripMargin
  }




  /**
   *  CLIENT JSON SERIALIZATION TYPE CLASSES
   */

  case class EdgeGroupKey(sourceVertexType: VertexType, sourceVertexId: VertexId, eType: EdgeType, targetVertexType: VertexType)


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
   * UserDefinedAttribute (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/intro#formatting-data-in-json TG Formatting Advanced DataTypes]])
   *
   * E.g. The Tuple Definition
   *
   * {{{
   *    TYPEDEF TUPLE <name STRING, age INT> Person
   * }}}
   * Translates to
   *
   * {{{
   *    {
   *      "keyList: ["name", "age"],
   *      "valueList": ["Sam", 24]
   *    }
   * }}}
   *
   * __Rule__:
   *
   * `In TG the Order of the Fields of a User Define Type matter at Ingestion time, hence we sort it alphabetically by default to ensure an order.`
   * `The sorting is first done upstream in the schema builder, so we do it downstream when inserting data.`
   *
   */
  implicit val encodeUserDefinedAttribute: Encoder[UserDefinedAttribute] = new Encoder[UserDefinedAttribute] {

    override def apply(a: UserDefinedAttribute): Json = {
      obj(a.aType -> obj("value" -> makeKeysValuesObject (a.values.sortBy(_._1)) ))
    }

    private def makeKeysValuesObject (values: List[(AttributeType, TgAttribute)]): Json =  {
      values
        .unzip
        .pipe { case (types, attributes) => (types map {_.asJson}) -> ( attributes map makeJsonValueFromAttributeValue ) }
        .pipe { case (keys, values) => obj("keylist" -> keys.asJson) -> obj("valuelist" -> values.asJson) }
        .pipe { case (keyListObject, valueListObject) => keyListObject.deepMerge(valueListObject) }
    }

    private def makeJsonValueFromAttributeValue(attribute: TgAttribute): Json = attribute match {
      case SingleValuedAttribute(_, value, dataType) => if (dataType == NUMBER) value.toDouble.asJson else value.asJson
      case MultiValuedAttribute(_, values, dataType) => if (dataType == NUMBER) values.map(_.toDouble).asJson else values.asJson
      case UserDefinedAttribute(_, _)           => "unsupported".asJson
    }
  }

  /**
   *  TgAttribute ( i.e. Attribute ADT)
   */
  implicit val encodeTgAttribute: Encoder[TgAttribute] = new Encoder[TgAttribute] {
    override def apply(a: TgAttribute):Json = a match {
      case s@SingleValuedAttribute(_, _, _) => s.asJson
      case m@MultiValuedAttribute(_, _, _) => m.asJson
      case uda@UserDefinedAttribute(_, _) => uda.asJson
    }
  }

  /**
   * TgAttributeList
   */
  implicit val encodeTgAttributeList: Encoder[List[TgAttribute]] = new Encoder[List[TgAttribute]] {
    override def apply(a: List[TgAttribute]): Json = {
      a.map(_.asJson).foldLeft(obj()){_.deepMerge(_)}
    }
  }




  /**
   * Vertex Standalone (not used in batch)
   */
  implicit val encodeStandAloneVertex: Encoder[Vertex] = new Encoder[Vertex] {
    override def apply(a: Vertex): Json = {
      obj(a.vType -> obj(a.id ->  a.attributes.asJson))
    }
  }

  /**
   * Edge StandAlone (not used in batch)
   */
  implicit val encodeStandAloneEdge: Encoder[Edge] = new Encoder[Edge] {
    override def apply(a: Edge): Json = {
      obj(a.sourceVertexType -> obj(a.sourceVertexId -> obj(a.eType -> obj(a.targetVertexType -> obj(a.targetVertexId -> a.attributes.asJson)))))
    }
  }





  /**
   *
   * Nested Vertices Message (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/built-in-endpoints#request-body-2  TG Json Message Specification]])
   *
   * {{{
   *  "vertices": {
   *     "<vertex_type>": {
   *         "<vertex_id>": {
   *             "<attribute>": {
   *                 "value": <value>,
   *                 "op": <opcode>
   *             }
   *         }
   *      }
   *  }
   * }}}
   *
   * __Rule__:
   *
   * `The nested hierarchy means that vertices are grouped by type`
   *
   */
  implicit val encodeNestedVertices: Encoder[List[Vertex]] = new Encoder[List[Vertex]] {
    override def apply(a: List[Vertex]): Json = {
      a.groupMapReduce
      { vertex => vertex.vType }
      { vertex => obj(vertex.id -> vertex.attributes.asJson) }
      { _.deepMerge(_) } pipe (_.asJson)
    }
  }

  /**
   * Nested Edges Message (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/built-in-endpoints#request-body-2  TG Json Message Specification]])
   *
   * {{{
   *
   *  "edges": {
   *     "<source_vertex_type>": {
   *        "<source_vertex_id>": {
   *           "<edge_type>": {
   *              "<target_vertex_type>": {
   *                 "<target_vertex_id>": {
   *                    "<attribute>": {
   *                       "value": <value>,
   *                       "op": <opcode>
   *                    }
   *                 }
   *              }
   *           }
   *        }
   *     }
   *  }
   * }}}
   *
   * __Rule__:
   *
   * `The nested hierarchy means Edges are first grouped by source vertex type, then vertex ID, then edge type, then target vertex type`
   *
   */
  implicit val encodeNestedEdges: Encoder[List[Edge]] = new Encoder[List[Edge]] {
    override def apply(a: List[Edge]): Json = {
      a.groupMapReduce
      { edge => EdgeGroupKey(edge.sourceVertexType, edge.sourceVertexId, edge.eType, edge.targetVertexType) }
      { edge => obj(edge.targetVertexId -> edge.attributes.asJson ) }
      { _.deepMerge(_) }
        .toList
        .map { case (egk, edgeGroup) => obj(egk.sourceVertexType -> obj(egk.sourceVertexId -> obj(egk.eType -> obj(egk.targetVertexType -> edgeGroup))))}
        .foldLeft(obj())(_.deepMerge(_))
    }
  }


  /**
   * TG Message As Nested Objects (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/built-in-endpoints#request-body-2  TG Json Message Specification]])
   *
   * {{{
   * {
   *
   *  "vertices": {
   *     "<vertex_type>": {
   *         "<vertex_id>": {
   *             "<attribute>": {
   *                 "value": <value>,
   *                 "op": <opcode>
   *             }
   *         }
   *      }
   *  }
   *
   *  "edges": {
   *     "<source_vertex_type>": {
   *        "<source_vertex_id>": {
   *           "<edge_type>": {
   *              "<target_vertex_type>": {
   *                 "<target_vertex_id>": {
   *                    "<attribute>": {
   *                       "value": <value>,
   *                       "op": <opcode>
   *                    }
   *                 }
   *              }
   *           }
   *        }
   *     }
   *  }
   *
   * }
   * }}}
   *
   * @see [[encodeNestedVertices]]
   * @see [[encodeNestedEdges]]
   */
  implicit val encodeTgMessageAsNestedObjects: Encoder[TgMessage] = new Encoder[TgMessage] {
    override def apply(a: TgMessage): Json = {
      obj("vertices" -> a.vertices.asJson, "edges" -> a.edges.asJson)
    }
  }

}
