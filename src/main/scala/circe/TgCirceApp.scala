package circe


/**
 * Revisit issue with empty value
 */
object TgCirceApp extends App {

  import cats.syntax.functor._
  import io.circe.Json._
  import io.circe._
  import io.circe.syntax._
  import io.circe.generic.auto._


  import scala.util.chaining.scalaUtilChainingOps

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

  /**
   * SingleValuedAttribute
   */
  implicit val encodeSingleValuedAttribute: Encoder[SingleValuedAttribute] = new Encoder[SingleValuedAttribute] {
    override def apply(a: SingleValuedAttribute): Json = {
      obj(
        a.aType -> obj("value" -> a.value.asJson)
      )
    }
  }

  /**
   * MultiValuedAttribute
   */
  implicit val encodeMultiValuedAttribute: Encoder[MultiValuedAttribute] = new Encoder[MultiValuedAttribute] {
    override def apply(a: MultiValuedAttribute): Json = {
      obj(
        a.aType -> obj("value" -> a.values.asJson)
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
   * Vertex Standalone
   */
  implicit val encodeStandAloneVertex: Encoder[Vertex] = new Encoder[Vertex] {
    override def apply(a: Vertex): Json = {
      a.attributes.map(_.asJson).foldLeft(obj()){_.deepMerge(_)} pipe { attributes => obj( a.vType -> obj(a.id -> attributes) ) }
    }
  }

  /**
   * Vertex Nested
   */
  implicit val encodeNestedVertex: Encoder[Vertex] = new Encoder[Vertex] {
    override def apply(a: Vertex): Json = {
      a.attributes.map(_.asJson).foldLeft(obj()){_.deepMerge(_)} pipe { attributes => obj(a.id -> attributes)}
    }
  }


  /**
   *
   */

  implicit val encodeStandAloneEdge: Encoder[Edge] = new Encoder[Edge] {
    override def apply(a: Edge): Json = {
      obj(
        a.sourceVertexType -> obj(a.sourceVertexId -> obj(a.eType -> obj(a.targetVertexType -> obj(a.targetVertexId -> a.attributes.asJson))))
      )
    }
  }


  case class EdgeGroupKey(sourceVertexType: String, sourceVertexId: String, eType:String, targetVertexType: String)


  /**
   * TgMessageWithStandAloneObjects
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
      val edgeGroupsMap: List[(EdgeGroupKey, Json)] =
        edges.groupMapReduce
        { edge => EdgeGroupKey(edge.sourceVertexType, edge.sourceVertexId, edge.eType, edge.targetVertexType) }
        { edge => obj(edge.targetVertexId -> edge.attributes.map(_.asJson).foldLeft(obj()){_.deepMerge(_)} ) }
        { _.deepMerge(_) }.toList

      val jsonEdgeGroups: List[Json] = edgeGroupsMap map { case (egk, jsonGroup) =>
        obj(
          egk.sourceVertexType -> obj(egk.sourceVertexId -> obj(egk.eType -> obj(egk.targetVertexType -> jsonGroup)))
        )
      }
      jsonEdgeGroups.foldLeft(obj())(_.deepMerge(_))
    }

  }






  /*SingleValuedAttribute("dateCreated", "10-16-2021", STRING)*/

  /*MultiValuedAttribute("medScanId", List("medId2", "MedId4"), STRING).asJson*/


  val v1 =
    Vertex(
      "Resnet_Protein",
      "protein_1",
      List(
        SingleValuedAttribute("dateCreated", "10-16-2021", STRING),
        SingleValuedAttribute("dateModified", "10-05-2022", STRING),
        MultiValuedAttribute("medScanId", List("medId2", "MedId4"), STRING)
      )
    )

  val v2 =
    Vertex(
      "Resnet_Protein",
      "protein_2",
      List(
        SingleValuedAttribute("dateCreated", "10-18-2021", STRING),
        SingleValuedAttribute("dateModified", "10-05-2022", STRING),
        MultiValuedAttribute("keggId", List("keggId6", "keggId7"), STRING)
      )
    )



  /*println(v1.asJson(encodeNestedVertex).spaces2)
  println(v2.asJson(encodeNestedVertex).spaces2)*/



  val edgeAttributes1: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-18-2021", STRING), SingleValuedAttribute("effect", "positive", STRING), SingleValuedAttribute("mechanism", "direct interaction", STRING))
  val edgeAttributes2: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-16-2021", STRING), SingleValuedAttribute("effect", "negative", STRING), SingleValuedAttribute("mechanism", "cleavage", STRING))

  val e1: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000019","Resnet_ClinicalParameter", edgeAttributes1)
  val e2: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000018","Resnet_ClinicalParameter", edgeAttributes2)

  //println(e1.asJson.spaces2)

  val tgMsg1 = TgMessage(List(v1, v2), List(e1,e2))

  println(tgMsg1.asJson.spaces2)

}
