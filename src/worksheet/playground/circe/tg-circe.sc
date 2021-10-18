
import cats.syntax.functor._
import io.circe.Json.{JNull, JNumber, JObject, obj}
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._

import scala.util.chaining.scalaUtilChainingOps







sealed trait TgObject
final case class Vertex(vType: String, id: String, attributes: List[SingleValuedAttribute]) extends TgObject
final case class Edge() extends TgObject


sealed trait TgDataType
case object NUMBER extends TgDataType
case object STRING extends TgDataType


sealed trait TgAttribute
final case class SingleValuedAttribute(aType: String, value: String, dataType: TgDataType) extends TgAttribute
final case class MultiValuedAttribute(aType: String, values: List[String], dataType: TgDataType) extends TgAttribute

/**
 * Attributes
 */
implicit val encodeSingleValuedAttribute: Encoder[SingleValuedAttribute] = new Encoder[SingleValuedAttribute] {
  override def apply(a: SingleValuedAttribute): Json = {
    obj(
      a.aType -> obj("value" -> a.value.asJson).asJson
    )
  }
}

implicit val encodeMultiValuedAttribute: Encoder[MultiValuedAttribute] = new Encoder[MultiValuedAttribute] {
  override def apply(a: MultiValuedAttribute): Json = {
    obj(
      a.aType -> obj("value" -> a.values.asJson).asJson
    )
  }
}




/**
 * Vertex
 */
implicit val encodeVertex: Encoder[Vertex] = new Encoder[Vertex] {
  override def apply(a: Vertex): Json = {
    ///val attributes = a.attributes.map(_.asJson).foldLeft(Json.Null){_.deepMerge(_)}
    a.attributes.map(_.asJson).foldLeft(Json.Null){_.deepMerge(_)}
      .pipe {attributes =>
        obj(
          a.vType -> obj(a.id -> attributes)
        )
      }
  }
}

implicit val encodeEmbeddedVertex: Encoder[Vertex] = new Encoder[Vertex] {
  override def apply(a: Vertex): Json = {
    ///val attributes = a.attributes.map(_.asJson).foldLeft(Json.Null){_.deepMerge(_)}
    a.attributes.map(_.asJson).foldLeft(Json.Null){_.deepMerge(_)}
      .pipe {attributes =>
         obj(a.id -> attributes)
      }
  }
}




Vertex(
  "Resnet_Protein",
  "protein1",
  List(
    SingleValuedAttribute("dateCreated", "10-16-2021", STRING),
    SingleValuedAttribute("pubID", "PubId1", STRING)
  )
).asJson(encodeEmbeddedVertex).spaces4




/*SingleValuedAttribute("dateCreated", "10-16-2021", STRING)*/

/*MultiValuedAttribute("medScanId", List("medId2", "MedId4"), STRING).asJson*/

Map[String, Json]().asJson

List[Int]().asJson