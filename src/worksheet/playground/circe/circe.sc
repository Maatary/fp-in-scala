import cats.syntax.functor._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._
import cats.syntax.all._


/*Json.obj(
  "name"  ->  "Maat".asJson,
  "last"  ->  List(1,2,3,4).asJson
)

Json.obj().spaces2*/

import io.circe.generic.semiauto._
import  io.circe.parser._

case class Result(accepted_vertices: Int, accepted_edges: Int)
case class Version(edition: String, api: String, schema: Int)
case class TgResponse(version: Version, error: Boolean, message: String,  results: List[Result], code: String)

implicit val tgResponseDecoder: Decoder[TgResponse] = deriveDecoder[TgResponse]
implicit val tgResponseEncoder: Encoder[TgResponse] = deriveEncoder[TgResponse]

val tgResponse =
  """
    |{
    |   "version" : {
    |     "edition" : "enterprise",
    |     "api" : "v2",
    |     "schema" : 3
    |   },
    |   "error" : false,
    |   "message" : "",
    |   "results" : [
    |     {
    |       "accepted_vertices" : 0,
    |       "accepted_edges" : 1
    |     }
    |   ],
    |   "code" : "REST-0002"
    |}
    |""".stripMargin



parse(tgResponse).getOrElse(Json.Null).as[TgResponse]