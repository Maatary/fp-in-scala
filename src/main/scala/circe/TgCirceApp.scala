package circe

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.Lang
import org.http4s._
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.client.dsl.io._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.Client
import cats.syntax.all._
import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.jena.ontology.OntDocumentManager
import scribe._
import io.circe.syntax._
import io.circe.parser._
import TgDataTypes._
import fs2._
import _root_.io.circe.Json

/**
 * Revisit issue with empty value
 */
object TgCirceApp extends App {




  def makeTgRequest(msg: TgMessage):  Request[IO] = {
    POST(msg.asJson.spaces2, uri"http://localhost:9000/graph/Entellect?ack=all&new_vertex_only=false&vertex_must_exist=false").withContentType(`Content-Type`(MediaType.application.json, DefaultCharset))
  }


  val v1 =
    Vertex(
      "Resnet_Protein",
      "protein_1",
      List(
        SingleValuedAttribute("dateCreated", "10-16-2021", STRING),
        SingleValuedAttribute("dateModified", "10-05-2022", STRING),
        SingleValuedAttribute("idOwner", "0", NUMBER),
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
        SingleValuedAttribute("idOwner", "10", NUMBER),
        MultiValuedAttribute("keggId", List("keggId6", "keggId7"), STRING)
      )
    )


  val edgeAttributes1: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-18-2021", STRING), SingleValuedAttribute("effect", "positive", STRING), SingleValuedAttribute("mechanism", "direct interaction", STRING))
  val edgeAttributes2: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-16-2021", STRING), SingleValuedAttribute("effect", "negative", STRING), SingleValuedAttribute("mechanism", "cleavage", STRING))

  val e1: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000019","Resnet_ClinicalParameter", edgeAttributes1)
  val e2: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000018","Resnet_ClinicalParameter", edgeAttributes2)


  /*SingleValuedAttribute("dateCreated", "10-16-2021", STRING).asJson
  MultiValuedAttribute("medScanId", List("medId2", "MedId4"), STRING).asJson

  println(v1.asJson(encodeNestedVertex).spaces2)
  println(v2.asJson(encodeNestedVertex).spaces2)
  println(e1.asJson.spaces2)

  val tgMsg1 = TgMessage(List(v1, v2), List(e1,e2))

  println(tgMsg1.asJson.spaces2)*/





  /*val postMsgLogic = for {

    tgMsg          <- IO.pure(TgMessage(List(v1, v2), List(e1,e2)))

    clientResource <- IO { BlazeClientBuilder[IO].resource }

    tgResp         <-  clientResource.use { client => client.expect[String](makeTgRequest(tgMsg)) }

    _              <-  IO.println(tgResp)

  } yield ()

  postMsgLogic.unsafeRunSync()*/


  val postStreamMsgLogic = for {

    client   <-  Stream.resource(BlazeClientBuilder[IO].resource)

    _        <-
      Stream
      .emits(List(TgMessage(List(v1, v2), List(e1,e2))))
      .evalTap { msg  => IO { info(msg.asJson.spaces2) } }
      .evalMap { msg  => client.expect[String](makeTgRequest(msg)) }
      .evalMap { resp => IO { info(parse(resp).getOrElse(Json.Null).spaces2) } }

  } yield ()

  postStreamMsgLogic.compile.drain.unsafeRunSync()
}
