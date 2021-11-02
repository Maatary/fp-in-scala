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
    POST(
      msg.asJson.spaces2,
      uri"https://tigergraph-api.sdc-oxygen-dev.nonprod.entellect.com/graph/Entellect?ack=all&new_vertex_only=false&vertex_must_exist=false",
      Authorization(Credentials.Token(AuthScheme.Bearer, "02hriecshllksn0bgtf1nse2918k97m2")))
      .withContentType(`Content-Type`(MediaType.application.json, DefaultCharset))
  }


  val v0 =
    Vertex(
      "Reaxys_Metabolizer",
      "Metabolizer_1",
      List(
        SingleValuedAttribute("hasMetabolizerNumber", "1829", STRING),
        UserDefinedAttribute(
          "hasMetabolizerTotalNumber",
          List
            (
              SingleValuedAttribute("hasValueUnit", "2000", STRING),
              SingleValuedAttribute("hasValue", "2000", STRING),
              SingleValuedAttribute("hasDisplayValue", "2000yards", STRING),
              SingleValuedAttribute("hasPlausibility", "2000", STRING),
              SingleValuedAttribute("hasStatisticalInformation", "2000", STRING),
              SingleValuedAttribute("hasStandardValue", "2000", STRING),
              SingleValuedAttribute("hasValuePrecision", "2000yards", STRING)
              //Reaxys_MetabolizerTotalNumber (hasDisplayValue STRING (1024), hasPlausibility STRING (1024), hasStandardValue STRING (1024), hasStatisticalInformation STRING (1024), hasValue STRING (1024), hasValuePrecision STRING (1024), hasValueUnit STRING (1024))
              //Reaxys_MetabolizerTotalNumber (hasValueUnit STRING (1024), hasValue STRING (1024), hasDisplayValue STRING (1024), hasPlausibility STRING (1024), hasStatisticalInformation STRING (1024), hasStandardValue STRING (1024), hasValuePrecision STRING (1024))
            )
            .map(attr => attr.aType -> attr)
        )
      )
    )

  val v1 =
    Vertex(
      "Resnet_Protein",
      "protein_656",
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
      "protein_909",
      List(
        SingleValuedAttribute("dateCreated", "10-18-2021", STRING),
        SingleValuedAttribute("dateModified", "10-05-2022", STRING),
        SingleValuedAttribute("idOwner", "10", NUMBER),
        MultiValuedAttribute("keggId", List("keggId6", "keggId7"), STRING)
      )
    )


  val edgeAttributes1: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-18-2021", STRING), SingleValuedAttribute("effect", "positive", STRING), SingleValuedAttribute("mechanism", "direct interaction", STRING))
  val edgeAttributes2: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "10-16-2021", STRING), SingleValuedAttribute("effect", "negative", STRING), SingleValuedAttribute("mechanism", "cleavage", STRING))
  val edgeAttributes3: List[TgAttribute] = List(SingleValuedAttribute("dateCreated", "01-04-2020", STRING), SingleValuedAttribute("effect", "unknown", STRING), SingleValuedAttribute("mechanism", "cleavage", STRING))

  val e1: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000019","Resnet_ClinicalParameter", edgeAttributes1)
  val e2: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_ClinicalParameter_72057594038000018","Resnet_ClinicalParameter", edgeAttributes2)
  val e3: Edge                           = Edge("Resnet_Regulation", "Resnet_SmallMol_72057594038010064", "Resnet_SmallMol","Resnet_Protein_20057594038000040","Resnet_Protein", edgeAttributes2)


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
      .emits(List(TgMessage(List(v0,v1), List(e1,e2,e3))))
      .evalTap { msg  => IO { info("Sending Request: \n" +  msg.asJson.spaces2) } }
      .evalMap { msg  => client.expect[String](makeTgRequest(msg)) }
      .evalMap { resp => IO { info("Received Response: \n" + parse(resp).getOrElse(Json.Null).spaces2) } }

  } yield ()

  postStreamMsgLogic.compile.drain.unsafeRunSync()
}
