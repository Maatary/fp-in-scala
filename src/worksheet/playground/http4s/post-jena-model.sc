import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.Lang
import org.http4s.headers._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

//import cats._
//import cats.implicits._


import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._

import org.http4s.client.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.Client

//import org.http4s.headers._
//import org.http4s.server._
//import org.http4s.dsl.impl._



def loadEnrichedAsByteArray(filename: String) = {

  for {

    model         <- IO { ModelFactory.createDefaultModel() }
    _             <- IO { model.read(filename) }

    outStream     <- IO { new ByteArrayOutputStream() }
    _             <- IO {model.write(outStream, Lang.TTL.getName)} //Jena uses Uft-8 per default



  } yield(outStream.toByteArray)

}

val enrichedWithDynamic = "elsevier_entellect_enriched_dbschema_resnet_basic_with_tag_with_dynamic.ttl"


val enrichGraphService  = HttpRoutes.of[IO] {

  case GET -> Root / "enrichGraph" =>

    loadEnrichedAsByteArray(enrichedWithDynamic).flatMap { bArray =>
      Ok(bArray) map { _.withContentType(`Content-Type`(MediaType.text.turtle, DefaultCharset)) } //utf-8 is default
    }

}.orNotFound


val req = Request[IO](GET, uri"http://www.example.com/enrichGraph")


enrichGraphService(req).flatMap(_.as[String]).unsafeRunSync()



val receviedModel =
  for {
  arrayBytes <- enrichGraphService(req).flatMap(_.as[Array[Byte]]) //utf-8
  inStream   <- IO {new ByteArrayInputStream(arrayBytes)}
  model      <- IO {ModelFactory.createDefaultModel().read(inStream, null, Lang.TTL.getName)} //jena expect utf-8 per default

  } yield(model)

receviedModel.flatMap(model => IO{model.write(System.out, Lang.TTL.getName)}).unsafeRunSync()



