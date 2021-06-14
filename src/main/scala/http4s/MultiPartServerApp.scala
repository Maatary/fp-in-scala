package http4s

import org.apache.jena.rdf.model._
import org.apache.jena.riot._
import cats.syntax.all._
import fs2.io.readInputStream
import org.http4s.EntityDecoder.multipart
import org.http4s.multipart.Part._
//import cats._
//import cats.implicits._

import cats.effect._
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global

import org.http4s._
import org.http4s.headers._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.multipart._

import org.http4s.client.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.Client

import java.io._

import scribe._


object MultiPartServerApp extends App {



  def inputStreamAsFileData[F[_]](name: String, filename: String, in: => InputStream, headers: Header.ToRaw*) (implicit F: Sync[F]): Part[F] = {
    val ChunkSize = 8192
    fileData(name, filename, readInputStream(F.delay(in), ChunkSize), headers: _*)
  }

  def asMultiPart(basicTagArray: Array[Byte]): Multipart[IO] = {
    Multipart[IO](
      Vector(
        //Part.formData("text", "This is text."),
        inputStreamAsFileData[IO]("basicSchema", "basic_with_tag", new ByteArrayInputStream(basicTagArray), `Content-Type`(MediaType.text.turtle, DefaultCharset))
      ))
  }

  def loadEnrichedAsByteArray(filename: String) = for {

    model         <- IO { ModelFactory.createDefaultModel() }
    _             <- IO { model.read(filename) }

    outStream     <- IO { new ByteArrayOutputStream() }
    _             <- IO { model.write(outStream, Lang.TTL.getName) } //Jena uses Uft-8 per default


  } yield(outStream.toByteArray)



  val enrichedWithDynamic = "elsevier_entellect_enriched_dbschema_resnet_basic_with_tag_with_dynamic.ttl"
  val basicWithTag        = "elsevier_entellect_enriched_dbschema_resnet_basic_with_tag.ttl"


  val enrichGraphService  = HttpRoutes.of[IO] {

    case req @ POST -> Root / "enrichGraph" =>


      val response = loadEnrichedAsByteArray(enrichedWithDynamic).flatMap { bArray =>
        Ok(bArray) map { _.withContentType(`Content-Type`(MediaType.text.turtle, DefaultCharset)) } //utf-8 is default
      }

      req.decodeWith(multipart[IO], strict = true) { multipart : Multipart[IO] =>

        multipart.parts.find(_.filename === Some("basic_with_tag"))
          .fold { BadRequest("missing basic_with_tag file") } { part =>
            part.as[String].flatTap(e => IO {info(e)}).flatMap(_ => response)
          }
      }

  }.orNotFound


  val stringIO = for {

    basicWithTagArray <- loadEnrichedAsByteArray(basicWithTag)

    multiPart = asMultiPart(basicWithTagArray)
    request   = Method.POST(multiPart, uri"http://localhost/enrichGraph").withHeaders(multiPart.headers)

    responseOfIO          <- enrichGraphService(request)

    response              <- responseOfIO.as[String]


  } yield (response)

  println(stringIO.unsafeRunSync())

}
