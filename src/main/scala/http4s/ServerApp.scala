package http4s

import cats.effect._
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.Lang
import org.http4s.headers._
import scribe.{Level, Logger}

import java.io._

//import cats._
//import cats.implicits._


import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._

import org.http4s.client.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.Client


import org.http4s.blaze.server.BlazeServerBuilder

import java.io.ByteArrayOutputStream

object ServerApp extends App {

  Logger(classOf[org.http4s.blaze.server.BlazeServerBuilder[IO]].getName).withMinimumLevel(Level.Error).replace()

  val enrichedWithDynamic = "elsevier_entellect_enriched_dbschema_resnet_basic_with_tag_with_dynamic.ttl"

  def loadEnrichedAsByteArray(filename: String) = for {
      model         <- IO { ModelFactory.createDefaultModel() }
      _             <- IO { model.read(filename) }
      outStream     <- IO { new ByteArrayOutputStream() }
      _             <- IO { model.write(outStream, Lang.TTL.getName) } //Jena uses Uft-8 per default

    } yield(outStream.toByteArray)

  val enrichGraphService  = HttpRoutes.of[IO] {

    case GET -> Root / "enrichGraph" =>

      loadEnrichedAsByteArray(enrichedWithDynamic).flatMap { bArray =>
        Ok(bArray) map { _.withContentType(`Content-Type`(MediaType.text.turtle, DefaultCharset)) } //utf-8 is default
      }

  }.orNotFound

  // note cats.effect.unsafe.implicits.global is IORuntime.global but as implicit.
  BlazeServerBuilder[IO](IORuntime.global.compute)
  .bindHttp()
  .withHttpApp(enrichGraphService)
  .resource
  .use(_ => IO.never).as(ExitCode.Success)
  .unsafeRunSync()



}


/*
object BlazeExample extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    BlazeExampleApp.resource[IO].use(_ => IO.never).as(ExitCode.Success)
}

object BlazeExampleApp {
  def httpApp[F[_]: Async]: HttpApp[F] =
    Router(
      "/http4s" -> ExampleService[F].routes
    ).orNotFound

  def resource[F[_]: Async]: Resource[F, Server] = {
    val app = httpApp[F]
    BlazeServerBuilder[F](global)
      .bindHttp(8080)
      .withHttpApp(app)
      .resource
  }
}*/
