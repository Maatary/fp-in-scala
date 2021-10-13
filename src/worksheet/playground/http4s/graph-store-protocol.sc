import org.apache.jena.rdf.model._
import org.apache.jena.riot._
import cats.syntax.all._
import fs2.io.readInputStream
import org.http4s.blaze.client.BlazeClientBuilder
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





BlazeClientBuilder[IO](IORuntime.global.compute)
  .resource
  .use { client =>

    val request: Request[IO]  =
      GET(uri"https://stardog.sdc-oxygen-dev.nonprod.entellect.com/IntegrationHub?graph=https://data.elsevier.com/lifescience/schema/skos",
        Authorization(BasicCredentials("admin", "admin")))

    client.expect[String](request)

  }.
  flatMap{
    IO.println(_)
  }.unsafeRunSync()






