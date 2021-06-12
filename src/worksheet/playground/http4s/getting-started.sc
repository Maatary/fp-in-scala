import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.http4s.headers._

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


val helloWorldService = HttpRoutes.of[IO] {

  case areq@ GET -> Root / "hello" =>

    areq match {

      case e if e.contentType.fold(false)(content => content.mediaType == MediaType.text.turtle) =>

        Ok("Hello, better world.".getBytes()).map(_.withHeaders())

      case _ => BadRequest("wrong mediaType")
    }


}.orNotFound


val req = Request[IO](GET, uri"http://www.example.com/hello").withContentType(`Content-Type`(MediaType.text.turtle))

//POST("hello", uri"")

`Cache-Control`

val resp = helloWorldService(req).unsafeRunSync()

helloWorldService(req).flatMap( rep => rep.as[String]).unsafeRunSync()
//Encoder should do that for you
//new String(resp.body.compile.toList.unsafeRunSync().toArray)

uri"www.example.com".path

