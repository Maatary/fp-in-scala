import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.http4s.headers.`Content-Type`

//import cats._
//import cats.implicits._


import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._


//import org.http4s.headers._
//import org.http4s.server._
//import org.http4s.dsl.impl._


val helloWorldService = HttpRoutes.of[IO] {

  case areq@ GET -> Root / "hello" =>
    areq match {
      case e if e.contentType.fold(false)(content => content.mediaType == MediaType.text.turtle) => Ok("Hello, better world.".getBytes())
      case _ => BadRequest("wrong meditype")
    }


}.orNotFound


val req = Request[IO](GET, uri"/hello").withContentType(`Content-Type`(MediaType.text.turtle))



val resp = helloWorldService(req).unsafeRunSync()

helloWorldService(req).flatMap( rep => rep.as[String]).unsafeRunSync()

new String(resp.body.compile.toList.unsafeRunSync().toArray)

