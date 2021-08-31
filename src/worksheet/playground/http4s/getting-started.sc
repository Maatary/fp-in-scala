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


/**
 *  = http4 High Level Principle =
 *
 * A Server is basically a function
 * {{{ HttpApp : Request[IO] => IO [Response[IO]]  }}}
 *
 * A Route of the Server is basically a function
 * {{{  HttpRoutes : Request[IO] => IO [Option[Response[IO]] ] }}}
 *
 *  == Why the difference ==
 *
 *  -- A Server always returns a Response
 *
 *  -- A Route might note return a Response.
 *
 *  -- So as we supply routes to the server we can add a catch all function  at the end that map
 *  over the Response. If not Return NotFound else Leave the response unchanged
 *
 *  -- See function .orNotFound
 *
 *  -- Because we are describing function it is actually implemented as a kleisli flatMap
 *
 *  == Expressing it with Kleisli for Monadic Function composition ==
 *
 *  {{{ HttpApp: Kleisli[IO, Request[IO], Response[IO]] }}}
 *
 *  {{{ HttpRoutes: Kleisli[OptionT[IO, *], Request[IO], Response[IO]] }}}
 *
 *  -- We use '''OptionT[IO, *]''' to easily compose with   '''IO[ Option[ Response[IO] ]  ] '''
 *
 *  -- '''OptionT[IO, *]'''  let us compose IO[Option] as if  it was just an Option, it stacks it for us: It is a monad Transformer.
 *
 *  == Abstracting over the Effect type we get http4s Signature ==
 *
 *  {{{ HttpApp[F[_]]: Kleisli[F, Request[F], Response[F]] }}}
 *
 *  {{{ HttpRoutes[F[_]]: Kleisli[OptionT[IO, *], Request[F], Response[F]] }}}
 *
 *
 *  == The httpRoutes.Of  constructor ==
 *
 * Takes a partial function '''PartialFunction[Request[F], F[Response[F] ] ]'''
 * and automatically lift it in '''Kleisli[OptionT[IO, *], Request[F], Response[F] ]''' for your behind the scene.
 *
 * see  [[ org.http4s.HttpRoutes#of(scala.PartialFunction, cats.Monad)  ]]
 * {{{
     def of[F[_]: Monad](pf: PartialFunction[Request[F], F[Response[F]]]): HttpRoutes[F] =
        Kleisli(req => OptionT(Applicative[F].unit >> pf.lift(req).sequence))
 *
 * }}}
 *
 *  == Understanding the IO in Request[IO] & Response[IO] ==
 *
 *  === From Gitter ===
 *
 *  -- I don't really think of a Request as a Request[F, A]. The request always holds a method, a uri, and a stream of bytes.
 *
 *  -- You might use an EntityDecoder to turn that stream of bytes into an A. But there's not really a parameteric A in the request.
 *  Just F[_], or what kind of effect produces the bytes.
 *
 *  -- >> I meant Request[F, Byte] Indeed. It is implicit. But yeah, got the point. There is no parameter, it is always Byte.
 *
 *  -- Response[F] means the body happens in F, but you already have a Status and Headers in a pure context. F[Response[F]] means the Status and Headers also happen in effect type F.
 *
 *  -- So yeah, you go to a database. The database call might return a result set. It might not. It might one time and not the next. It might raise a network error. These are all effects that can happen when processing a given request.
 *
 *  --> Got it.
 *
 *  -- F[Request[F]] does happen in the backends: reading an incoming request off the socket is an effect.
 *  -- A backend has an F[Request[F]] and needs an F[Response[F]] to render. How do we get there? Well, if F is a monad, flatMap(F[A])(A => F[B]): F[B].
 *
 *
 *
 */
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



val resp = helloWorldService(req).unsafeRunSync()

helloWorldService(req).flatMap( rep => rep.as[String]).unsafeRunSync()
//Encoder should do that for you
//new String(resp.body.compile.toList.unsafeRunSync().toArray)

uri"www.example.com".path

