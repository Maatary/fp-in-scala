package ref

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref}

import scala.collection.mutable.ListBuffer

object CatsIOApp extends App {

  for {

    io2 <- IO { ListBuffer.empty[Int] }

    ref <- Ref[IO].of(io2)

    _ <- ref.update(_.+=(12))

    _ <- ref.update(_.+=(14))

    _ <- ref.update(_.+=(18))

    res <- ref.get

  } yield println(res)

  //.unsafeRunSync()

}
