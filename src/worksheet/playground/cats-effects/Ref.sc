
import cats.syntax.all._
import cats.effect._
import cats.effect.unsafe.implicits.global

import scala.collection.mutable.ListBuffer


val refIO = Ref[IO].of(2)
val ref = refIO.unsafeRunSync()
// never happening, nor remembered, need to run ref.update(_ => 4).unsafeRunSync()
ref.update(_ => 4).unsafeRunSync()
ref.get.unsafeRunSync()


Ref[IO].of(2).flatMap{ ref => ref.update(_ => 10).flatMap(_ => ref.get)}.unsafeRunSync()

(for {
  io2 <- IO { ListBuffer.empty[Int] }
  ref <- Ref[IO].of(io2)
  _   <- ref.update(_.+=(12))
  _   <- ref.update(_.+=(14))
  _   <- ref.update(_.+=(18))
  res <- ref.get
} yield (res)).unsafeRunSync()

