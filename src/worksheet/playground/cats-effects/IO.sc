import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._



import cats.effect._

object Debug {

  implicit class DebugHelper[A](ioa: IO[A]) {

    def debug: IO[A] = for {
      a <- ioa
      tn = Thread.currentThread.getName
      _ <- IO { println(s"[${/*Colorize.reversed*/(tn)}] $a") }
    } yield a

  }

}

import Debug._

val h = IO { "Hello" }.debug
val w = IO { "World" }.debug




h.flatMap(a => w.map(b => s"$a $b")).debug.unsafeRunSync()

(h, w).mapN ( { case (a:String, b: String) => s"$a $b" } ).debug.unsafeRunSync()


(h, w).parMapN ( { case (a:String, b: String) => s"$a $b" } ).debug.unsafeRunSync()