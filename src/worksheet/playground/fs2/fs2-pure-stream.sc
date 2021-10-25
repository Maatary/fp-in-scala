import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO}
import fs2.Stream
import cats.syntax.all._

import java.util.EmptyStackException



//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

val s1 = Stream(1,2,3,4,5,8,7,8)

s1.take(6)

Stream(1,2,3).map(_ + 1).toList

//Stream(List(1,2,3,4,5):_*).take(1).toList


/*val test = for {
  fiber <- IO.raiseError(new RuntimeException("boom"))
  _     <- IO.sleep(1.seconds)
} yield ExitCode.Success

test.unsafeRunSync()*/
/*
implicit def ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

Future{throw new EmptyStackException }*/

val eff = Stream.eval(IO { println("BEING RUN!!"); 1 + 1 })


Stream.emits(List(1,2,3,4))

val e1 = Stream(List(1,2,3,4), List(3,33,56,7))
  .evalMap {
    list => Stream.emits(list).evalMap(IO.pure).compile.last
  }.compile.toVector.unsafeRunSync()

val e2 = Stream(List(1,2,3,4), List(3,33,56,7))
  .flatMap {
    list =>
      Stream
      .eval(
        Stream.emits(list)
        .evalMap(IO.pure)
        .compile.last
      )
  }.compile.toVector.unsafeRunSync()




