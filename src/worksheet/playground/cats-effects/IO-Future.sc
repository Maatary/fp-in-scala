import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.concurrent.Future

implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

IO.fromFuture(IO{Future{Thread.sleep(10000); "hello from the future"}}).flatMap(e => IO{println(e)}).unsafeRunSync()