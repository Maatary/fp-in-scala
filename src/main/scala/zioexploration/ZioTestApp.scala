package zioexploration

import org.apache.kafka.clients.producer._
import zio._
import zio.blocking.Blocking
import zio.clock.Clock
import zio.console.Console
import zio.duration.durationInt
import zio.json._
import zio.kafka.consumer._
import zio.kafka.producer.{Producer, ProducerSettings}
import zio.kafka.serde.Serde
import zio.stream._

import java.util.UUID
import scala.util.{Failure, Success}


/**
 * The orginal Zio provide is just the run method of a Klesli (see basic idea below)
 *
 * {{{
 *   case class Reader[-R, +A](provide: R => A) { self =>
 *     def map[B](f: A => B) = flatMap(a => Reader.point(f(a)))
 *     def flatMap[R1 <: R, B](f: A => Reader[R1, B]) =
 *        Reader[R, B](r => f(self.provide(r)).provide(r))
 *   }
 *   object Reader {
 *     def point[A](a: => A): Reader[Any, A] = Reader(_ => a)
 *     def environment[R]: Reader[R, R] = Reader(identity)
 *     def access[R, A](f: R => A): Reader[R, A] =
 *       environment[R].map(f)
 *   }
 * }}}
 *
 */





object ZioTestApp extends App {

  val consumerSettings: ConsumerSettings =
    ConsumerSettings(List("localhost:9092"))
      .withGroupId("stocks-consumer")

  val managedConsumer: RManaged[Clock with Blocking, Consumer.Service] =
    Consumer.make(consumerSettings)

  val consumer: ZLayer[Clock with Blocking, Throwable, Consumer] =
    ZLayer.fromManaged(managedConsumer)

  //Stream.succeed("").provideLayer()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ???
}
