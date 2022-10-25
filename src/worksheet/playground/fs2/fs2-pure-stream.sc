import cats.effect.unsafe.implicits.global
import cats.effect._
import cats.kernel.Monoid
import fs2.Stream
import cats.syntax.all._
import fs2.kafka.{AutoOffsetReset, ConsumerRecord, ConsumerSettings, Deserializer, KafkaConsumer}

import java.util.EmptyStackException
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.language.postfixOps



//import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

/*val s1 = Stream(1,2,3,4,5,8,7,8)

s1.take(6)

Stream(1,2,3).map(_ + 1).toList*/

//Stream(List(1,2,3,4,5):_*).take(1).toList


/*val test = for {
  fiber <- IO.raiseError(new RuntimeException("boom"))
  _     <- IO.sleep(1.seconds)
} yield ExitCode.Success

test.unsafeRunSync()*/
/*
implicit def ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

Future{throw new EmptyStackException }*/

/*
val eff = Stream.eval(IO { println("BEING RUN!!"); 1 + 1 })


Stream.emits(List(1,2,3,4))

val e1 = Stream(List(1,2,3,4), List(3,33,56,7))
  .evalMap {
    list => Stream.emits(list).evalMap(IO.pure).compile.last
  }.compile.toVector.unsafeRunSync()

val e2 = Stream(List(1,2,3,4), List(3,33,56,7))
  .flatMap { list =>
    Stream.eval(Stream.emits(list).evalMap(IO.pure).compile.last)
  }.compile.toVector.unsafeRunSync()

val e3 = Stream(List(1,2,3,4), List(3,33,56,7))
  .flatMap { list =>
    Stream
      .eval(
        Stream.emits(list).covary[IO].groupWithin(2, 1.seconds)
          .evalMap(ch => IO.println(ch.toList)).compile.drain
      )
      .evalMap(_ => IO.println(s"done with batch:$list"))
      .as(list.last)
  }.compile.toVector.unsafeRunSync()
*/

/*
Stream(1,2,3).chunkN(1).unchunks

Stream.range(1,8).evalMapChunk(i => IO.println(i)).take(2).compile.drain.unsafeRunSync()
Stream.range(1,8).buffer(4).evalMapChunk(i => IO.println(i)).take(2).compile.drain.unsafeRunSync()
*/

/*
Stream.emits(1 to 20 toList).covary[IO]
  .mapAsync(20)(i => IO.println(Thread.currentThread().getName + ": " + i))
  .take(4)
  .compile
  .drain
  .unsafeRunSync()

Stream.emits(1 to 20 toList).covary[IO]
  .evalMap(i => IO.println(Thread.currentThread().getName + ": " + i))
  .prefetchN(20)
  .take(4)
  .compile
  .drain
  .unsafeRunSync()

Stream.range(1,20).buffer(4).covary[IO]
  .mapAsync(2)(i => IO.println(Thread.currentThread().getName + ": " + i))
  .take(2)
  .compile
  .drain
  .unsafeRunSync()

Stream.range(1,20).covary[IO]
  .evalMapChunk(i => IO.println(Thread.currentThread().getName + ": " + i))
  .take(2)
  .compile
  .drain
  .unsafeRunSync()
*/

/**
 * Testing Chunks
 */
Stream.range(1, 5).debugChunks().compile.drain  // chunk of 1 element
Stream.emits(1 to 4).debugChunks().compile.drain //chunk of 4 element but stream of Int
Stream.emits(1 to 4 toList).debugChunks().compile.drain //chunk of 4 element but stream of Int


Stream.range(1, 4).debugChunks().map{_ * 2}.take(2).debugChunks().compile.drain

// u need a println in  map to see
Stream.emits(1 to 4).debugChunks().map{e => println(e); e * 2}.take(2).debugChunks().compile.drain



Stream.emit(1,2,3,4).debugChunks().compile.drain // weird make a 4-tuple

import fs2.Chunk
Stream.chunk(Chunk(1,2,3,4)).debugChunks().compile.drain // you set the chunk but it is a stream of element here not chunk
Stream.chunk(Chunk(1,2,3,4)).chunks // to make the chunk explicit


Stream.emits(1 to 20 toList).debugChunks()
  .unchunk.debugChunks()
  /*.unchunks.debugChunks()
  .chunkN(4).debugChunks()
  .unchunks.debugChunks()*/
  .compile.drain

/*Stream.emits(1 to 20 toList).covary[IO].chunkN(20).unchunks
  .mapAsync(20)(i => IO.println(Thread.currentThread().getName + ": " + i))
  .take(4)
  .compile
  .drain
  .unsafeRunSync()*/

/*def processRecord(record: ConsumerRecord[String, String]): IO[Unit] =
  IO(println(s"Processing record: $record"))

val consumerSettings = ConsumerSettings(
  keyDeserializer = Deserializer[IO, String],
  valueDeserializer = Deserializer[IO, String]
).withAutoOffsetReset(AutoOffsetReset.Earliest)
  .withBootstrapServers("localhost:9092")
  .withGroupId("group")

val stream =
  KafkaConsumer.stream(consumerSettings)
    .subscribeTo("topic")
    .partitionedRecords
    .map { partitionStream =>
      partitionStream
        .evalMap { committable =>
          processRecord(committable.record)
        }
    }
    .parJoinUnbounded*/

Stream(1,2,3,4).debugChunks().compile.drain

//Repeat is Lazy, that is, you repeat on call.
Stream
  .repeatEval(IO {List (1,2,3,4)})
  .flatMap{list  => Stream.emits(list) }
  .take(8)
  .evalMap(IO.println(_))
  .compile
  .drain.unsafeRunSync()
