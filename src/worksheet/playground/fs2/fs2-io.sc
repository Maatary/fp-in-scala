import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2._
import fs2.io.file.{Files, Path}
import cats.syntax.all._

Files[IO].
  createDirectories(Path("HelloDirectory"))
  .flatMap { _ => "HelloText".getBytes.pure[IO] }
  .flatMap { Stream.emits(_).through(Files[IO].writeAll(Path("HelloDirectory/HelloFile.txt"))).compile.drain }
  .flatMap(_ => Path("HelloDirectory/HelloFile.txt").parent.fold("Nothing")(_.toString).pure[IO])
  .unsafeRunSync()