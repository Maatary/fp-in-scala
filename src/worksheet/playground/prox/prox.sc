import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.github.vigoo.prox.ProxFS2


val          prox  : ProxFS2[IO]                   = ProxFS2[IO]
import prox._
implicit val runner: ProcessRunner[JVMProcessInfo] = new JVMProcessRunner




val proc1 = Process("ls", List("-hal"))

val proc1Redirected =
  proc1
    .toFoldMonoid(fs2.text.utf8.decode)
    .errorToFoldMonoid(fs2.text.utf8.decode)

val  res: IO[prox.ProcessResult[Unit, Unit]] = proc1.run()

proc1Redirected.run().unsafeRunSync()