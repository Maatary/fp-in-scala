import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.github.vigoo.prox.ProxFS2


val f = (a: String) => (b: String) => (c :Int) => b match {
  case "hello" => println("it's hello"); s"hello $a"
  case _ => println("it's not hello"); s"good by $a"
}

f("Maatari")("hello")(1)


val h = (a: String) => (b: String) => b match {
  case "hello" =>  (c: Int)  => { s"hello $a you are ${c.toString} years old"}
  case _       =>  (c: Int)  => s"bye $a you are ${c.toString} years old"
}


def func(a:  String)(b: String) = a + b

func("2")(_)



val          prox  : ProxFS2[IO]                   = ProxFS2[IO]
import prox._

implicit val runner: ProcessRunner[JVMProcessInfo] = new JVMProcessRunner




val proc1 = Process("ls", List("-hal"))

val proc1Redirect = proc1.toFoldMonoid(fs2.text.utf8.decode)

val  res: IO[prox.ProcessResult[Unit, Unit]] = proc1.run()

proc1Redirect.run().unsafeRunSync()



