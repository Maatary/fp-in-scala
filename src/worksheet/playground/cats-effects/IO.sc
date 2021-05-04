import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

def print(x: Int) = {
  if(x == 3 || x == 4)
    IO.raiseError(new RuntimeException("error " + x))
  else
    IO(println(x))
}

List.range(1, 6).traverse(print(_)).unsafeRunSync()
List.range(1, 6).traverse(print(_).attempt)