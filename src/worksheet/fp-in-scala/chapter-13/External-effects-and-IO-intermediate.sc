import scala.concurrent.Future

sealed trait IO[A] {

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap[A,B](this, f) // we do not interpret the `flatMap` here, just return it as a value
  def map[B](f: A => B): IO[B] =
    flatMap[B](f andThen (Return(_)))

}
case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]

object IO { // Notice that none of these operations DO anything

  def unit[A](a: => A): IO[A] = Return(a)

  def forever[A,B](a: IO[A]): IO[B] = {
    lazy val t: IO[B] = forever(a)
     a flatMap (_ => t)
  }
}



/*def printLine(s: String) =
  Suspend( () => Return(println(s)) )*/

def printLine(s: String): Suspend[Unit] =
  Suspend( () => println(s) )

val p = IO.forever(printLine("Still going..."))

/*val actions: Stream[IO[Unit]] =
  Stream.fill(100000)(printLine("Still going..."))
val composite: IO[Unit] =
  actions.foldLeft(IO.unit(())) { (acc, a) => acc flatMap { _ => a } }*/

// There is only one sensible way to implement this as a
// tail-recursive function, the one tricky case is left-nested
// flatMaps, as in `((a flatMap f) flatMap g)`, which we
// reassociate to the right as `a flatMap (ar => f(a) flatMap g)`
@annotation.tailrec
def run[A](io: IO[A]): A = io match {
  case Return(a) => a
  case Suspend(r) => r()
  case FlatMap(x, f) => x match {
    case Return(a) => run(f (a))
    case Suspend(r) => run(f( r()))
    //case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
  }
}

run(p)


