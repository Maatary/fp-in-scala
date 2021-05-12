package ScribePlayGround


sealed trait IO[A] { self =>

  def run: A

  /**
   * Coincidence: act a bit like a flatMap
   *
   * Sequence 2 IO Action into one Bigger Action
   *
   * i.e. On interpretation run the first then the second
   *
   * However it does not take into account the first result
   *
   * It Just discard it.
   */
  def ++(io: IO[A]): IO[A] = new IO[A] {
    def run: A = {self.run; io.run}
  }

  def map[B](f: A => B): IO[B] = new IO[B] {
    def run: B = f(self.run)
  }

  // Here we build a function see `def run`
  def flatMap[B](g: A => IO[B]): IO[B] = new IO[B] {
    def run = g(self.run).run
  }

  //That's not building a function
  def _flatMap[B](g: A => IO[B]): IO[B] = {
    g(self.run)
  }



}

object IO {

  /**
   * Lift a Value into an IO
   */
  def unit[A](a: => A): IO[A] = new IO[A] {def run = a}

  /**
   * Construct an IO Value
   */
  def apply[A](a: => A): IO[A] = unit(a)

  /**
   * Compose a '''List of IO Action that each return an A''' into an '''IO Action that return a List of A'''
   */
  def sequence[A](l: List[IO[A]]): IO[List[A]] = {
    l.foldRight(unit(Nil:List[A])) {(a, b) => a.flatMap{e => b.map(e::_)} }
  }

  /**
   * Fill a list with a monadic function and then compose list of functions into a bigger function
   * that sequence them
   */
  def replicateM[A](n:Int)(io: IO[A]) = {
    sequence[A](List.fill[IO[A]](n)(io))
  }

  def empty: IO[Unit] = new IO[Unit] { def run: Unit = () }

  def printLine(msg: String): IO[Unit] = new IO[Unit] {def run: Unit = println(msg)}

  def buildMsg(msg:String) = new IO[String] {def run = msg}

  def forever[A,B](a: IO[A]): IO[B] = {
    //lazy val t: IO[B] = forever(a) // no need for this here
    a flatMap (_ => forever(a))
  }
}


object IOBasic extends App {

  import IO._

  forever(printLine("forever hello")).run


}
