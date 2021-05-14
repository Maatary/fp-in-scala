package IO.IOBasic

/**
 *
 * = Solving the IO StackOverflow Problem =
 *
 *  -- (1) '''Instead of letting program control just flow through with function calls, we explicitly bake into our data type the control flow that we want to support.'''
 *
 *  -- (2) '''Instead of making flatMap a method that constructs a new IO in terms of run, we can just make it a data constructor of the IO data type.'''
 *
 *  -- (3) '''Then the interpreter can be a tail-recursive loop. Whenever it encounters a constructor like FlatMap(x,k), it will simply interpret x and then call k on the result.'''
 *
 *  == On point (2) ==
 *
 *  This means not defining as `run = f(self.run).run` (see implication below):
 *
 *  - `run()`
 *      - `newIO = f(self.run)`
 *      - `newIO.run()   `                  ''// stacking first run()''
 *                - `newIO = f(self.run)`
 *                - `newIO.run()`         ''// stacking second run()''
 *
 *
 *
 *  -- Where the execution of the first IO must Stack to execute the next IO.
 *  -- If the second IO is itself a result of a flatMap, then we start piling up the stack further.
 *
 *
 *  == Solution: Modeling the Control Flow ==
 *
 *  This new IO type has three data constructors, representing the three different kinds of control flow that we want the interpreter of this data type to support.
 *
 *  -- '''Return''' represents an IO action that has finished, meaning that we want to return the value a without any further steps.
 *
 *  -- '''Suspend''' means that we want to execute some effect to produce a result.
 *
 *  -- '''FlatMap'''  lets us continue an existing computation by using the result of the first computation to produce a second computation.
 *
 *    - The flatMap method’s implementation can now simply call the FlatMap data constructor and return immediately.
 *
 *    - When the interpreter encounters FlatMap(sub, k), it can interpret the subcomputation sub and then remember to call the continuation k on the result.
 *
 *    - Then k will continue executing the program.
 *
 *
 *
 */
sealed trait IO[A] {

  def flatMap[B](f: A => IO[B]): IO[B] = {
    FlatMap[A,B](this, f)
  }


  def map[B](f: A => B): IO[B] = {
    flatMap[B] { a => Return(f(a)) }
  }

}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]



object IO {

  def unit[A](a: => A): IO[A] = Return(a)

  /**
   * With our new implementation of flatMap based on our control flow datatype i.e. Return, Suspend and FlatMap
   *
   * Forever now create an infinite nested structure, much like a Stream.
   *
   * The “head” of the stream is a Function0, and the rest of the computation is like the “tail”:
   *
   * {{{FlatMap( IO  ,
   *             _ => FlatMap( IO  , _ => FlatMap(...))) }}}
   *
   * === Illustration using "printLine" as the IO i.e. forever(printLine) ===
   *
   *
   * {{{FlatMap( Suspend(() => println(s)) ,
   *             _ => FlatMap( Suspend(() => println(s)) , _ => FlatMap(...))) }}}
   *
   *
   *
   */
  def forever[A,B](a: IO[A]): IO[B] = {
    a flatMap (_ => forever(a))
  }

  /**
   * A Simple Delayed computation (i.e. Function0)
   */
  def printLine(s: String): IO[Unit] = Suspend(() => println(s))

  /**
   *
   * -- If '''x''' is itself a '''FlatMap constructor''' , then we know that '''IO''' consists of '''two FlatMap constructors''' '''nested''' on the '''left''':
   *
   *
   *
   *
   * -- `FlatMap( FlatMap (y , g) , f ) ` ==  ` ( y flatMap g ) flatMap f `
   *
   *
   *
   *
   * -- We then '''re-associate it to the right''' as such:
   *
   *
   *
   *
   * -- `(y flatMap g) flatMap f --> y flatMap (a => g(a) flatMap f)`
   *
   *
   * -- `FlatMap( FlatMap (y , g) , f ) ` --> `FlatMap( y, a => FlatMap (g(a), f) )`
   *
   *
   * --
   *
   */
  @annotation.tailrec
  def run[A](io: IO[A]): A = io match {

    case Return(a) => a

    case Suspend(r) => r()

    case FlatMap(x, f) => x match { // things not to do run(f(run(x))) //can stackoverflow because of inner run

      case Return(a) => run(f (a))

      case Suspend(r) => run(f( r()))

      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f)) //  FlatMap( FlatMap (y , g) , f ) --> FlatMap( y, a => FlatMap (g(a), f) )

      //case FlatMap(y, g)  => run (  FlatMap (y, { (e:Any) => FlatMap ( g (e), f) } )  )

    }
  }

}


object IOBasic extends App {

  import IO._



  /**
   * This creates an infinite nested structure, much like a Stream.
   *
   * The “head” of the stream is a Function0, and the rest of the computation is like the “tail”:
   *
   * === Illustration ===
   *
   * {{{FlatMap( Suspend(() => println(s)) ,
   *             _ => FlatMap(Suspend(() => println(s)), _ => FlatMap(...)))}}}
   *
   */
  val p = forever(printLine("Hello forever ... "))

  //run(p) //No Overflow

  def f: Int => IO[Int] = (x: Int) => Return(x * 2)

  val fun = List.fill(4)(f).foldLeft[(Int => IO[Int])](f) {

    // Why not ? Because you have no delay no () => A, no suspension (Kind of if we consider the flatMap structure build that need to be interpreted)
    // I don't think it really makes a difference here.
    //TODO investigate this
    //case (a,b) => { (x:Int) => a(x) flatMap b }

    case (a,b) => {  x => Suspend( () => () ) flatMap { _ => a(x).flatMap(b)}  }// Book trick, i don't like it. Just to have a suspend but why

    //case (a,b) => (x:Int) => Suspend ( () => a(x).flatMap(b) ) } errata error
  }

  println(run(fun(2)))
}
