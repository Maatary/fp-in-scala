


/**
 * == Notes On the meaning of IO ==
 *
 * `The Red Book is not so good at explaining the essence and meaning of IO. It explain its technical aspect well though.`
 *
 * `For a full understanding of IO and its purpose (description of external action and
 * hence referential transparency and local reasoning),
 * check your notes in Evernote.`
 *
 *
 * == Notes ( sometime tweaked) from the book ==
 *
 *
 * === Part 1 (include run, ++ and empty ) ===
 *
 *  The first thing we can perhaps say about IO as it stands right now '''(see basic implementation below without map and flatMap)'''
 *  is that it forms a '''Monoid '''(empty is the identity, and ++ is the associative operation).
 *
 *  So if we have, for example, a List[IO], we can ''reduce'' that to a '''single IO''',
 *  and the '''associativity''' of '''++''' means that we can do this either by '''folding left''' or '''folding right'''.
 *  On its own, this isn’t very interesting.
 *
 *  '''All it seems to have given us is the ability to delay when a side effect actually happens.'''
 *
 * === Part 2 (include all the definition below) ===
 *
 * ==== Benefit of IO ====
 *
 *  -- '''IO computations are ordinary values'''. We can store them in lists, pass them to functions, create them dynamically,
 *  and so on. Any common pattern can be wrapped up in a function and reused.
 *
 *
 *  -- '''Reifying IO computations as values means we can craft a more interesting interpreter than the simple run method
 *  baked into the IO type itself'''.
 *  Later in this chapter, we’ll build a more refined IO type and sketch out an interpreter that uses non-blocking I/O in its implementation.
 *  What’s more, as we vary the interpreter, client code remains identical — we don’t expose the representation of IO to the programmer at all!
 *  It’s entirely an implementation detail of our IO interpreter.
 *
 *
 *  ==== Issue with Implementation bellow ====
 *
 *  -- '''flatMap StackOverflow'''
 *
 *  -- '''No Concurrency support'''
 *
 *  -- '''A value of type IO[A] is completely opaque'''. '''It’s really just a lazy identity — a function that takes no arguments.'''
 *  When we call run, we hope that it will eventually produce a value of type A, '''but there’s no way for us to inspect such a program and see what it might do.'''
 *  It might hang forever and do nothing, or it might eventually do something productive.
 *  There’s no way to tell. '''We could say that it’s too general, and as a result there’s little reasoning that we can do with IO values.'''
 *  '''We can compose them with the monadic combinators, or we can run them, but that’s all we can do.'''
 *
 *
 *  == On the stack overflow of flatMap ==
 *
 *
 *
 */

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
    a flatMap (_ => forever(a))
  }
}

/**
 * PlayGround
 */
import IO._

//Create an IO Value i.e. description of an action that affect the state of the world
val ptrl1 = printLine("Hello IO") //An IO Action which when run, returns nothing
val ptrl2 = printLine("An Introduction")

// Interpret the IO Action i.e. run the action
ptrl1.run // Do the effect and returns nothing

//++ sequence the two actions into a bigger action.
(ptrl1 ++ ptrl2).run  // Do the effect and returns nothing

//This stackoverflow
forever(printLine("forever hello")).run