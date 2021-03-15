
/***
 *  == Critical Point to remember ==
 *
 *  The recursive function sequence left or right building a data structure rcursively.
 *
 *  In nutshell, from a list of function we compose their applicatin to return a function that returns a list of their result.
 *
 *  What is important to understand here is that, the list that the combinator function returns is specified as a recursive function.
 *
 *  That is the list of results is a recursive function which upon evaluation yield the actual list of result.
 *
 *  Said differently the Sequence Combinator build a recursive function which upon evaluation return a list of result.
 *
 *  That is, it builds a function that recursively apply the functions of its input list and build the list of the result of their application.
 */


case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State[S, B] { (s: S) =>
    val (a1, s1) = run(s)
    f(a1) -> s1
  }

  def map2[B, C](stB: State[S, B])(f: (A, B) => C): State[S, C] = State[S, C] { (s: S) =>
    val (a, sa) = run(s)
    val (b, sb) = stB.run(sa)
    (f(a, b), sb)
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State[S, B] { (s: S) =>
    val (a, sa) = run(s)
    g(a).run(sa)
  }

  import State._ // To reuse unit

  /**
   * == Remember ==
   *
   * Here you map in terms of flatMap,
   * hence no need to create the combined function yourself,
   * that is what flatMap does !!!
   *
   * It combines State[S, A] with the function resulting from unit(f(a)) i.e. constant State[S, B]
   *
   * unit(f(a)) is inferred to be State[S, B] because of the signature of flatMap
   *
   * In particular what is inferred is `"S"`.
   *
   * Without specifying it, it could have been inferred to be nothing (the default for invariant and covariant type)
   *
   */
  def _map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  /**
   * == Remember ==
   *
   * flatMap is more powerful than [[map]] and [[map2]].
   *
   * Both can be implemented in term of [[flatMap]]
   */
  def _map2[B, C](stB: State[S, B])(f: (A, B) => C): State[S, C] = {
    flatMap { a => stB.map { b => f(a, b) } }
  }


  /**
   * == As flatMap using for-Comprehension ==
   */
  def __map[B](f: A => B): State[S, B] = {
    for {
      a <- this // thread the result of the first function
      b <- unit(f(a)) // make explicit the input of the map function of the result of the second function
    } yield b // map over the result of the second function
  }

  /**
   * == As flatMap using for-Comprehension ==
   */
  def __map2[B, C](stB: State[S, B])(f: (A, B) => C): State[S, C] = {
    for {
      a <- this // thread the result of the first function
      b <- stB // make explicit the input of the map function of the result of the second function
    } yield f(a, b) // map over the result of the second function with closure over the result of the fist function
  }

}

object State {
  //def unit[S, A](a: A): State[S, A] = State((s: S) => (a, s))
  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }


  /**
   * == Critical Notes - Via foldRight ==
   *
   * The idiomatic solution is expressed via foldRight
   *
   */
  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight(unit[S, List[A]](Nil)) { (a, b) => a.map2(b)((a, b) => a :: b) }
  }

  /**
   * == Critical Notes - Via foldRight Raw ==
   *
   * The idiomatic solution expressed as an explicit/equivalent of foldRight
   *
   */
  def _sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = l match {
    case Nil => unit(Nil)
    case s :: xs => s.map2(sequence(xs))((a, b) => a :: b)
  }


  /**
   * == Critical Notes - Via foldLeft ==
   *
   * Need to `reverse` because it is a `foldLeft`, which build the list by reversing the order !!!
   *
   * Note however that we reverse before composing. l.reverse.foldLeft
   *
   * We can do that because we are dealing with function composition as opposed to "computation directly"
   * (for a lack of a better word)
   *
   * This is wrong in a scenario where a `statefull application/computation`
   * happen while doing the folding (see [[intsLeft]] in purely-functional-state-rng)
   *
   * Indeed the order/sequencing of the application of the computation matters !!!
   * However it matters at application time !!!
   *
   * Here we are not applying the function but simply composing function
   * hence we can reverse the order first and compose next ???? `Really???`
   *
   * == Book Notes: ==
   *
   * We can also write the loop using a left fold. This is tail recursive like the
   * previous solution, but it reverses the list _before_ folding it instead of after.
   * You might think that this is slower than the `foldRight` solution since it
   * walks over the list twice, but it's actually faster! The `foldRight` solution
   * technically has to also walk the list twice, since it has to unravel the call
   * stack, not being tail recursive. And the call stack will be as tall as the list
   * is long.
   *
   */
  def __sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.reverse.foldLeft(unit[S, List[A]](Nil)) { (b, a) => b.map2(a)((b, a) => a :: b) }
  }


  /**
   * == Critical Notes - Via foldLeft Raw ==
   *
   * This implementation uses a loop internally and is the same recursion
   * pattern as a left fold. It is quite common with left folds to build
   * up a list in reverse order, then reverse it at the end.
   * (We could also use a collection.mutable.ListBuffer internally.)
   */
  def ___sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
      }
    State((s: S) => go(s,sas,List()))
  }



}

import State._

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }
}

/*
val nextIntState = State[RNG, Int](s => s.nextInt)

val listNextInt = List.fill(10)(nextIntState)


//Sequence Right
sequence(listNextInt).run(SimpleRNG(42))


//Sequence Left
__sequence(listNextInt).run(SimpleRNG(42))

//Sequence Left Raw

___sequence(listNextInt).run(SimpleRNG(42))*/


List(println("1"), println("2"), println("3"))

def test(n: Int) = {
  val y = e + 1
  val e = n
}

def test2(n: Int) = {
  val y = e + 1
  val e = n
}