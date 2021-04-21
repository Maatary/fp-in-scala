import scala.annotation.tailrec

/**
 * == Notes ==
 *
 * State is just a function S => (A, S)
 *
 * The Combinator on State are combinator of function.
 *
 * That is, the combinator allows you to combine function application
 *
 * This means building a bigger function that apply the input function in specific way e.g.
 *
 *    -- Mapping where the mapping function is applied to the result of the state function.
 *
 *    -- sequencing where the result of the first one is sequence to the second one.
 *
 * == -Critical On The meaning Of Sequence- ==
 *
 *    -- In the case of sequence, your bigger function is the recursive function.
 *
 *    -- You do not build a bigger function that sequence like in your code of map2 all the function application
 *
 *    -- It is actually the recursive function call that does it
 *
 *    -- The recursive function is the bigger function which apply all the function in your list recursively
 *
 *    -- Step back and remember that a recursive function describe a series of call of which we do not know the number in advance
 *
 *    -- The succession of call to make is described by the recursion, of which the number is determined by the execution itself (based on the terminal point of the recursion)
 *
 *    -- So that full unraveled blue print of a bigger function composed into that you are looking for is actually described by a recursive (structure) which is a function here
 *
 *    -- By applying we mean build chuck of bigger function out of 2 functions i.e. use map 2
 *
 *    -- The bigger function is never fully unravel into its smaller chucnk but when you apply the bigger function
 *
 *    -- It is actually either fully unravel on the stack when using fold right, or directly applied by chunk if using foldLeft
 *
 *    -- So via foldLeft, the full unraveling never happen, you recursively apply each chunk according to the bigger picture i.e. the Bigger function
 *
 *    -- In the end, it all comes down to understanding that `the Bigger function is a recursive function`
 *
 *    -- Think of it as setting the bigger function as executing a chain of map2 sequentially, it is just never fully unravel as explained above expect artificially on the stack when using foldRight
 *
 *    -- The stack `artificially unravel the all thing for use`
 *
 *  === -Take Away On The Meaning Of Sequence- ===
 *
 *    -- You do not have that all sequence of function made up / laid out before the execution. !!!
 *
 *    -- The all Sequence is describe by the Recursive Structure  !!!!
 *
 *    -- It is only fully laid out artificially on the Stack via FoldRight but as execution i.e. actual function call
 *
 *    -- In other words, the sequence is unravelled as you execute it !!!
 *
 *    -- It is the function calls that are fully unravelled in the stack, not the function definition. That blue print sequence will never exist !!!!
 *
 */


case class State[S, +A](run: S => (A, S)) {

  /**
   * Combine a mapping function f: A => B with the state function S => (A, S)
   * to return a new state function S => (f(A), S)
   */
  def map[B](f: A => B): State[S, B] = State[S, B] { (s: S) =>
    val (a1, s1) = run(s)
    f(a1) -> s1
  }

  /**
   * Map2 is a bit more complex as it does two things in building a function that combines the two state function and a mapping function.
   *
   *  -- It combine two state functions by sequencing their execution,
   *     which imply threading the returned state of the application of the first state action
   *     (to the input of the state function into which the all thing is being combined into)
   *     to/for the application of the second state action
   *
   *  -- It then produce the (value, state) pair result where the value is the function f: (A, B) => C
   *     applied to the 2 values return by each state action, and the state is the one returned by the last state action.
   */
  def map2[B, C](stB: State[S, B])(f: (A, B) => C): State[S, C] = State[S, C] { (s: S) =>
    val (a, sa) = run(s)
    val (b, sb) = stB.run(sa)
    (f(a, b), sb)
  }

  /**
   *  == Critical Reminder ==
   *
   * It is critical to remember that we're combining 2 fips into a function that reuse/apply those 2 fips.
   *
   * g(a).run(sa) is important
   *
   * g(a) is State[S, B], but your function already define the input parameter (s: S) which is required to run "this",
   * i.e. the first state.
   *
   * Hence to recover State[S, B], you need to apply sa, as in g(a).run(sa) which yield a (B, S)
   * hence S => (B, S)
   *
   * Beyond the technical follow the type explanation above, reminding ourself what we are doing further help with this:
   *
   * `It is critical to remember that we're combining 2 fips into a function that reuse/apply those 2 fips.`
   *
   *   -- `That is, we are creating a function that sequence the application of the two input function`
   *
   * or
   *
   *   -- `That is, we are creating a state action that sequence the application of the two input state action`
   *
   * This falls in the pattern of composing function. add a new parameter, and apply the internal function to recover
   * the function you want at this. you get a function that will apply the function you compose,
   * by specifying how those internals will be applied to the outer new parameter. (to be re-written)
   *
   */
  def flatMap[B](g: A => State[S, B]): State[S, B] = State[S, B] { (s: S) =>
    val (a, sa) = run(s)
    g(a).run(sa) // We pass the new state along
  }

  import State._ // To reuse unit

  /**
   *  == Remember ==
   *
   *  Here you map in terms of flatMap,
   *  hence no need to create the combined function yourself,
   *  that is what flatMap does !!!
   *
   *  It combines State[S, A] with the function resulting from unit(f(a)) i.e. constant State[S, B]
   *
   *  unit(f(a)) is inferred to be State[S, B] because of the signature of flatMap
   *
   *  In particular what is inferred is `"S"`.
   *
   *  Without specifying it, it could have been inferred to be nothing (the default for invariant and covariant type)
   *
   */
  def _map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }

  /**
   * == Remember ==
   *
   *  flatMap is more powerful than [[map]] and [[map2]].
   *
   *  Both can be implemented in term of [[flatMap]]
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
   * ==Critical Notes==
   *
   *  The recursive function sequence left or right build a data structure recursively.
   *
   *  In nutshell, from a list of function we compose their applications to return a function that returns a list of their result.
   *
   *  What is important to understand here is that, the list that the combinator function returns is specified as a recursive function.
   *
   *  That is the list of results is a recursive function which upon evaluation yield the actual list of result.
   *
   *  Said differently the Sequence Combinator build a recursive function which upon evaluation return a list of result.
   *
   *  That is, it builds a function that recursively apply the functions of its input list and build the list of the result of their application.
   *
   *  -- The execution of each function is happening in the order of the sequence, while the combination of results is in reversed order !!!
   */
  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight(unit[S, List[A]](Nil)) { (a, b) => a.map2(b)((a, b) => a :: b) } // this is a function building, you need to understand that well
  }

  /**
   * == Critical Notes - Via foldRight Raw ==
   *
   * The idiomatic solution expressed as an explicit/equivalent of foldRight
   *
   * ==Critical Notes==
   *
   *  The recursive function sequence left or right build a data structure recursively.
   *
   *  In nutshell, from a list of function we compose their applications to return a function that returns a list of their result.
   *
   *  What is important to understand here is that, the list that the combinator function returns is specified as a recursive function.
   *
   *  That is the list of results is a recursive function which upon evaluation yield the actual list of result.
   *
   *  Said differently the Sequence Combinator build a recursive function which upon evaluation return a list of result.
   *
   *  That is, it builds a function that recursively apply the functions of its input list and build the list of the result of their application.
   *
   *  -- The execution of each function is happening in the order of the sequence, while the combination of results is in reversed order !!!
   *
   */
  def _sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = l match {
    case Nil => unit(Nil)
    case s :: xs => s.map2(sequence(xs))((a, b) => a :: b) // this is a function building, you need to understand that well
  }


  /**
   * == Critical Notes - Via foldLeft ==
   *
   * Need to `reverse` because it is a `foldLeft`, which build the list by reversing the order !!!
   *
   * Note however that I originally reverse before composing. {{{l.reverse.foldLeft}}}
   *
   * We can not do that because we sequence the computation in the reverse order !!!!
   *
   * If we were threading the same input to each function application, that would work,
   * but here the result of a function application is part of the input of the function that follows it.
   *
   * `It is a stateful computation !!!`
   *
   * Another way to put it is: it is particularly wrong in a scenario where a `statefull application/computation`
   * happen while doing the folding (see [[intsLeft]] in purely-functional-state-rng)
   *
   * The proper way with a foldLeft in those scenario is to reverse after.
   *
   * Here we fold into a state action, hence we must map over it to reverse its list
   *
   * {{{l.foldLeft.map}}} (it is just further composing the function)
   *
   * -- The execution of each function and the combination of their results is happening in the order of the sequence !!!
   *
   *
   * == Book Notes: ==
   *
   * We can also write the loop using a left fold.
   * You might think that this is slower than the `foldRight` solution since it
   * walks over the list twice, but it's actually faster! The `foldRight` solution
   * technically has to also walk the list twice, since it has to unravel the call
   * stack, not being tail recursive. And the call stack will be as tall as the list
   * is long.
   *
   */
  def __sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldLeft(unit[S, List[A]](Nil)) { (b, a) => b.map2(a)((b, a) => a :: b) } map (_.reverse) // this is a function building, you need to understand that well
  }


  /**
   * == Critical Notes - Via foldLeft Raw ==
   *
   * -- The execution of each function and the combination of their results is happening in the order of the sequence !!!
   *
   */
  def ___sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {

    @tailrec
    def ___sequenceRec(acc: State[S, List[A]], states: List[State[S, A]]): State[S, List[A]] = states match {

      case Nil => acc map (_.reverse)
      case s::xs => ___sequenceRec(acc.map2(s)((b, a) => a :: b), xs)

      // Interesting the threading order of map2 changed my result order in the right way
      // But this was luck because the functions are the same nextInt
      //case s::xs => ___sequenceRec(s.map2(acc)(_::_), xs)
    }
    ___sequenceRec(unit(Nil), states)
  }


  /**
   * == Sequence via fold Left Raw (More Raw i.e. Bare Bone) ==
   *
   * his implementation uses a loop internally and is the same recursion
   * pattern as a left fold. It is quite common with left folds to build
   * up a list in reverse order, then reverse it at the end.
   * (We could also use a collection.mutable.ListBuffer internally.)
   */
  def sequenceBook[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
      actions match {
        case Nil => (acc.reverse,s)
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) } // this is a function building, you need to understand that well
      }
    State((s: S) => go(s,sas,List()))
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))



}

import State._


import scala.util.chaining.scalaUtilChainingOps

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

val nextIntState = State[RNG, Int](s => s.nextInt)

val listNextInt = List.fill(10)(nextIntState)



{println("Sequence via foldRight")} pipe { _ => sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldRight Raw")} pipe { _ => sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldLeft")} pipe {_ => __sequence(listNextInt).run(SimpleRNG(42)) }

{println("Sequence via foldLeft Raw")} pipe { _ => ___sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldLeft Raw")} pipe { _ => sequenceBook(listNextInt).run(SimpleRNG(42))}


/*sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

def insertCoin: State[Machine, Unit] = State {
  case s@Machine(true, candies, coins) if candies > 0 => () -> s.copy(coins = coins)
  case s@Machine(_, _, _) => () -> s
}

def turn: State[Machine, Int] = ???*/


/**
 * The rules of the machine are as follows:
 *
 *  -- Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
 *
 *  -- Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
 *
 *  -- Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
 *
 *  -- A machine that’s out of candy ignores all inputs.
 */
/*
def simulateMachine(l: List[Input]): State[Machine, (Int, Int)] = {

  l.map{case Coin => insertCoin case Turn => turn}

  ???
}
*/

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(true, candy - 1, coin)
    }

  // modify[Machine] _ compose update = input => modify(update(input))
  // = input => modify[Machine] i.e. ( modify(f: Machine => Machine) )
  // sequence(modify[Machine])
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update) )
    s <- get
  } yield (s.coins, s.candies)

  //simulateMachine(Nil).run(machine)
}

