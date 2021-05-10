import scala.annotation.tailrec

/**
 * == Notes ==
 *
 * State is just a function S => (A, S).
 *
 * The Combinator on State are combinator of functions.
 *
 * That is, the combinator allows you to combine function application.
 *
 * In other words, they allow you to compose functions into bigger function.
 *
 * This means building a bigger function that apply the input functions in specific way e.g.
 *
 *    -- mapping: where the mapping function is applied to the result of the state action function.
 *
 *    -- sequencing: where the result of the first one is sequenced as input the second one.
 *
 * == Critical: On The meaning Of Sequence ==
 *
 *    -- Building a bigger function means composing a function out of smaller functions.
 *
 *    -- Generally speaking, Composing 2 functions means creating a functions that applies the two inputs functions.
 *
 *    -- The way it is applied (order or inputs) depend of the nature of the composition.
 *
 *    -- Composing two state actions function means creating a function that sequence their application.
 *
 *    -- By sequencing we mean the output of the first function is used as input to the second (see [[map2]] ).
 *
 *    -- For state action building a bigger function is sequencing the application of multiple smaller functions.
 *
 *    -- The key is to understand that we compose the bigger function recursively two functions at a time.
 *
 *    -- This is illustrated in the various '''sequence functions'''  below
 *
 *    -- Where one of the function is the composition of the previous step (expect for the terminal case function).
 *
 *
 */


case class State[S, +A](run: S => (A, S)) {

  /**
   * Map Compose a mapping function f: A => B with the state action function S => (A, S)
   * into a new state action function S => (f(A), S)
   */
  def map[B](f: A => B): State[S, B] = State[S, B] { (s: S) =>
    val (a1, s1) = run(s)
    f(a1) -> s1
  }

  /**
   * '''Map2 Compose 2 state action functions and a mapping function into one state action function''' in which:
   *
   *  -- it sequences the first state action with the second state action
   *
   *  -- '' i.e. the state returned by the first is passed to the second''
   *
   *  -- It returns a pair where:
   *
   *  -- the value is the result of applying the mapping function to both values
   *  returned by the state action function
   *
   *  -- the state is the state returned by the second state action function.
   *
   *
   *  === Observation ===
   *
   *  -- The mapping function can be anything:
   *
   *  -- it could return the 2 values as pair, or one of the value or unit for the matter.
   *
   *  -- So if we are only interested in the value returned by the second state action,
   *     then map2 is turned into a pure sequencing function.
   *
   *  -- the mapping function makes map2 flexible.
   *
   *  === Critical point to Remember ===
   *
   *  -- '''Map2 compose a bigger function out of two smaller functions by sequencing their application'''.
   *
   *
   *
   */
  def map2[B, C](stB: State[S, B])(f: (A, B) => C): State[S, C] = State[S, C] { (s: S) =>
    val (a, sa) = run(s)
    val (b, sb) = stB.run(sa)
    (f(a, b), sb)
  }

  /**
   *  == Critical Reminder ==
   *
   * FlatMap compose '''(i) a state action function State[S, A] ''' with
   * ''' (ii) a combining/sequencing/glue function A -> [S, B] ''' that returns
   * another '''state action function [S, B]'''
   * upon being passed the result '''A''' of the first state action function.
   *
   *
   * The result of it is a bigger function that sequence the two state action function.
   *
   *
   * g(a).run(sa) is important
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
   *  foldRight return a composed function.
   *
   *  It recursively composed the function in the list, 2 function at the time via map2
   *
   *  More specifically, it reach the end of the list, where it reach S => (List(), S)
   *
   *  It then composed that function with the preceding in the stack in a bigger function
   *
   *  That bigger function in turn is composed into a bigger function with the preceding in the stack
   *
   *  This goes on until we reach back the first function in the list.
   *
   */
  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight(unit[S, List[A]](Nil)) { (a, b) => a.map2(b)((a, b) => a :: b) } // this is recursively composed function.
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
    case s :: xs => s.map2(_sequence(xs))((a, b) => a :: b) // this is recursively composed function
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

//val listNextInt = List.fill(1000000)(nextIntState)

val seq = _sequence(List.fill(100000)(nextIntState))

//seq.run(SimpleRNG(42))
/*
{println("Sequence via foldRight")} pipe { _ => sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldRight Raw")} pipe { _ => sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldLeft")} pipe {_ => __sequence(listNextInt).run(SimpleRNG(42)) }

{println("Sequence via foldLeft Raw")} pipe { _ => ___sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldLeft Raw")} pipe { _ => sequenceBook(listNextInt).run(SimpleRNG(42))}*/


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

/*sealed trait Input
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
}*/

//List(1,2,3).foldRight(0)(_ + _)