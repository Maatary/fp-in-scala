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
 *  == Critical: On The StateFull Nature Of State Action Function Composition ==
 *
 *    -- The fact that '''map2''' and '''flatMap with closure''' for state action function '''thread the output of the first function as input to the next''' implies '''Statefulness'''
 *
 *    -- This StateFull Nature of the combinator functions means that the order of execution matter
 *
 *    -- This in turn has implication on function like '''foldLeft''' and ordering see [[__sequence]] which is implemented trough a '''foldLeft'''
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
   * == Important observations ==
   *
   *  -- flapMap as opposed to map2 '''enforce an order of execution''' between
   *     the two state action function trough the '''glue function'''.
   *
   *  -- The second state action function State[S, B] '''is only available when the
   *     glue function g is applied as per g(a) '''
   *
   *  -- At that point the second state action is ran '''g(a).run(sa)'''
   *
   *  -- '''The second state action function can close over the input of the glue function'''
   *
   *  -- '''This is how flatMap can implement map2 or map, it more powerful see [[_map]] and [[_map2]]'''
   *
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
   *  It combines State[S, A] with the glue function that returns unit(f(a)) i.e. constant State[S, B]
   *
   */
  def _map[B](f: A => B): State[S, B] = {
    flatMap { a => unit ( f(a)) } // unit type inferred because of currying of flatMap
  }



  /**
   * == Remember ==
   *
   *  flatMap is more powerful than [[map]] and [[map2]].
   *
   *  Both can be implemented in term of [[flatMap]]
   *
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
    } yield b // map over the result of the second function desugared to unit(f(a)).map(b => b)
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

  /**
   * Lift '''a''' into a state action '''s -> (a, s)''', that return '''a''' as value and maintain the input state '''s'''
   *
   * It allows among other to build neutral state action function, in operation like '''foldLeft''' and '''foldRight'''
   *
   * -- In '''foldLeft''' unit pass along the input state to the composed computation
   *
   * -- In '''foldRight''' unit pass along the returned state of the composed computation
   */
  def unit[S, A](a: A): State[S, A] = State { s => (a, s) }


  /**
   *  == Notes - Sequence Via foldRight ==
   *
   *
   *  '''foldRight''' return a '''composed function'''.
   *
   *  It recursively compose the functions in the list, 2 functions at the time via map2 (or flatMap)
   *
   *  More specifically, it descends to the end of the list, where it reach S => (List(), S)
   *
   *  It then composed that function with the preceding in the stack in a bigger function
   *
   *  That bigger function in turn is composed into a bigger function with the preceding in the stack
   *
   *  This goes on until we reach back the first function in the list.
   *
   *  === On the List of Results Order ===
   *
   *  Our composition returns '''a list of results''', thanks to the mapping function we pass to map2
   *  which combine the results of the application of its 2 inputs function into a list of 2 results.
   *
   *  In other words, each composed function (bigger function), returns the list of results of the two functions being combined.
   *
   *  As explained above the composition of 2 functions at the time happens recursively.
   *
   *  The first combination is between the last function in the list and the terminal case function '''(of foldRight)'''.
   *
   *  '''The terminal case function''' is the first function to return a list and that list is empty i.e. '''unit(Nil)'''.
   *
   *  `Note that Unit(Nil) pass along its input state, ensuring last returned state of the overall composed computation is returned,`
   *  `moreover it provides en Empty List to start combining result in a List of result while coming back up.`
   *
   *  When composed with the last function that like every other returns just a result,
   *
   *  the map function of map2, combine the two results into a list, trough an append operation.
   *
   *  Then we combine up back to the first function, 2 function at the time.
   *
   *  When we reach back the first function, it will be composed with a function that return the list of result of all the other function in the list.
   *
   *  The order of the result is the same as the order of the application of each function (see below)
   *
   *    ==== On composition and ordering ====
   *
   *  '''2 things happens at the same time while coming back up:'''
   *
   *  -- '''The composition of 2 functions i.e. arranging their execution order via map2 ( or flatmap + map with closure )'''
   *
   *  -- '''The building of the result as a list of result which follow the execution order thanks to the Append operation'''
   *
   *   ==== On execution, ordering and stacking - An illustration attempt ====
   *
   *  -- `ComposedFunctionA = call {FunctionA, composedFunctionB } return A::ListResultComposedFunctionB`
   *
   *  -- `composedFunctionB = call {functionB, composedFunctionC } return B::ListResultComposedFunctionC`
   *
   *  -- `.....`
   *
   *  -- `ComposedFunctionLast = call {functionLast, unit(Nil)} return last::List()`
   *
   *
   *
   *  == Specificity of his Implementation ==
   *
   *  -- '''This implementation rely on scala List foldRight, which use a buffer and mutation.'''
   *
   *  -- '''It does not stackoverflow while composing the bigger function'''
   *
   *  == Critical Observation ==
   *
   *  In composing function as such, we are setting ourself for stacking function calls when calling the composed function.
   *
   *  Hence when calling the bigger function, every function is stacked (starting with the bigger function) up to the terminal one.
   *  This cause stack overflow, if the composition is large.
   *
   *  An Attempted illustration
   *
   *  `combine (a, b) = c`
   *
   *  `combine (d, c) = e`
   *
   *  `When we call e, we stack it and then call d and c, then we stack c which call a and b`
   *
   */
  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
    l.foldRight(unit[S, List[A]](Nil)) { (a, b) => a.map2(b)((a, b) => a :: b) }
  }


  /**
   *  == Notes - Sequence Via foldRight Raw ==
   *
   *
   *  '''foldRight''' return a '''composed function'''.
   *
   *  It recursively compose the functions in the list, 2 functions at the time via map2 (or flatMap)
   *
   *  More specifically, it descends to the end of the list, where it reach S => (List(), S)
   *
   *  It then composed that function with the preceding in the stack in a bigger function
   *
   *  That bigger function in turn is composed into a bigger function with the preceding in the stack
   *
   *  This goes on until we reach back the first function in the list.
   *
   *
   *  === On the List of Results Order ===
   *
   *  Our composition returns '''a list of results''', thanks to the mapping function we pass to map2
   *  which combine the results of the application of its 2 inputs function into a list of 2 results.
   *
   *  In other words, each composed function (bigger function), returns the list of results of the two functions being combined.
   *
   *  As explained above the composition of 2 functions at the time happens recursively.
   *
   *  The first combination is between the last function in the list and the terminal case function '''(of foldRight)'''.
   *
   *  '''The terminal case function''' is the first function to return a list and that list is empty i.e. '''unit(Nil)'''.
   *
   *  `Note that Unit(Nil) pass along its input state, ensuring last returned state of the overall composed computation is returned,`
   *  `moreover it provides en Empty List to start combining result in a List of result while coming back up.`
   *
   *  When composed with the last function that like every other returns just a result,
   *
   *  the map function of map2, combine the two results into a list, trough an append operation.
   *
   *  Then we combine up back to the first function, 2 function at the time.
   *
   *  When we reach back the first function, it will be composed with a function that return the list of result of all the other function in the list.
   *
   *  The order of the result is the same as the order of the application of each function (see below)
   *
   *    ==== On composition and ordering ====
   *
   *  '''2 things happens at the same time while coming back up:'''
   *
   *  -- '''The composition of 2 functions i.e. arranging their execution order via map2 ( or flatmap + map with closure )'''
   *
   *  -- '''The building of the result as a list of result which follow the execution order thanks to the Append operation'''
   *
   *   ==== On execution, ordering and stacking - An illustration attempt ====
   *
   *  -- `ComposedFunctionA = call {FunctionA, composedFunctionB } return A::ListResultComposedFunctionB`
   *
   *  -- `composedFunctionB = call {functionB, composedFunctionC } return B::ListResultComposedFunctionC`
   *
   *  -- `.....`
   *
   *  -- `ComposedFunctionLast = call {functionLast, unit(Nil)} return last::List()`
   *
   *
   *
   *  == Specificity of his Implementation ==
   *
   *  -- '''As the Raw version this implementation is explicitly non-tail recursive'''
   *
   *  -- '''It does stackoverflow while composing the bigger function'''
   *
   *  -- '''If the list of functions is large, the composition itself will stackoverflow before building the bigger function'''
   *
   *  == Critical Observation (Same as above) ==
   *
   *  In composing function as such, we are setting ourself for stacking function calls when calling the composed function.
   *
   *  Hence when calling the bigger function, every function is stacked (starting with the bigger function) up to the terminal one.
   *  This cause stack overflow if the composition is large.
   *
   *  An Attempted illustration
   *
   *  `combine (a, b) = c`
   *
   *  `combine (d, c) = e`
   *
   *  `When we call e, we stack it and then call d and c, then we stack c which call a and b`
   *
   */
  def _sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = l match {
    case Nil => unit(Nil)
    case s :: xs => s.map2(_sequence(xs))((a, b) => a :: b)
  }


  /**
   * == Critical Notes - Via foldLeft ==
   *
   * see [[sequence]] and [[_sequence]] which implement the '''foldRight''' approach
   * for an overall understanding of how the composition works.
   * Here we simply highlight the contrast of implementing trough '''foldLeft'''
   *
   *
   * As apposed to the composing order in foldRight approach, here the composition of function
   * 2 function at the time is happening from left to right. the bigger function is on the left.
   *
   * That is, we build a bigger function from left to right.
   *
   * We start with '''Unit(Nill)''' which is composed with the first function on the list.
   *
   * `Unit(Nil) will simply pass along the input state, and provide en Empty List to start combining result in a List of result. `
   *
   * This gives a bigger function which is the composed with the next function in the list.
   *
   * This goes on, 2 functions at the time, until we reach the end of the List.
   *
   * === On the List of Results Order ===
   *
   *
   * As in '''foldRight''' the execution order of the composition goes from left to right.
   * `However the stacking at execution is triggered by the left operand of the bigger function`.
   * The opposite of what happens when composing with the '''foldRight'''.
   *
   * `The output list of results of the final composed function is set up in the reverse order of the execution of the functions`
   *
   * That is, as we descend the list, each composed function returns as result a list of results, made of the results of the two functions being composed.
   * More specifically, each of the function visited has his result appended to the result of the incoming composed function.
   * Unit(Nil) represents the first composed function. It passes along the input state and return an empty List.
   *
   *
   *    ==== On composition and ordering ====
   *
   *  '''2 things happens at the same time while descending:'''
   *
   *  -- '''The composition of 2 functions i.e. arranging their execution order via map2 ( or flatmap + map with closure )'''
   *
   *  -- '''The building of the result as a list of results which is in the reverse of the function execution order,
   *        because of the Append operation happening while descending starting with the initial Unit(Nil)'''
   *
   *
   *   ==== On execution, ordering and stacking - An illustration attempt ====
   *
   *  -- `ComposedFunctionFinal = call {composedFunctionFinal-1, FunctionLast } return  ResultFunctionLast::ListResultComposedFunctionFinal-1`
   *
   *  -- `composedFunctionFinal-1 = call {composedFunctionFinal-2, FunctionFinal-1 } return ResultFunctionFinal-1::ListResultComposedFunctionFinal-2`
   *
   *  -- `.....`
   *
   *  -- `ComposedFunctionInit = call {unit(Nil), FunctionFirst} return FunctionFirst::List()`
   *
   *
   *  -- So ultimately when running the fully composed function the first function to be executed is Unit(Nil), then the first in the List and so on.
   *
   *  == Critical Observation On the Implementation Accounting For The Statefulness Nature of State Action Function Composition ==
   *
   * Given the above explanation, the result list need to be '''reversed''' to put it in the same order as the function execution order.
   *
   * Note that the reversing must happen when we are done.
   *
   * The idiomatic foldLeft ordering trick {{{l.reverse.foldLeft}}} can't be used because we are dealing with stateful computations i.e. the order matter.
   *
   * '''Indeed, when we compose 2 functions with `map2`` or `flatMap with closure` the input of the second function is the output of the first !!!!'''
   *
   *
   *  === Additional Observation ===
   *
   * If we were threading the same input to each function application, that would work,
   * but here the result of a function application is part of the input of the function that follows it.
   *
   * `It is a stateful computation !!!`
   *
   * Another way to put it, is: it is particularly wrong in a scenario where a `stateful application/computation`
   * happen while doing the folding (see [[intsLeft]] in purely-functional-state-rng)
   *
   * The proper way with a foldLeft in those scenario is to reverse '''after'''.
   *
   * Here we fold into a state action, hence we must map over it to reverse its list
   *
   * {{{l.foldLeft.map}}} (it is just further composing the function)
   *
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
    l.foldLeft(unit[S, List[A]](Nil)) { (b, a) => b.map2(a)((b, a) => a :: b) } map (_.reverse)
  }


  /**
   * == Critical Notes - Via foldLeft Raw ==
   *
   */
  def ___sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = {

    @tailrec
    def ___sequenceRec(acc: State[S, List[A]], states: List[State[S, A]]): State[S, List[A]] = states match {

      case Nil => acc map (_.reverse)
      case s::xs => ___sequenceRec(acc.map2(s)((b, a) => a :: b), xs)

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
        case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
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

//val listNextInt = List.fill(100000)(nextIntState)

/** foldRight composition, will stack overflow while composing the function see [[_sequence]] */
//val seqRight = _sequence(listNextInt)

/** Will not stack overflow on composition (we accumulate the composition) **/
//val SeqRightScala = sequence(listNextInt) //which use mutation and buffer so as a foldLeft
/** Stack overflow at execution in any case because the composition stack calls see [[sequence]] and [[_sequence]].*/
//SeqRightScala.run(SimpleRNG(42))

//seq.run(SimpleRNG(42))
/*
{println("Sequence via foldRight")} pipe { _ => sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldRight Raw")} pipe { _ => sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldLeft")} pipe {_ => __sequence(listNextInt).run(SimpleRNG(42)) }

{println("Sequence via foldLeft Raw")} pipe { _ => ___sequence(listNextInt).run(SimpleRNG(42))}

{println("Sequence via foldLeft Raw")} pipe { _ => sequenceBook(listNextInt).run(SimpleRNG(42))}*/



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


//TODO revisit that one day
//modify[Machine] _  compose Candy.update

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

  //modify[Machine] _  = eta expansion of modify[Machine] so we get
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs map (modify[Machine] _ compose update))
    s <- get
  } yield (s.coins, s.candies)
}