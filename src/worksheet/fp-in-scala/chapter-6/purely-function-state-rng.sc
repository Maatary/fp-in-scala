

import scala.annotation.tailrec
import scala.util.Random

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

object RNG {

  /**
   * We need to be quite careful not to skew the generator.
   * Since `Int.MinValue` is 1 smaller than `-(Int.MaxValue)`,
   * it suffices to increment the negative numbers by 1 and make them positive.
   * This maps Int.MinValue to Int.MaxValue and -1 to 0.
   */

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  /**
   * We generate an integer >= 0 and divide it by one higher than the maximum.
   * This is just one possible solution.
   */
  def double0to1(rng: RNG): (Double, RNG ) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def randomPair(rng0: RNG): (Int, Int, RNG) = {
    val (n1, rng1) = rng0.nextInt
    val (n2, rng2) = rng1.nextInt
    (n1, n2, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {

    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)

  }


  /**
   * == Tail recursion ==
   *
   * We compute and pass the state to the next computation in the recursion
   * i.e carry the state
   *
   * As in a foldLeft it reverses the order of things.
   * The first computation are added first in the list
   * and therefore found at the end of the list when done i.e List(randX, rand3, rand2, rand1, rand0)
   *
   * == Critical Point ==
   *
   * The computation however are still done in the right order, the recomposition of the list is reversed.
   * Hence we only need to reverse the list to get what we want.
   *
   * (l.reverse, rng)
   *
   * This is raw implementation of foldleft behavior shows you how to reverse at the end of the recursion i.e.
   * at the `termination` point/case
   *
   *  == On Statefull Computation implication ==
   *
   * In a foldLeft, which is a function you can't modify,
   * the only option would have been to reverse when the foldLeft is done i.e. l.foldLeft.reverse
   *
   * Not before as in l.reverse.foldLeft whenever `stateful computation` happen while going down
   * Because that would change the statefull computation, the resulting state and computation sequence would be wrong.
   *
   */
  def intsLeft(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def intRec(count: Int)(rng: RNG)(l: List[Int]): (List[Int], RNG) = count match {
      case count if count <= 0 => (l.reverse, rng)
      case _ =>
        val (i, rng1) = rng.nextInt
        intRec(count - 1)(rng1)(i::l)
    }

    intRec(count)(rng)(List())
  }

  /**
   * == Non-Tail Recursive ==
   *
   * The combine operation is computed trough the stack
   *
   * The combine operation is the one that trigger the recursive call,
   * as it needs one of its operand to be computed first.
   *
   * The Recursive call go down the recursion while computing the operation that need to happen on the element visited.
   *
   * Along the way we stack the combine operation that happens upon coming back up.
   *
   *   -- Here the element is the counter i.e. count and the operation is rng.nextInt
   *
   *   -- The computation on the element visited (the left operand) is (iNext, rNext) = rng.nextInt
   *
   *   -- The recursion call is (li, r) = intsRight(count - 1)(rNext)
   *
   *   -- The combine operation is (iNext::li, r)
   *
   *
   * Note that this case is peculiar in that because of the nature of the state action that returns a pair
   * all intermediary result must be made explicit as above to be passed along and combine properly.
   *
   *   -- first state return i.e. RNext is used in the recursive call,
   *   and the first result iNext and the state return from the recursion are used in the combine operation.
   *
   *   -- Usually the combine calls the recursion directly as well as the computation on the element visited
   *   e.g. combine ( f(a), RecursiveCall(, , b) )
   *
   *   -- Note exactly True to be revisited ** Actually the other scenario where we typically diverge from the usual as above is on lazy Stream
   *   where the by-name parameter is returned, but where also we can short circuit things,
   *   based on the result of the first (left) operand **
   *
   *   -- Read more notes in [[intsRightBook]]
   *
   *
   * == Note from previous work ==
   *
   * You should generate as you go down, and the concatenation as you come back up.
   * Indeed FoldRight always `combine` while coming back up but do the operation on the operand while going down,
   * that's what preserve the order.
   * Here it particularly matters because rng.next is stateful
   */
  def intsRight(count: Int)(rng: RNG): (List[Int], RNG) = count match {

    case count if count <= 0 => (List(), rng)
    case count if count > 0 =>
      val (iNext, rNext) = rng.nextInt
      val (li, r) = intsRight(count - 1)(rNext)
      (iNext::li, r)
  }

  /**
   * In this approach we compute the next rng while we go down
   * So the only operation that is happening while coming back up,
   * is the concatenation and the assignment of the new state
   * This feels more like a fold right
   * The first computation are added last so we maintain the order
   * It is done in two step because of the random generation that generate to values,
   * which needs to be dealt with separately.
   */
  def intsRightBook(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count <= 0)
      (List(), rng)
    else {
      val (x, r1)  = rng.nextInt
      val (xs, r2) = intsRightBook(count - 1)(r1)
      (x :: xs, r2)
    }

  /**
   * == Reflection ==
   *
   * Looking back at our implementations, we’ll notice a common pattern:
   * each of our functions has a type of the form RNG => (A, RNG) for some type A.
   * Functions of this type are called state actions or state transitions
   * because they transform RNG states from one to the next.
   *
   * These state actions can be combined using combinators,
   * which are higher-order functions that we’ll define in this section.
   *
   * == Important Note ==
   *
   * What does it means to combine function exactly ?
   *
   *  -- It means composing functions
   *
   *  -- So combining state action means composing them into one bigger function
   *
   *  -- It means creating a function that compose (reuse in a way the original function being combined)
   *
   *  -- Bottom line is the result of the composition is a function (no matter how sophisticated the type is written)
   *
   *  -- We RETURN A FUNCTION
   *
   *
   * == State Action Function Type Definition ==
   *
   * Since it’s pretty tedious and repetitive to pass the state along ourselves,
   * we want our combinators to pass the state from one action to the next automatically.
   *
   * To make the type of actions convenient to talk about, and to simplify our thinking about them,
   * let’s make a type alias for the RNG state action data type:
   *
   * We can understand it as a Randomly Generated A  - a State Action -
   *
   * A program that depends on some RNG, uses it to generate an A,
   * and also transitions the RNG to a new state that can be used by another action later.
   *
   * We want to write combinators that let us combine Rand actions while avoiding explicitly passing along the RNG state.
   *
   * The important thinking shift here, is that we will now really think of a function as a `Value` i.e. a Value of Type Rand[A]
   */
  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = _.nextInt // e => e.nextInt

  /**
   * == Important note ==
   *
   * A simple RNG state transition is the unit action, which passes the RNG state through without using it,
   * always returning a constant value rather than a random value. It is a peculiar case because it is Unit.
   *
   *  -- **abusively** we can say that Unit combine a constant and a Rand Function into a Rand that always return a constant value
   *
   *  -- More accurately the combinator create a Rand out of a constant function
   *
   *  -- It creates a Rand that reuse the constant function
   *
   *  -- Maybe we can say it combine a constant value into a Rand that returns a constant value
   *
   * == Note on Everything is an Expression to think of a function as a value ==
   *
   * First example of thinking of a function as a Value.
   * Remember that in lambda calculus, everything is an expression, and irreducible expression are called Value
   * A function, is just an expression that can be further evaluated, or a deferred value
   * In other words, you return an expression that need to be further evaluated to get to its irreducible form.
   *
   */
  def unit[A](a: A): Rand[A] = Rng => (a, Rng)


  /**
   * Another RNG state transition is map,
   * it transforms the output of a state action without modifying the state itself.
   *
   *  -- It combines/compose a Rand and a function f: A => B
   *
   *  -- It create a function that reuse the Rand and f being combined/composed
   *
   *  -- It is function composition
   *
   * In fact generally speaking, mapping over a function, is function composition,
   * when the output of the original function becomes the input the function used to map with.
   *
   * Mapping over a function can be translated as such f map g = g(f)
   *
   */
  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] = rng0 => {
    val (i1, rng1) = rand(rng0) // reuse rand to get the intermediary result
    (f(i1), rng1) // reuse f and build the expected result
  }

  /**
   *  Reuse nonNegativeInt: Rand[Int]
   *
   *  Reuse Even f: Int => Int
   *
   *  Create a function that return nonNegativeEven: Rand[Int]
   */
  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)


  def double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))


  /**
   * == Notes: ==
   *
   * Combine tow function into one function that returns
   *
   *    -- the combined/map result of the two functions as per the map function f
   *
   *    -- the generator state as returned by the last function executed
   *
   * Reuse Rand[A]
   *
   * Reuse Rand[B]
   *
   * Reuse f: (A,B) => C
   *
   * Create Rand[C] = rng => (C, RNG) where C = f(a,b)
   *
   * == Side Node ==
   *
   * Rand[A] = RNG => (A, RNG)  this can be seen as  RNG => F[A, RNG] where F is Tuple[A, B] type Constructor
   *
   * In other words, we are combining `effectful` computation
   */
  def map2[A, B, C](rand1: Rand[A], rand2: Rand[B])(f:(A, B) => C): Rand[C] = rng => {
    val (i1, r1) = rand1(rng)
    val (i2, r2) = rand2(r1)
    (f(i1, i2), r2)
  }

  /**
   * Both Combine two Rand into a Rand that produce a Pair consisting of the result of the first and second Rand
   * and the state of the generator of the last Rand executed.
   * It uses it map2 for this, with the map function f that return a pair out of its two parameters.
   */
  def both[A, B](rand1: Rand[A], rand2: Rand[B]): Rand[(A,B)] = {
    map2(rand1, rand2)((_,_))
  }

  def randIntDouble: Rand[(Int, Double)] = both[Int, Double](int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
   * Fundamentally it is just taking a list of function and combining them into a function that return a list of result.
   * In other words that function being built reuse all the functions by applying them (with some sequencing in the process)
   * and combining all those results into a list.
   *
   * Sequence for combining a List of transitions into a single transition.
   *
   * One can think of it as a function that compose a list of state actions function into one giant state action function
   *
   * The combination coming back up is just running map2 where the second Rand return a (list[A], RNG2) and the first (A, RNG1)
   * then we concatenate (a::list[A], RNG2) which result in rng => (a::list[A], RNG2)
   *
   * returns:
   *
   * rng => (List[A], RNG)
   *
   */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit(Nil) //rng => (List(), rng) Build an Rand returning a constant value
    case s::xs => map2(s, sequence(xs))(_::_) //Combine by applying the parameter functions (see map2)
  }

  def sequenceViaFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil:List[A])){(a, b) => map2(a, b)(_::_)}
  }

  /**
   * This was my original implementation
   *
   * - Here, the rng parameter is managed explicitly but there is No need (see [[sequence]])
   */
  def sequenceOriginal[A](l: List[Rand[A]]): Rand[List[A]] = rng => l match {
    case Nil   =>  (List[A](), rng) ////unit(List()[A])(rng)
    case s::xs => map2(s, sequenceOriginal(xs))(_::_)(rng)
  }

  /**
   * From Book:
   *
   * It's interesting that we never actually need to talk about the `RNG` value
   * in `sequence`. This is a strong hint that we could make this function polymorphic in that type.
   *
   */
  def intsViaSequence(count: Int): Rand[List[Int]] = {
    val rands = List.fill(count)(int)
    sequence(rands)
  }

  /**
   * == Notes: ==
   *
   * g(i)(r) is important
   * g(i) is Rand[B], but your function already define the input parameter rng
   * hence to recover Rand[B], you need to apply r, as in g(i)(r) which yield a (B, RNG)
   * hence rng => (B, RNG)
   *
   * This falls in the pattern of composing function. add a new parameter, and apply the internal function to recover
   * the function you want at this. you get a function that will apply the function you compose,
   * by specifying how those internals will be applied to the outer new parameter. (to be re-written)
   *
   * == Book Notes: ==
   *
   * flatMap allows us to generate a random A with Rand[A], and then take that A and choose a Rand[B] based on its value.
   *
   * In nonNegativeLessThan, we use it to choose whether to retry or not, based on the value generated by nonNegativeInt (see [[nonNegativeLessThan]]).
   *
   * == Further Notes: ==
   *
   * There is a rng passing/sequencing happening in the process.
   *
   * In other words, like [[map2]] [[flatMap]] allows to sequence the rng for us.
   *
   * Note that this is not passing the original rng to each fip (function input parameter),
   * but passing the rng producing by the first Rand when applied to the outer rng, and passing it along to the second Rand.
   *
   * It is critical to remember that we're combining 2 fips into a function that reuse/apply those 2 fips, hence the automated sequencing.
   *
   * the difference with [[map2]] is that [[flatMap]] sequence the function execution according to the order provided by the user.
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (i, r) = f(rng)
    g(i)(r) // We pass the new state along
  }

  /**
   * Example of Using flatMap Recursively.
   *
   * == Book Notes: ==
   *
   * flatMap allows us to generate a random A with Rand[A], and then take that A and choose a Rand[B] based on its value.
   *
   * In nonNegativeLessThan, we use it to choose whether to retry or not, based on the value generated by nonNegativeInt (see [[nonNegativeLessThan]]).
   */
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }


  /**
   * Unit lift A into a constant Rand[A]
   * Hence we use it to building a constant Rand[f(a)] which is Rand[B]
   */
  def mapViaFlatMap[A, B](randA: Rand[A]) (f: A => B): Rand[B] = {
    flatMap (randA) {a => unit(f(a))}
  }

  def map2ViaFlatMap[A, B, C](rand1: Rand[A], rand2: Rand[B])(f:(A, B) => C): Rand[C] = {

    flatMap(rand1){a => map(rand2) {b => f(a, b)}}

    //flatMap(rand1){a => flatMap(rand2) {b => unit(f(a,b))}}

    /*for {
      rng1 <- rand1
      rng2 <- rand2
    } yield f(rng1, rng2)*/
  }

}

import RNG._

nonNegativeInt _




/** *
 * Difference between a purely functional state approach and the impure approach
 */

// Note: the method without constructor pick the seed unlikely to be the same as the previous invocation

// sRng does mutate under the hood
val sRng = new Random()
sRng.nextInt()
sRng.nextInt()


// rng does not mutate because nextInt has the type () => (nextInt, nextRNG)
// rng stays unchanged
// See [[[SimpleRNG]]]
// nextInt is pure i.e. referentially transparent i.e. same input lead to same output
// i.e. expression reducible to a value
// The value of the Seed makes the state of SimpleRNG
// Same Seed, means same output when the function is referentially transparent
val rng = SimpleRNG(42)
rng.nextInt
rng.nextInt


/**
 * Playing around: a random pair generator
 */

/*randomPair(rng)*/

/**
 * Implemented Method evaluation
 */


/*
nonNegativeInt(rng)
double0to1(rng)
*/

intsLeft(10)(rng)
intsRight(10)(rng)
intsRightBook(10)(rng)

//sequence(List(unit(1), unit(2), int))(rng)

val unit2 = unit(2)



implicitly[Rand[_] =:= Function1[RNG, Tuple2[_, RNG]]]