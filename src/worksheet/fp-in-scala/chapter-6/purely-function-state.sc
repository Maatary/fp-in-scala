

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
   * Tail recursion
   *
   * We compute and pass the state to the next computation in the recursion
   * i.e carry the state
   * As in a Fold left it reverse the order of things
   * Indeed the first computation are added first in the list
   */
  def intsLeft(count: Int)(rng: RNG): (List[Int], RNG) = {

    @tailrec
    def intRec(count: Int)(rng: RNG)(l: List[Int]): (List[Int], RNG) = count match {
      case count if count <= 0 => (l, rng)
      case count if count > 0 =>
        val (i, rng1) = rng.nextInt
        intRec(count - 1)(rng1)(i::l)
    }

    intRec(count)(rng)(List())
  }

  /**
   * Non-Tail Recursive
   *
   * The state is computed trough the stack
   *
   * Recursive call simply go down the recursion without computing anything
   * Along the way we stack the operation that needs to happen upon coming back up
   * We take the result of the termination case,
   * compute the next int, build the result by concatenating the int, and passing the next rng
   *
   * Interestingly it behave more like a foldLeft because the first computation are done while coming back up.
   *
   * See next
   *
   * You should generate as you go down, and the concatenation as you come back up.
   * Indeed FoldRight always `combine` while coming back up but do the operation on the operand while going down,
   * that's what preserve the order.
   * Here it particularly matters because rng.next is stateful
   */
  def intsRight(count: Int)(rng: RNG): (List[Int], RNG) = count match {

    case count if count <= 0 => (List(), rng)
    case count if count > 0 =>
      val (li, r) = intsRight(count - 1)(rng)
      val (iNext, rNext) = r.nextInt
      (iNext::li, rNext)
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
   * Looking back at our implementations, we’ll notice a common pattern:
   * each of our functions has a type of the form RNG => (A, RNG) for some type A.
   * Functions of this type are called state actions or state transitions
   * because they transform RNG states from one to the next.
   *
   * These state actions can be combined using combinators,
   * which are higher-order functions that we’ll define in this section.
   *
   * Since it’s pretty tedious and repetitive to pass the state along ourselves,
   * we want our combinators to pass the state from one action to the next automatically.
   */

  /**
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
   *
   */

  type Rand[+A] = RNG => (A, RNG)

  def int: Rand[Int] = _.nextInt // e => e.nextInt

  /**
   * A simple RNG state transition is the unit action, which passes the RNG state through without using it,
   * always returning a constant value rather than a random value.
   *
   * First example of think of a function as a Value.
   * Remember that in lambda calulus, everything is an expression, and irreducible expression are called Value
   * A function, is just an expression that can be further evaluated, or a deferred value
   * In other words, you return an expression that need to be further evaluated to get to its irreducible form.
   *
   */
  def unit[A](a: A): Rand[A] = Rng => (a, Rng)


  /**
   * Another RNG state transition is map,
   * it transforms the output of a state action without modifying the state itself.
   *
   * Feels more like a state transition combinator
   *
   * Remember, Rand[A] is just a type alias for a function type RNG => (A, RNG)
   * We could also say Rand[A] is an expression type, or more specifically, a function expression type.
   *
   * so this is just a kind of function composition.
   *
   * In fact generally speaking, one can say that mapping over a function, is function composition,
   * when the output of the original function becomes the input the function use to map over.
   *
   * Mapping over a function can be translated as such f map g = g(f)
   *
   */
  def map[A, B](rand: Rand[A])(f: A => B): Rand[B] = rng0 => {
    val (i1, rng1) = rand(rng0)
    (f(i1), rng1)
  }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))


  def map2[A, B, C](rand1: Rand[A], rand2: Rand[B])(f:(A, B) => C): Rand[C] = rng => {
    val (i1, r1) = rand1(rng)
    val (i2, r2) = rand2(r1)
    (f(i1, i2), r2)
  }

  def both[A, B](rand1: Rand[A], rand2: Rand[B]): Rand[(A,B)] = {
    map2(rand1, rand2)((_,_))
  }

  def randIntDouble: Rand[(Int, Double)] = both[Int, Double](int, double)

  def randDoubleInt: Rand[(Double, Int)] = both(double, int)

  /**
   * Sequence for combining a List of transitions into a single transition.
   *
   * One can think of it as a function that compose a list of state actions function into one giant state action function
   *
   * The combination coming back up is just running map2 where the second rand return a (list[A], RNG2) and the first (A, RNG1)
   * then we concatenate (a::list[A], RNG2) which result in rng => (a::list[A], RNG2)
   *
   * returns:
   *
   * rng => (List[A], RNG)
   *
   */

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit(Nil) //rng => (List(), rng)
    case s::xs => map2(s, sequence(xs))(_::_)
  }

  def sequenceViaFoldRight[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil:List[A])){(a, b) => map2(a, b)(_::_)}
  }

  /**
   * Same Thing, the rng parameter is just managed explicitly but there is No need
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
   * g(i)(r) is important
   * g(i) is Rand[B], but your function already define the input parameter rng
   * hence to recover Rand[B], you need to apply r, as in g(i)(r) which yield a (B, RNG)
   * hence rng => (B, RNG)
   * This falls in the pattern of composing function. add a new parameter, and apply the internal function to recover
   * the function you want at this. you get a function that will apply the function you compose,
   * by specifying how those internals will be applied to the outter new parameter. (to be re-written)
   */
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (i, r) = f(rng)
    g(i)(r) // We pass the new state along
  }

  /**
   * Example of Using flatMap Recursively.
   */
  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  def mapViaFlatMap[A, B](rand: Rand[A])(f: A => B): Rand[B] = {
    flatMap(rand){a => unit(f(a))}
  }

  /**
   * You re-implemented map with the second flatMap. There was no need.
   * See next (Implementation by the author)
   */
  def map2ViaFlatMap[A, B, C](rand1: Rand[A], rand2: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(rand1){a => flatMap(rand2){b => unit(f(a,b))}}
  }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra){a => map(rb)(b => f(a, b))}

}

import RNG._

nonNegativeInt _



/** *
 * Difference between a purely functional state approach and the impure approach
 */

val sRng = new Random()
// sRng does mutate under the hood
sRng.nextInt()
sRng.nextInt()


val rng = SimpleRNG(42)
// rng does not mutate.
rng.nextInt
rng.nextInt

/** *
 * Playing around: a random pair generator
 */


/**
 *  Implemented Method
 */


/*randomPair(rng)

nonNegativeInt(rng)

double0to1(rng)*/

intsLeft(10)(rng)
intsRight(10)(rng)
intsRightBook(10)(rng)

//sequence(List(unit(1), unit(2), int))(rng)

val unit2 = unit(2)



