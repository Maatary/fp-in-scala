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
    val (n, rng1) = nonNegativeInt(rng)
    (n / (Int.MaxValue.toDouble + 1), rng1)
  }

  def randomPair(rng0: RNG): (Int, Int, RNG) = {
    val (n1, rng1) = rng0.nextInt
    val (n2, rng2) = rng1.nextInt
    (n1, n2, rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
   randomPair(rng) match  {
     case (i1, i2, nRng) => (i1 -> i2.toDouble, nRng)
    }
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    randomPair(rng) match {
      case (i1, i2, nRng) => (i1.toDouble -> i2, nRng)
    }
  }

  def double3(rng0: RNG): ((Double, Double, Double), RNG) = {
    val (i1, rng1) = rng0.nextInt
    val (i2, rng2) = rng1.nextInt
    val (i3, rng3) = rng2.nextInt

    (i1.toDouble, i2.toDouble, i3.toDouble) -> rng3

  }

  def ints(count: Int): (List[Int], RNG) = {
    def intsRec(count: Int, listInt: List[Int], rng: RNG): (List[Int], RNG) = count match {
      case n if n <= 0 => listInt -> rng
      case _ =>
        val (i, rng1) = rng.nextInt
        intsRec(count - 1, i::listInt, rng1)
    }
    intsRec(count, List(), SimpleRNG(42))
  }


}


/** *
 * Difference between a purely functional state approach and the impure appraoch
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
import RNG._

randomPair(rng)

nonNegativeInt(rng)

double0to1(rng)

ints(10)


