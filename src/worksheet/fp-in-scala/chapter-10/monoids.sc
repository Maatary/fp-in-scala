
/**
 *
 * = Monoids =
 *
 * We’ll see how monoids are useful in two ways:
 * they facilitate parallel computation by giving us the freedom to break our problem into chunks that can be computed in parallel;
 * and they can be composed to assemble complex calculations from simpler pieces.
 *
 *
 * == Introduction ==
 *
 * Let’s consider the algebra of string concatenation.
 * We can add "foo" + "bar" to get "foobar", and the empty string is an identity element for that operation.
 * That is, if we say (s + "") or ("" + s), the result is always s.
 * Furthermore, if we combine three strings by saying (r + s + t), the operation is associative—it doesn’t matter whether we parenthesize it: ((r + s) + t) or (r + (s + t)).
 *
 * The exact same rules govern integer addition. It’s associative, since (x + y) + z is always equal to x + (y + z), and it has an identity element, 0, which “does nothing” when added to another integer.
 * Ditto for multiplication, whose identity element is 1.
 * The Boolean operators && and || are likewise associative, and they have identity elements true and false, respectively.
 *
 * == Corollary ==
 *
 *
 * The term for this kind of algebra is monoid.
 * The laws of associativity and identity are collectively called the monoid laws.
 * A monoid consists of the following:
 *
 *  - Some type A
 *  - An associative binary operation, op, that takes two values of type A and combines them into one: op(op(x,y), z) == op(x, op(y,z)) for any choice of x: A, y: A, z: A
 *  - A value, zero: A, that is an identity for that operation: op(x, zero) == x and op(zero, x) == x for any x: A
 *
 *  == Notes ==
 *
 *  '''The answer to the question “What is a monoid?” is simply that a monoid is a type, together with the monoid operations and a set of laws. '''
 *  '''A monoid is the algebra, and nothing more.'''
 *
 */

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

/**
 * Example of Monoid: String Monoid
 */

val stringMonoid: Monoid[String] = new Monoid[String] {

  override def op(a1: String, a2: String): String = a1 + a2

  override def zero: String = ""
}

/**
 * Example of Monoid: List Monoid
 */

def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
  override def op(a1: List[A], a2: List[A]) =  a1 ++ a2

  override def zero = List.empty[A]
}