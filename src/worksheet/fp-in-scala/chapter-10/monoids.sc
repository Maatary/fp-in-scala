
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
 *  '''The answer to the question “What is a monoid?” is simply that a monoid is a type, together with the monoid operations and a set of laws.'''
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

  def op(a1: String, a2: String): String = a1 + a2
  val zero: String = ""
}

/**
 * Example of Monoid: List Monoid
 */

def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
  def op(a1: List[A], a2: List[A]) =  a1 ++ a2
  val zero = List.empty[A]
}


val intAddition: Monoid[Int] = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 + a2
  val zero = 0
}

val intMultiplication: Monoid[Int] = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 * a2
  def zero = 1
}

val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  def op(x: Boolean, y: Boolean) = x || y
  val zero = false
}

val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  def op(x: Boolean, y: Boolean) = x && y
  val zero = true
}


/**
 *  == optionMonoid ==
 *
 *  Personal remark: technically it could be said that this combine nothing, but type wise, it just work, see proof below.
 *
 *  ==== Identity law compliance ====
 *
 *  `op(x, zero) == x and op(zero, x) == x`
 *
 *  expands to
 *
 *  `x orElse None == x and None orElse x == x`
 *
 *  ==== Associativity Law compliance ====
 *
 *  x orElse (y orElse z) == (x orElse y) orElse Z
 *
 *  e.g.
 *
 *  None orElse( None orElse Option (1) ) == (None orElse None ) orElse Option (1) == Option (1)
 *
 *  == Notes ==
 *
 *  Notice that we have a choice in how we implement `op`.
 *  We can compose the options in either order. Both of those implementations
 *  satisfy the monoid laws, but they are not equivalent.
 *  This is true in general--that is, every monoid has a _dual_ where the
 *  `op` combines things in the opposite order.
 *
 *  Monoids like `booleanOr` and
 *  `intAddition` are equivalent to their duals because their `op` is commutative
 *  as well as associative.
 *
 */
def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  def op(x: Option[A], y: Option[A]) = x orElse y
  val zero = None
}

//Associativity Law illustrated.
Option(2) orElse( Option(3) orElse Option (1) )
(Option(2) orElse Option(3) ) orElse Option (1)
//Associativity Law illustrated.
None orElse( None orElse Option (1) )
(None orElse None ) orElse Option (1)

/**
 * We can get the dual of any monoid just by flipping the `op`.
 */
def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
  def op(x: A, y: A): A = m.op(y, x)
  val zero = m.zero
}

// Now we can have both monoids on hand:
def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid[A]
def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

/**
 * EndoFunction
 *
 * There is a choice of implementation here:
 * Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
 * We can then get the other one using the `dual` construct (see previous answer).
 */
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(a1: A => A, a2: A => A): A => A = (a:A) => a1(a2(a)) // a1 compose a2
  def zero: A => A = identity[A]
}