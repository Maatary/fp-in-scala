
import cats.syntax.all._


/**
 * =Some Monad Laws=
 *
 */


/**
 * ==Right Identity==
 */
val rightIdentity = Option(2).flatMap(_.pure[Option]) === Option(2)


/**
 * ==Left Identity==
 */
def f: Int => Option[Int] = int => Option(2 * int)

val leftIdentity = Option(2).flatMap(f) === f(2)

/**
 * ==Associativity==
 *
 * === In Nutshell: ===
 *
 *  - '''flatMap''' sequence computation.
 *
 *  - Therefore so long as the order of the sequence remains the same we get the same result
 *
 *  - Assembling that sequencing starting from the left or the right does not matter.
 *
 *  - The only part that might confuse is the appearance of a '''lambda''' in '''the first flatMap'''
 *
 *  - But that is just nesting, it is exactly what we do in for-comprehension which allows closure.
 *
 *  `Note that `
 *
 *  {{{
 * Option(2).flatMap(g).flatMap(h) ==
 *                                  Option(2).flatMap(x => g(x)).flatMap(x => h(x))
 *  }}}
 *
 *
 * - Lambda is always there, we just need to make it explicit when we nest
 *
 */
def g: Int => Option[Int] = int => Option(2 * int)
def h: Int => Option[Int] = int => Option(4 * int)


val associativity = Option(2).flatMap(g).flatMap(h) === Option(2).flatMap(x => g(x).flatMap(h))



