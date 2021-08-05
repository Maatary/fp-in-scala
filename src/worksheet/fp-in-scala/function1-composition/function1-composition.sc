

val f : String => Int = (s: String) => s.toInt

val g: Int => Int = (i: Int) => i + 1


/**
 *  g compose f = g after f = g . f = x => g(f(x))
 *
 *  f: String => Int
 *  g: Int => Int
 *
 *  g compose f : String => Int
 *
 *  `If a function (g) composes another function (f)`
 *
 *  `Then that function (g) comes after that function (f) it composes`
 *
 */
val gcomposef = g compose f
val gf         = (s: String) => g(f(s))


gcomposef ("2")
gf("2")

assert(gcomposef("2") == gf("2"))

/**
 * f andThen g == g compose f == g after f
 */

val fandTheng = f andThen g

fandTheng("2")

assert(fandTheng("2") == gcomposef("2") )

import cats.syntax.all._

/**
 *  Implementing Compose via flatmap for the exercise
 */
val  gcomposef2 = f.flatMap{fr => {_ => g(fr) } } // g  compose f or g after f

assert(gcomposef2("2") == gcomposef("2") )



