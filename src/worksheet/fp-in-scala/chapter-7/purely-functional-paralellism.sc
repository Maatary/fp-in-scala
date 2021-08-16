



/**
 *  = Key Points =
 *
 *
 *  --  Separating the concern of describing a computation from actually running it.
 *
 *
 *  --  We expand from modelling the Results (i.e. Effect) of a Computations to modelling the Computation themselves.
 *
 *
 *  --  Note that in this content IO computation introduce in chapter 13 are just one an example of it.
 *
 *
 *  --  Where we only had the Effect (Result) of a Computation was a Value, now the Computation itself is a value.
 *
 *
 *  --  This is all for referential transparency
 *
 *
 *  --  In this chapter emphasis will be put on algebraic reasoning and the idea that an API can be described by an algebra that obeys specific laws.
 *
 *
 *  === Refresher ===
 *
 *  - `An expression e is referentially transparent if for all programs p all occurrences of e in p can be replaced by the result of evaluating e without affecting the meaning of p.`
 *
 *  - `A function f is pure if the expression f(x) is referentially transparent for all referentially transparent x`.
 *
 *
 *
 */


case class test()


/**
 *  Divide and conquer Sum of a list
 *
 *  -- Side Note: val (ll, lr) = l.splitAt(l.size /  2) is a patten match. Different from multiple assignment in one line notation.
 *
 *  -- Side Note: The function of course is not tail recursive i.e. the recursive is not in last position.
 *
 *  -- '''This implementation can be parallelized. '''
 *
 *
 *  -- ''As we think about what sort of data types and functions could enable parallelizing this computation,
 *  we can shift our perspective. Rather than focusing on how this parallelism will ultimately be implemented
 *  and forcing ourselves to work with the implementation APIs directly (likely related to java.lang.Thread and the java.util.concurrent library),
 *  we’ll instead design our own ideal API as illuminated by our examples and work backward from there to an implementation.''
 *
 */

def sumSplitSequential(l: List[Int]): Int = l match {

  case l if l.size <= 1 => l.headOption getOrElse 0

  case _           =>

    val (ll, lr) = l.splitAt(l.size /  2)

    sumSplitSequential(ll) + sumSplitSequential(lr)

}

sumSplitSequential(List.fill(10)(1) )


/**
 *  = Parallel Computation DataType BASIC =
 *
 *  Just Looking at the line sum(l) + sum(r), which invokes sum on the two halves recursively,
 *  we can see that any data type we might choose to represent our parallel computations
 *  needs to be able to contain a result.
 *  That result will have some meaningful type (in this case Int),
 *  and we require some way of extracting this result.
 *
 *
 *
 *
 */

trait Par[A]

object Par {

  /**
   * Takes an unevaluated A and returns a computation that might evaluate it in a separate thread.
   * We call it unit because in a sense it creates a unit of parallelism that just wraps a single value.
   */
  def unit[A](a: => A): Par[A] = ???

  /**
   * Extract the resulting value from a parallel computation.
   */
  def get[A](par: Par[A]): A = ???

  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = ???

}

def sumPar(l: List[Int]): Par[Int] = l match {

  case l if l.size <= 1 => Par.unit ( l.headOption getOrElse 0 )

  case _           =>

    val (ll, lr) = l.splitAt(l.size /  2)

    Par.map2(sumPar(ll), sumPar(lr)) {_ + _}
}
