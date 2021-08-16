



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


/**
 *  == BadSumPar: What's wrong with it ? ==
 *
 *  We have a choice about the meaning of '''unit''' and '''get'''.
 *
 *  -- (1) '''unit''' could begin evaluating its argument immediately in a separate (logical) thread
 *
 *  -- (2) unit could simply hold onto its argument until '''get''' is called and begin evaluation then.
 *
 *
 *  === (2) Unit hold onto its arguments -> Sequentialization due to Scala eager parameter evaluation Order ===
 *
 *  Function arguments in Scala are strictly evaluated from left to right,
 *  so if unit delays execution until get is called,
 *  we will both spawn the parallel computation and wait for it to finish before spawning the second parallel computation.
 *  This means the computation is effectively sequential!
 *
 *  === (1) unit evaluates its argument immediately -> it breaks referential transparency  ===
 *
 *  If unit evaluates its argument concurrently, then calling get arguably breaks referential transparency.
 *  We can see this by replacing sumL and sumR with their definitions
 *
 *  {{{Par.get(Par.unit(sum(l))) + par.get(Par.unit(sum(r)))}}}

 *
 *  if we do so, we still get the same result, but our program is no longer '''parallel'''
 *
 *
 *  In other words if we replace SumR or SumL by their values i.e. Par.get(par.unit(sum(x))) the meaning of the program changes.
 *  We go back to be sequential.
 *
 *  `The key here is to understand that with eager evaluation at unit, the expression change the state of the world.
 *  So depending on where you put that expression the meaning of the program changes. In this case the consequence is big,
 *  as we go back to sequentiality. What we need is a description of the action that change the state of the  world, and evaluate it
 *  ourself when ready.`
 *
 *  If unit starts evaluating its argument right away,
 *  the next thing to happen is that get will wait for that evaluation to complete.
 *  So the two sides of the + sign won’t run in parallel if we simply inline the sumL and sumR variables.
 *
 *   === (1) and (2) do not handle side effecting code well ===
 *
 *  We can see that unit has a definite side effect, but only with regard to get.
 *  That is, unit simply returns a Par[Int] in this case, representing an asynchronous computation.
 *  But as soon as we pass that Par to get, we explicitly wait for it, exposing the side effect.
 *
 *  'The key point in all and all with both solution, is that they do not handle structurally the fact that a parallel computation
 *  is an IO Action or a side effectful Action, that is, an Action that affect the state of the world.
 *  It is not a mere calculus of value, it changes the state of the word, where here, it changes the state of the computation'
 *
 *
 *  === Conclusion: What is needed ===
 *
 *  So it seems that we want to avoid calling get, or at least delay calling it until the very end.
 *  We want to be able to combine asynchronous computations without waiting for them to finish.
 *
 */

def badSumPar(l: List[Int]): Int = l match {

  case l if l.size <= 1 => l.headOption getOrElse 0

  case _           =>

    val (ll, lr) = l.splitAt(l.size /  2)

    val sumL = Par.unit(badSumPar(ll))

    val sumR = Par.unit(badSumPar(lr))

    Par.get(sumL) + Par.get(sumR)

}


/**
 *  == SumPar: What's right about it ? ==
 *
 *  Here our function is referentially transparent. No side effect are executed i.e. Par is not executed.
 *  Indeed our function returns a Par i.e. a computation, not the result of the computation  i.e. Int here.
 *
 *  However now the problem is shifted to map2. It all comes down to how map2 will combine the 2 parallel computations.
 *
 *  === If map2 argument are strict - stack trace ===
 *
 *  {{{
 *
 *    sum(IndexedSeq(1,2,3,4))
 *
 *    map2(sum(IndexedSeq(1,2)), sum(IndexedSeq(3,4)))(_+_)
 *
 *    map2(
 *         map2(sum(IndexedSeq(1)), sum(IndexedSeq(2)))(_+_),
 *         sum(IndexedSeq(3,4))
 *         ) (_+_)
 *
 *    map2(
 *         map2( unit(1), unit(2))(_+_),
 *         sum(IndexedSeq(3,4))
 *        )(_+_)
 *
 *   map2(
 *        map2( unit(1), unit(2))(_+_),
 *        map2( sum(IndexedSeq(3)), sum(IndexedSeq(4)))(_+_)
 *        )(_+_)
 *
 *  }}}
 *
 *
 *
 */
def sumPar(l: List[Int]): Par[Int] = l match {

  case l if l.size <= 1 => Par.unit ( l.headOption getOrElse 0 )

  case _           =>

    val (ll, lr) = l.splitAt(l.size /  2)

    Par.map2(sumPar(ll), sumPar(lr)) {_ + _}
}
