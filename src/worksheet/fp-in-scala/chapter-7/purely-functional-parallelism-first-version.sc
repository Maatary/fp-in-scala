

import org.apache.jena.ext.com.google.common.base.Supplier

import java.util.concurrent.{Callable, CompletableFuture}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContextExecutorService, Future}
import scala.util.Success

/**
 *  == Book Notes ==
 * We should note that Future doesn’t have a purely functional interface.
 * This is part of the reason why we don’t want users of our library to deal with Future directly.
 * But importantly, even though methods on Future rely on side effects, our entire Par API remains pure.
 * It’s only after the user calls run and the implementation receives an ExecutorService that we expose the Future machinery.
 * Our users therefore program to a pure interface whose implementation nevertheless relies on effects at the end of the day.
 * But since our API remains pure, these effects aren’t side effects.
 *
 * == Comment ==
 *
 * -- '''Par Being a function, means we  are lazy, nothing get executed.'''
 *
 * -- '''When we combine Par with combinator like map2, we create a new function, which means again, nothing get executed.
 *       map2  create a description of a computation.'''
 *
 * -- '''The bare minimum description of a computation is actually a function.'''
 *
 * == Consideration ==
 *
 * In this implementation i'm simulating the use of java future that the book uses with scala future.
 *
 * I'm avoiding to use advanced scala future functionality to stick to the spirit of the book.
 *
 */

object Par {

  /**
   *  '''Side Notes:'''
   *
   *   A Type Function like f(x) = power(x) i.e Par[A] evaluate to a Type which here is a function.
   *
   *   '''Type Alias''' are actually '''Type Function'''
   */
  type Par[A] = ExecutionContextExecutorService => Future[A]


  /**
   * Create the a Program Description that return the value A, upon interpretation.
   *
   * The Program description is a function.
   *
   * Lift a constant value, into a Parallel Computation that ''immediately''
   * return the constant without forking upon interpretation.
   *
   * Create a Computation that ''immediately'' result in the value a.
   *
   * Promotes a constant value to a parallel computation.
   */
  def unit[A](a: A): Par[A] = _ => Future.successful(a)


  /**
   * Wraps its unevaluated argument in a Par and marks it for concurrent evaluation.
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * Marks a computation for concurrent evaluation.
   * The evaluation won’t actually occur until forced by run.
   */
  def fork[A](a: => Par[A]): Par[A] = es => {

    import scala.jdk.FutureConverters._

    /*CompletableFuture.supplyAsync(new Supplier[A] {
      override def get(): A = es.submit(new Callable[A] {
        override def call(): A = Await.result(a(es), Duration.Inf)
      }).get()
    }).asScala*/

    // Trick because i am working with scala future albeit not using its facility just the type as  if  it  was  a java future
    CompletableFuture.supplyAsync{ () =>

      es.submit(new Callable[A] {
        def call(): A = Await.result(a(es), Duration.Inf) // If it is unit, note that you are already in the callable when evaluated, so  that is when a is ran/evaluated
      }).get()

    }.asScala

    // Because i am using scala future and not java future as in the book
    // In all and all this is problematic anyway
    //a(es)


  }

  /**
   * Combines the results of two parallel computations with a binary function.
   *
   * It returns a computation description i.e. a function i.e. Par
   *
   * It essence we combine 2 functions into a bigger function i.e. 2 descriptions into a bigger descriotion
   *
   * The bigger description, give the blueprint of how to execute the two innger parallel computation and combine their results.
   *
   *  -- submit the 2 futures immediately
   *
   *  -- Await for both results (the await are sequential but that is fine, it  is just await, not the start of the future)
   *
   *  -- The futures are still running in parallel.
   *
   *  Can also be if we want the operation the combination to be asynchronous
   *  Not sure it makes a big big difference
   *
   * {{{
   * fork(unit(f(Await.result(fa, Duration.Inf), Await.result(fb, Duration.Inf))))(es)
   * }}}
   * {{{
   * lazyUnit(f(Await.result(fa, Duration.Inf), Await.result(fb, Duration.Inf)))(es)
   * }}}
   */
  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = es => {

    val fa = parA(es)
    val fb = parB(es)

    unit(f(Await.result(fa, Duration.Inf), Await.result(fb, Duration.Inf)))(es)

    //lazyUnit(f(Await.result(fa, Duration.Inf), Await.result(fb, Duration.Inf)))(es)
    //fork(unit(f(Await.result(fa, Duration.Inf), Await.result(fb, Duration.Inf))))(es)

  }


  /**
   *  Convert any function A => B to one that evaluates its result asynchronously.
   */
  def asyncFRaw[A, B](f: A => B): A => Par[B] = (a: A) => (es: ExecutionContextExecutorService) => {

    Future(f(a))(es)
  }

  /**
   *  Convert any function A => B to one that evaluates its result asynchronously.
   */
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  /**
   * Extracts a value from a Par by actually performing the computation.
   * Not really needed here, because Par is a Function. Would be useful for a structure like IO
   */
  def run[A](es: ExecutionContextExecutorService)(a: Par[A]): Future[A] = a(es)


}



