import scala.concurrent.{ExecutionContextExecutorService, Future}
import scala.util.Success



object Par {

  // Type Function like f(x) = power(x) i.e Par[A] evaluate to a Type
  type Par[A] = ExecutionContextExecutorService => Future[A]


  /**
   * Create the a Program Description that return the value A, upon interpretation.
   *
   * Lift a constant value, into a Parallel Computation that ''immediately''
   * return the constant without forking upon interpretation.
   *
   * Create a Computation that ''immediately'' result in the value a.
   *
   * Promotes a constant value to a parallel computation.
   */
  def unit[A](a: A): Par[A] = _ => Future.fromTry(Success(a))


  /**
   * Wraps its unevaluated argument in a Par and marks it for concurrent evaluation.
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  /**
   * Marks a computation for concurrent evaluation.
   * The evaluation won’t actually occur until forced by run.
   */
  def fork[A](a: => Par[A]): Par[A] = ???

  /**
   * Combines the results of two parallel computations with a binary function.
   */
  def map2[A, B, C](parA: Par[A], parB: Par[B])(f: (A, B) => C): Par[C] = ???


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
   */
  def run[A](es: ExecutionContextExecutorService)(a: Par[A]): Future[A] = a(es)


}

import cats.Parallel
/*
import cats.ParallelArityFunctions
import cats.ParallelArityFunctions2
import cats.NonEmptyParallel*/
