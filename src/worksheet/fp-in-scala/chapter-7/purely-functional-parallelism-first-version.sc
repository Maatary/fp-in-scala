

trait Par[A]

object Par {


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
  def unit[A](a: A): Par[A] = ???


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
   * Extracts a value from a Par by actually performing the computation.
   */
  def run[A](a: Par[A]): A = ???


}