import cats.{Functor, Monad}

case class Kleisli[F[_], A, B](run: A => F[B]) {
  def apply(a: A): F[B] = run(a)

  def map[C](f: B => C)(implicit F: Functor[F]): Kleisli[F, A, C] = Kleisli { a => F.map(run(a))(f) }


  /**
   * Effectful Function Sequencing i.e. Effectful function as Monad
   *
   * {{{ A => F[B] flatMap B => A => F[C] }}}
   *
   * Pay attention to the difference with Monad on Function1
   *
   * - {{{ p => g(f(p))(p)  }}}
   *
   *
   * - {{{ a => M.flatMap(run(a))(b => f(b).run(a))  }}}
   *
   *
   * - In Function1 you apply the glue `g` yourself and thread the input
   *
   * - In EffectFul Function you rely on the monadic nature of the result of the first function,
   *   and supply it with a new glue function that calls the orginal input glue function.
   *   This is because it is an effect, and it is that effectful result that should manage the sequencing of the next effect.
   */
  def flatMap[C](f: B => Kleisli[F, A, C])(implicit M: Monad[F]): Kleisli[F, A, C] = Kleisli { a => M.flatMap(run(a))(b => f(b).run(a)) }



  /**
   *   Bellow we have the Effectful equivalent of `f compose g`  and `g andThen f`
   */

  // Effectful g andThen f
  def flatMapF[C](f: B => F[C])(implicit M: Monad[F]): Kleisli[F, A, C] = Kleisli { a => M.flatMap(run(a))(f) }

  def andThen[C](f: B => F[C])(implicit M: Monad[F]): Kleisli[F, A, C] = flatMapF(f)

  def andThen[C](that: Kleisli[F, B, C])(implicit M: Monad[F]): Kleisli[F, A, C] = this andThen that.run


  // Effectful  f compose  g  - It relies on andThen
  def compose[Z](that: Kleisli[F, Z, A])(implicit M: Monad[F]): Kleisli[F, Z, B] = that andThen this

  def compose[Z](f: Z => F[A])(implicit M: Monad[F]): Kleisli[F, Z, B] = Kleisli(f) andThen this.run


}

{
  import cats.data.Kleisli
}

