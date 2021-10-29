import cats.{Functor, Monad}

implicit def function1Monad[P]: Monad[Function1[P, *]] = new Monad[Function1[P, *]] {
  override def pure[A](r: A): Function1[P, A] = _ => r

  override def flatMap[A, B](f: Function1[P, A])(g: A => Function1[P, B]): Function1[P, B] = p => g(f(p))(p)

  override def tailRecM[A, B](a: A)(f: A => P => Either[A, B]) = ???
}

implicit def Function1Functor[P]: Functor[P => *] = new Functor[Function[P, *]] {
  override def map[A, B](fa: Function[P, A])(f: A => B) = p => f(fa(p))
}


implicit class FlatMapSyntaxForFunction1[P, A](f: Function1[P, A]) {
  def flatMap[B](g: A => P => B): P => B = Monad[Function1[P, *]].flatMap(f)(g)
}

implicit class FunctorSyntaxForFunction1[P, A](f: P => A){
  def map[B](g: A => B): P => B = Functor[P => *].map(f)(g)
}

val f = (s:String) => s.toUpperCase

val g = (s:String)  => s.toLowerCase


val fng = f.flatMap{ a => g.map( a -> _) }


val fng2 = for {
  cap <- f
  low <- g
} yield cap -> low


fng2("maaTari")


fng("Hello")


/***
 * def flatMap[C](f: B => Kleisli[F, A, C])(implicit M: Monad[F]): Kleisli[F, A, C] = {
 *      Kleisli { a => M.flatMap(run(a))(b => f(b).run(a)) }
 * }
 */