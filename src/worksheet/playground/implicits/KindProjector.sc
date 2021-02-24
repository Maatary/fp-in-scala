import cats.Monad

Monad[Either[Int, +*]].flatMap(Right(2))(_ => Right(4))


import cats.Monad
import cats.Functor

val e: Either[Int, Int] = Monad[Either[Int, +*]].flatMap(Right(2))(_ => Right(4))


implicit def makeFunctor[U]: Functor[Either[U, *]] = new Functor[Either[U, *]] {
  override def map[A, B](fa: Either[U, A])(f: A => B): Either[U, B] = fa match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }
}

/**
 * [A] => Either[E, A] Where E is fixed/Known i.e. T[A] or (* -> *)
 */
/*implicit def toFunctor[E]: Functor[({type T[A] = Either[E, A]})#T] = new Functor[({type T[A] = Either[E, A]})#T] {
  override def map[A, B](fa: Either[E, A])(f: A => B) = ???
}*/

def f[F[_]: Functor, A, B](a: F[A], f: A => B) = implicitly[Functor[F]].map(a)(f)

f(Left(42): Either[Int, String], (_: String).length) // Partial Unification as [T] => Either[Int, T], String
f(Right(42): Either[Error, Int], (_: Int) + 1) //Partial Unification as [T] => Either[Error, T], Int

//Hence it finds our implicit. The trick is Partial unification is Right-biased. It fixes the left, and leave the right most free.