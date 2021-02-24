import cats.{Monad, MonadError}
import cats.syntax.all._



//type MyEither[A] = Either[Int, A]
//Monad[MyEither].flatMap()

//Type Lambda to get Monad of Either
Monad[({type t[A] = Either[Throwable, A]})#t].flatMap(Right(3)){_ => Right(4)}
//Is equivalent to
Monad.apply[({type t[A] = Either[Throwable, A]})#t].flatMap(Right(3)){_ => Right(4)}


/**
 *  Partial Unification with Monad Type Class
 *  Bringing the implicit instance for Either for testing purpose
 *
 *  I don't partial unification is happening here, because `cats.Invariant.catsMonadErrorForEither[Throwable]`
 *  is already a partial application of Either.
 */

Monad.apply(cats.Invariant.catsMonadErrorForEither[Throwable]).flatMap(Right(3)){_ => Right(4)}

//Monad.apply.flatMap(Right(2).withLeft[Throwable])(_ => Right(2).withLeft[Throwable]) // ambiguity

Monad.apply(cats.Invariant.catsMonadErrorForEither[Throwable])

//MonadError[Either[Int, *], Int].attempt()