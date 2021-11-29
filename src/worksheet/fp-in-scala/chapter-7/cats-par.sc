import cats.data.NonEmptyList
import cats.syntax.all._


List(Either.right(42), Either.left(new Exception("")), Either.left(new Exception(""))).parSequence



/*
List(1,2,3,4).parTraverse[Either[Exception, *], Int](Right(1).withLeft[Exception])*/
