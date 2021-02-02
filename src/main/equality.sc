import cats.syntax.all._


Right(1).withLeft[Double] == Right(1).withLeft[String]

//as opposed to Cats Eq below which would fail

Right(1).withLeft[Double] === Right(1).withLeft[String]



