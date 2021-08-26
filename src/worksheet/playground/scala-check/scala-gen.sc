import org.scalacheck.Gen


val genInt = Gen.double

val pair = for {
  val1  <- Gen.chooseNum(1, 10)
  val2  <- Gen.chooseNum(1, 10)
} yield (val1,  val2)

pair.sample