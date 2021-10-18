import cats.syntax.functor._
import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.auto._
import io.circe.syntax._

sealed trait Event

case class Foo(i: Int) extends Event
case class Bar(s: String) extends Event
case class Baz(c: Char) extends Event
case class Qux(values: List[String]) extends Event

object GenericDerivation {
  implicit val encodeEvent: Encoder[Event] = Encoder.instance {
    case foo @ Foo(_) => foo.asJson
    case bar @ Bar(_) => bar.asJson
    case baz @ Baz(_) => baz.asJson
    case qux @ Qux(_) => qux.asJson
  }

  implicit val decodeEvent: Decoder[Event] =
    List[Decoder[Event]](
      Decoder[Foo].widen,
      Decoder[Bar].widen,
      Decoder[Baz].widen,
      Decoder[Qux].widen
    ).reduceLeft(_ or _)
}


Json.obj(
  "name"  ->  "Maat".asJson,
  "last"  ->  List(1,2,3,4).asJson
)

Json.obj().spaces2