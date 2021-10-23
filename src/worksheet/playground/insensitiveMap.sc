import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.collection.immutable.{SortedMap, TreeMap}

val map = TreeMap("hello" -> 2, "key" -> 3)(Ordering.by(_.toLowerCase))

val map2 = SortedMap("key1" -> 45, "Key2" -> 43, "KEY3" -> 42)(scala.math.Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))

map("Hello")

//https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488
def  inferCaseInsensitiveTypeFromUri(uri: String): IO[String] = {
  for {
    array  <- IO { uri.split("/entity/").last.split("/") }
    schema <- IO { array(0) }
    eType  <- IO { array(1) }
  } yield s"https://data.elsevier.com/lifescience/schema/${schema}/${eType}"
}

inferCaseInsensitiveTypeFromUri("https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488")
.unsafeRunSync()