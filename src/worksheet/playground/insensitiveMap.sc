import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.collection.immutable.{SortedMap, TreeMap}
import scala.util.chaining.scalaUtilChainingOps

val map = TreeMap("hello" -> 2, "key" -> 3)(Ordering.by(_.toLowerCase))

val map2 = SortedMap("key1" -> 45, "Key2" -> 43, "KEY3" -> 42)(scala.math.Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))

map("Hello")

import  scribe._
//https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488
//Not Type Safe, but we will catch the error somewhere else
def  inferCaseInsensitiveTypeFromUri(uri: String): String = {
  uri.split("/entity/").last.split("/")
    .pipe { array => s"https://data.elsevier.com/lifescience/schema/${array(0)}/${array(1)}"}
}

inferCaseInsensitiveTypeFromUri("https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488")






