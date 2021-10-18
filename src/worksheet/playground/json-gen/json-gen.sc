import json.value.{JsArray, JsInt, JsObj, JsValue}
import json.value.Preamble._
import json.value.gen.Preamble._
import json.value.gen.{JsArrayGen, JsObjGen}
import org.scalacheck.Gen
//import cats.syntax.all._


val l1 = List[(String, String)](("name", "STRING"), ("age", "INT"), ("job", "ARRAY")) map {
  case (key, "STRING") => JsObjGen(key -> Gen.alphaStr)
  case (key, "INT")    => JsObjGen(key -> Gen.double)
  case (key,"ARRAY")   => JsObjGen(key -> JsArrayGen(Gen.alphaStr, Gen.alphaStr, Gen.alphaStr))
}

//l1.foldRight[Gen[JsObj]](Gen.const(JsObj())){JsObjGen.concat(_, _)}.sample.get.toPrettyString

//val mySample = l1.reduceRight[Gen[JsObj]]{JsObjGen.concat(_, _)}.sample.get.toPrettyString



val l2: List[(String, Gen[JsValue])] = List[(String, String)](("name", "STRING"), ("age", "INT"), ("job", "ARRAY")) map {
  case (key, "STRING") => key -> Gen.alphaStr
  case (key, "INT")    => key -> Gen.double
  case (key,"ARRAY")   => key -> JsArrayGen(Gen.alphaStr.suchThat(_.nonEmpty), Gen.alphaStr.suchThat(_.nonEmpty), Gen.alphaStr.suchThat(_.nonEmpty))
}

JsObjGen.apply(l2:_*).sample.get

val myJsonObjString = JsArray(JsObjGen.apply(l2:_*).sample.get).toPrettyString


import org.json._

new JSONArray(myJsonObjString)


val person =
  JsObj(
    "@type" -> "Person",
    "age" -> 37,
    "age" -> 38)

val l = List(1,2,3) map JsInt.apply

JsArray(l)

