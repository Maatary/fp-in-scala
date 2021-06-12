import cats.effect._
import cats.effect.{Resource => eResource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.riot.Lang
import org.http4s.headers._

import java.io.ByteArrayOutputStream
import scala.collection.mutable.ArrayBuffer

//import cats._
//import cats.implicits._


import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._

import org.http4s.client.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.Client

//import org.http4s.headers._
//import org.http4s.server._
//import org.http4s.dsl.impl._


import org.apache.jena.rdf.model._


/**
 * Not needed because ByteArrayOutputStream being in memory does not need to be closed
 * and Jena handle the file resource closing automatically.
 */

val enrichedWithDynamic = "elsevier_entellect_enriched_dbschema_resnet_basic_with_tag_with_dynamic.ttl"

def makeOutStreamResourceIO = {
  IO {
    cats.effect.Resource.make[IO, ByteArrayOutputStream] {IO {new ByteArrayOutputStream()}} { outStream => IO {outStream.close()} }
  }

}

def getModelAsByArray(filename: String) = {

  for {

    outStreamResource <- makeOutStreamResourceIO

    bArray            <- outStreamResource.use { stream => IO { ModelFactory.createDefaultModel()
                                                                            .read(filename)
                                                                            .write(stream, Lang.TTL.getName) } >> IO {stream.toByteArray } }
  } yield (bArray)

}

//Just Printing
val e = new String(getModelAsByArray(enrichedWithDynamic).unsafeRunSync(), java.nio.charset.StandardCharsets.UTF_8)

println(e)





