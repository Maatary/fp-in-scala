package http4s


import org.apache.jena.rdf.model._
import org.apache.jena.riot._
import cats.syntax.all._
import cats.effect._
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global

import org.http4s._
import org.http4s.headers._
import org.http4s.dsl.io._
import org.http4s.client.dsl.io._
import org.http4s.multipart._
import org.http4s.Uri._
import org.http4s.client.Client
import org.http4s.syntax.all._
import org.http4s.implicits._

import scala.util.chaining.scalaUtilChainingOps





import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.blaze.client.BlazeClientBuilder
//import org.http4s.client.Client


import org.http4s.EntityDecoder.multipart
//import org.http4s.multipart.Part._


object CompileKarmaModelCientApp extends App {


  type Ontology           = String
  type PreMapping         = String
  type SyntheticMessage   = String
  type PreMappingModelURI = String
  type TopicName          = String


  /**
   *  A Function that takes list[ String ] and
   *  turns it into a IO [List [ Model] ]
   */



  /**
   *  A Function that takes a List[ Request[ IO ] ] and run them in parallel
   *  returning IO [ List[ String  ] ]
   */

  def runKarmaCompilationRequests(client: Client[IO],  l: List[Request[IO]]): IO[List[String]] = {
    l.parTraverse(client.expect[String](_))
  }


  /**
   * A Function that takes a List[(ontology, pre-mapping-model, syntheticMessage, mapping-model-uri, topicName)]
   * and returns list of "multi-parted POST" Request[IO]
   *
   * hardcode the endpoint for now
   *
   */

   def toRequests(preMappingsParams: List[(Ontology, PreMapping, SyntheticMessage, PreMappingModelURI, TopicName)]): List[Request[IO]] = {
      preMappingsParams map toRequest
   }


  /**
   * Function that takes
   *
   *  - ontology-model (ontology)
   *  - pre-mapping-model (model)
   *  - pre-mapping-sample (syntheticMessage)
   *  - mapping-model-uri (modelURI)
   *  - topicName
   *
   *  - OntologyType = true (hardcoded)
   *  - hostname = xxxx  (harcoded)
   *
   *  - EndPoint (defaulted | hardcoded)
   *
   *  and returns  a "MultiParted POST" Request[IO]
   *
   *
   */

   def toRequest(preMappingParams: (Ontology, PreMapping, SyntheticMessage, PreMappingModelURI, TopicName)): Request[IO] = {

     import org.http4s.implicits._

     Multipart[IO] (
       Vector (
         Part.formData("ontology"        , preMappingParams._1),
         Part.formData("model"           , preMappingParams._2),
         Part.formData("syntheticMessage", preMappingParams._3),
         Part.formData("modelURI"        , preMappingParams._4),
         Part.formData("topicName"       , preMappingParams._5),
         Part.formData("ontologyType"    , "true"),
         Part.formData("hostname"        , "100.67.219.44")
       )
     ) pipe { multipart => Method.POST(multipart , uri"http://localhost:8080").withHeaders(multipart.headers)}

   }

  import fs2.io.file.Files
  import fs2.io.file.Path



  (for {



    ontology         <- Files[IO].readAll(Path(getClass.getResource("/proxyInferenceModel.ttl").getPath)).compile.to(Array).map(new String(_))
    model            <- Files[IO].readAll(Path(getClass.getResource("/premapping-cellprocess.ttl").getPath)).compile.to(Array).map(new String(_))
    syntheticMessage <- Files[IO].readAll(Path(getClass.getResource("/premapping-cellprocess-sample-message.json").getPath)).compile.to(Array).map(new String(_))
    modelURI         <- IO.pure{"https://data.elsevier.com/lifescience/extraction/resnet/CellProcess"}
    topicName        <- IO.pure{"elsevier-cbe-resnet-CellProcess"}
    preMappingsParams = List[(Ontology, PreMapping, SyntheticMessage, PreMappingModelURI, TopicName)]((ontology,  model, syntheticMessage, modelURI, topicName))
    compileRequests   = toRequests(preMappingsParams)

    lResponse        <- BlazeClientBuilder[IO](IORuntime.global.compute).resource.use { runKarmaCompilationRequests(_, compileRequests) }

    _                <- IO.println(lResponse)


  } yield ()).unsafeRunSync()



}
