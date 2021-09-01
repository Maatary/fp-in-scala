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

import java.io.{ByteArrayInputStream, InputStream}
//import org.http4s.client.Client


import org.http4s.EntityDecoder.multipart
//import org.http4s.multipart.Part._


object CompileKarmaModelCientApp extends App {


  type Ontology           = String
  type Mapping            = String
  type SyntheticMessage   = String
  type MappingModelURI    = String
  type TopicName          = String
  type DatasetName        = String
  type EntityType         = String
  type IsCBE              = Boolean
  type CompiledMapping    = String

  type MappingInfo        = (Mapping, SyntheticMessage, EntityType, IsCBE)
  type MappingParams      = (Ontology, Mapping, SyntheticMessage, MappingModelURI, TopicName)



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

   def toRequests(mappingsParams: List[(Ontology, Mapping, SyntheticMessage, MappingModelURI, TopicName)]): List[Request[IO]] = {
      mappingsParams map toRequest
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

   def toRequest(mappingParams: (Ontology, Mapping, SyntheticMessage, MappingModelURI, TopicName)): Request[IO] = {

     import org.http4s.implicits._

     Multipart[IO] (
       Vector (
         Part.formData("ontology"        , mappingParams._1),
         Part.formData("model"           , mappingParams._2),
         Part.formData("syntheticMessage", mappingParams._3),
         Part.formData("modelURI"        , mappingParams._4),
         Part.formData("topicName"       , mappingParams._5),
         Part.formData("ontologyType"    , "true"),
         Part.formData("hostname"        , "100.67.219.44")
       )
     ) pipe { multipart => Method.POST(multipart , uri"http://localhost:8080").withHeaders(multipart.headers)}

   }

  def makeExtractionUri(datasetName: DatasetName, entityType: EntityType): MappingModelURI =  {
    s"https://data.elsevier.com/lifescience/extraction/$datasetName/${entityType.toLowerCase}"
  }

  def makeTopicName(datasetName: DatasetName, entityType: EntityType,  isCbe:  IsCBE): TopicName  = {
    s"elsevier-${if (isCbe) "cbe" else "raw"}-$datasetName-$entityType"
  }

  import fs2.io.file.Files
  import fs2.io.file.Path


  def compileKarmaMappingModel(pOntology: Ontology, datasetName: DatasetName, mappingsInfo: List[MappingInfo]): IO[List[(CompiledMapping, EntityType)]] = {

    for {

      mappingsParams <- IO.pure{ mappingsInfo.map { e => ( pOntology, e._1, e._2, makeExtractionUri(datasetName, e._3), makeTopicName(datasetName, e._3, e._4) )  } }
      compileRequests   = toRequests(mappingsParams)

      lResponse        <- BlazeClientBuilder[IO](IORuntime.global.compute).resource.use { runKarmaCompilationRequests(_, compileRequests) }

      res              <- lResponse.traverse(pairWithEntityType(_))

    } yield res
  }

  def pairWithEntityType(modelAsString: String):IO[(CompiledMapping, EntityType)] = {

    for {
      model        <- IO { ModelFactory.createDefaultModel().read(new ByteArrayInputStream(modelAsString.getBytes), null, Lang.TTL.getName) }

      sourceTopic  <- IO { model.listStatements(null, ResourceFactory.createProperty("http://isi.edu/integration/karma/dev#sourceName"), null).nextStatement().getString}

      entityType   <- IO.pure{ sourceTopic.split("-").last}


    } yield (modelAsString, entityType)

  }


 val program = for {

    ontology         <- Files[IO].readAll(Path(getClass.getResource("/proxyInferenceModel.ttl").getPath)).compile.to(Array).map(new String(_))
    model            <- Files[IO].readAll(Path(getClass.getResource("/premapping-cellprocess.ttl").getPath)).compile.to(Array).map(new String(_))
    syntheticMessage <- Files[IO].readAll(Path(getClass.getResource("/premapping-cellprocess-sample-message.json").getPath)).compile.to(Array).map(new String(_))
    //modelURI         <- IO.pure{"https://data.elsevier.com/lifescience/extraction/resnet/CellProcess"}
    //topicName        <- IO.pure{"elsevier-cbe-resnet-CellProcess"}

    compiledMapping  <- compileKarmaMappingModel(ontology, "resnet",  List( (model, syntheticMessage, "CellProcess", true ) ) )

  } yield compiledMapping

  program.flatMap(list => list.traverse(IO.println(_))).unsafeRunSync()





  //import fs2._
  //Stream.emits("hello".getBytes).through(Files[IO].writeAll(Path("test.txt"))).compile.drain.unsafeRunSync()

}
