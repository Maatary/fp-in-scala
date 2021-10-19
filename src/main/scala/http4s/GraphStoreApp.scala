package http4s

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.riot.Lang
import org.http4s._
import org.http4s.blaze.client.BlazeClientBuilder
import org.http4s.dsl.io._
import org.http4s.client.dsl.io._
import org.http4s.headers._
import org.http4s.implicits._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.Client
import cats.syntax.all._
import org.apache.commons.io.output.ByteArrayOutputStream
import org.apache.jena.ontology.OntDocumentManager
import scribe._

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets

object GraphStoreApp extends App {

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Info) )
    .replace()

  case class MandatoryImportStreams(fdnStream: String, skosXlStream: String, skosStream:String)

  sealed abstract class OntologyIdentifier(val uri: String)
  final case object `Skos-xl` extends OntologyIdentifier("https://data.elsevier.com/lifescience/schema/skos-xl")
  final case object  Fdn extends OntologyIdentifier("https://data.elsevier.com/lifescience/schema/foundation")
  final case object  Skos extends OntologyIdentifier("https://data.elsevier.com/lifescience/schema/skos")

  case class TripleStoreConfig(url: String, modelDB: String, user: String, password: String)

  val tStore = TripleStoreConfig("https://stardog.sdc-oxygen-dev.nonprod.entellect.com", "IntegrationHub", "admin", "admin")

  val proxyUrls = List("https://data.elsevier.com/lifescience/schema/resnet")

  def makeOntoRequest(tStore: TripleStoreConfig)(ontoUri: String) = {
    IO.fromEither(Uri.fromString(ontoUri)).map {GET (_, Authorization(BasicCredentials(tStore.user, tStore.password))) }

  }

  def  getImportStreams(client: Client[IO], tStore: TripleStoreConfig): IO[MandatoryImportStreams] = {
    for {

      fdnRequest     <-  makeOntoRequest(tStore)(s"${tStore.url}/${tStore.modelDB}?graph=${Fdn.uri}")
      skosXlRequest  <-  makeOntoRequest(tStore)(s"${tStore.url}/${tStore.modelDB}?graph=${`Skos-xl`.uri}")
      skosRequest    <-  makeOntoRequest(tStore)(s"${tStore.url}/${tStore.modelDB}?graph=${Skos.uri}")

      importStreams  <- (client.expect[String](fdnRequest), client.expect[String](skosXlRequest), client.expect[String](skosRequest)).tupled

      mImports = MandatoryImportStreams tupled importStreams

      _              <-  if (this.logger.includes(Level.Debug)) logOnto(mImports.fdnStream) >> logOnto(mImports.skosStream) >> logOnto(mImports.skosXlStream) else IO.unit

    } yield mImports
  }

  def getProxyStreams(client: Client[IO], tStore: TripleStoreConfig, proxies: List[String]): IO[List[String]] = {
    for {

      proxyRequestUrls <- IO.pure {  proxies map {proxyUrl => s"${tStore.url}/${tStore.modelDB}?graph=$proxyUrl"} }
      proxyRequests    <- proxyRequestUrls traverse makeOntoRequest(tStore)
      proxyStreams     <- proxyRequests traverse client.expect[String]

      _                <- if (this.logger.includes(Level.Debug)) proxyStreams traverse logOnto else IO.unit

    } yield proxyStreams
  }

  def logOnto(ontoStream: String): IO [Unit] = {
    for {

      model     <- IO { ModelFactory.createDefaultModel().read(new ByteArrayInputStream(ontoStream.getBytes(StandardCharsets.UTF_8)), null, Lang.TTL.getName)  }

      outStream <- IO.pure { new ByteArrayOutputStream() }

      _         <- IO { model.write(outStream, Lang.TTL.getName)  }

      _         <- IO { debug( outStream.toString(StandardCharsets.UTF_8) ) }

      _         <- IO { model.close() }

    } yield ()
  }


  (for {

    clientResource <- IO { BlazeClientBuilder[IO].resource }

    ontoStreams    <-  clientResource.use { client =>  (getImportStreams(client, tStore), getProxyStreams(client, tStore, proxyUrls)).tupled }

    (mImports, proxies)  = ontoStreams
    


  } yield ()).unsafeRunSync()





}
