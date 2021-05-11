import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import org.apache.jena.rdf.model.{ModelFactory, ResourceFactory}
import org.apache.jena.riot.Lang
import org.apache.jena.riot.system.stream.StreamManager
import org.apache.jena.util.ResourceUtils
import org.apache.jena.vocabulary.{OWL, RDF, RDFS}
import cats.syntax.all._
import scala.jdk.CollectionConverters._


/*
val prog = for {
  globalMapper <- IO { StreamManager.get().getLocationMapper }
  _            <- IO { globalMapper.addAltEntry("https://data.elsevier.com/lifescience/schema/rdbs", getClass.getResource("elsevier_entellect_schema_rdbs.ttl").getPath) }
  model        <- IO { ModelFactory.createDefaultModel()}
  _            <- IO { model.read("https://data.elsevier.com/lifescience/schema/rdbs") }

  resList      <- IO { model.listResourcesWithProperty(RDF.`type`, OWL.Ontology).asScala.toList}
   _           <- IO { println(resList.map(_.toString)) }
   _           <- IO { resList.map{r => ResourceUtils.renameResource(r, "https://data.elsevier.com/lifescience/schema/rdbsRenamed")}}
  resList      <- IO { model.listResourcesWithProperty(RDF.`type`, OWL.Ontology).asScala.toList}
  _            <- IO { println(resList.map(_.toString)) }

} yield ()*/
//ModelFactory.createDefaultModel().listStatements()
//ResourceFactory.createResource("urn:isbn").getLocalName
//ResourceFactory.createLangLiteral("hello", "en")
//prog.unsafeRunSync()

(for {
  model <- IO {ModelFactory.createDefaultModel()}

  //_     <- IO {model.createResource("http://example.com/Person").addProperty(RDF.`type`, RDFS.Class)}


  _     <- List("urn:maat", "urn:daniel").foldLeft[IO[Unit]](IO.unit) { case (io, urn) =>
                                                                        io.flatMap{ _ => IO { model.createResource(urn).addProperty(RDF.`type`, RDFS.Class)} }
                                                                      }


  //_     <- List(IO{model.createResource("urn:maat").addProperty(RDF.`type`, RDFS.Class)}, IO {model.createResource("urn:daniel").addProperty(RDF.`type`, RDFS.Class)}).sequence


  _     <- IO{model.write(System.out, Lang.TTL.getName)}

} yield ()).unsafeRunSync()

