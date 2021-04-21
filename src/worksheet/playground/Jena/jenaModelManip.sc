import cats.effect.kernel.Ref
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource}
import org.apache.jena.rdf.model.{ModelFactory, ResourceFactory}
import org.apache.jena.riot.Lang
import org.apache.jena.riot.system.stream.StreamManager
import org.apache.jena.util.ResourceUtils
import org.apache.jena.vocabulary.{OWL, RDF}

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

ResourceFactory.createResource().getNameSpace
//prog.unsafeRunSync()


val e = IO {println("hello")}

e.unsafeRunSync()

e.unsafeRunSync()


val f = println("hello")

f
