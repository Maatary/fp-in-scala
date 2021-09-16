import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.ModelFactory


import org.apache.jena.atlas.logging.LogCtl
import org.apache.jena.graph.Graph
import org.apache.jena.riot.Lang
import org.apache.jena.riot.RDFDataMgr
import org.apache.jena.shacl.ShaclValidator
import org.apache.jena.shacl.Shapes
import org.apache.jena.shacl.ValidationReport
import org.apache.jena.shacl.lib.ShLib

import scala.jdk.CollectionConverters._


val program = for {

  model       <- IO { ModelFactory.createDefaultModel().read("proxyInferenceModel.ttl") }

  shapes      <- IO { Shapes.parse( model.getGraph) }

  size        <- IO {shapes.iteratorAll().asScala.toList.size}

  //print          <- slist traverse(shape => IO {shape.getShapeNode.getURI})

  //_ <- IO {model.write(System.out, Lang.TTL.getName)}


} yield size

println(program.unsafeRunSync())