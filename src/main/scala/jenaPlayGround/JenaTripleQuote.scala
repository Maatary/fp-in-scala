package jenaPlayGround

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.apache.jena.query.ARQ
import org.apache.jena.rdf.model.{ModelFactory, ResourceFactory}
import org.apache.jena.riot.system.PrefixMapAdapter
import org.apache.jena.riot.writer.TurtleWriter
import org.apache.jena.riot.{Lang, RIOT}

object JenaTripleQuote extends App {

  ARQ.getContext().set(RIOT.multilineLiterals, true)



  val program = for {

    _     <- IO { ARQ.getContext().set(RIOT.multilineLiterals, true)}

    model <- IO { ModelFactory.createDefaultModel() }


    _     <- IO { model.read("extraction_mapping_chembl_activity.ttl")}

    _     <- IO {new TurtleWriter().write(System.out, model.getGraph, new PrefixMapAdapter(model), null, ARQ.getContext())}

    //_     <- IO { model.write(System.out, Lang.TURTLE.getName)}

  } yield ()


  program.unsafeRunSync()

}
