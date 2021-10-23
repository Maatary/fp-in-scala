package jenaPlayGround

import org.apache.jena.ontology.{OntDocumentManager, OntModelSpec}
import org.apache.jena.rdf.model.{Literal, ModelFactory, ResourceFactory}
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.riot.system.stream.StreamManager
import org.apache.jena.sys.JenaSystem
import org.apache.jena.vocabulary.RDF
import cats.effect.IO
import cats.effect.unsafe.implicits.global

import scala.jdk.CollectionConverters._


object JenaPipelineMessage extends App {

  import scribe._


  val program = for {

    ontDoc       <- IO { OntDocumentManager.getInstance() } // Set your global Ontology Manager without any LocationMapper, so the reliance on the StreamMndgr is ensured. The process is broken
    _            <- IO { ontDoc.setProcessImports(false) }

    ontModel     <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
    _            <- IO { ontModel.read("messages/smallmol.ttl", Lang.TURTLE.getName) }

    ontResource  <- IO { ontModel.getOntResource("https://data.elsevier.com/lifescience/entity/resnet/smallmol/72057594038209488") }

    _            <- IO { info(ontResource.getRDFType.toString) }
    _            <-
      IO {
        val node = ontResource.getPropertyValue(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/resnet/dateCreated"))

        info( "is Literal: " + node.isLiteral.toString )

        info( "CanAs Literal: " + node.canAs(classOf[Literal]).toString )

        info( "dataType: " + node.as(classOf[Literal]).getDatatypeURI )

        info( "String value: " + node.as(classOf[Literal]).getString )

        info( ontResource.getPropertyValue(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/resnet/dateCreated")).toString )

      }

    _            <-
      IO {
        val node = ontResource.getPropertyValue(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/resnet/flags"))

        info( "is Literal: " + node.isLiteral.toString )

        info( "CanAs Literal: " + node.canAs(classOf[Literal]).toString )

        info( "dataType: " + node.as(classOf[Literal]).getDatatypeURI )

        info( "double value: " + node.as(classOf[Literal]).getString)

        info( ontResource.getPropertyValue(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/resnet/flags")).toString )

      }

   /* _            <-
      IO {
        val nodes = ontResource.listPropertyValues(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/resnet/flagss")).asScala.toList.map(_.asLiteral().getDouble)

        info(" list double values: " + nodes.mkString("|"))

        info( ontResource.getPropertyValue(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/resnet/flags")).toString )

      }*/


    //_            <- IO {  ontModel.write(System.out, Lang.TURTLE.getName) }

  } yield ()

 // program.unsafeRunSync()

  import cats.syntax.all._

  //For List of TgAttribute
  println(List(Option(1), Option(3), None, Option(4)))

  println(List(Option(1), Option(3), None, Option(4)).flatten)

  println(List(Option(1), Option(3), None, Option(4)).flatMap(_.toList))

  println( List(IO(Option(1)), IO(Option(3)), IO(None), IO(Option(4))).sequence.map(_.flatten).unsafeRunSync() )

  val value: AnyVal = 2

  val sValue = value.toString

  println(sValue)

}
