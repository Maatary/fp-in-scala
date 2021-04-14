package JenaPlayGround

import org.apache.jena.graph.compose.MultiUnion
import org.apache.jena.ontology.{OntDocumentManager, OntModelSpec}
import org.apache.jena.riot.Lang
import org.apache.jena.riot.system.stream.StreamManager

object JenaMultiUnionApp {

  import cats.effect.IO
  import cats.effect.unsafe.implicits.global
  import org.apache.jena.rdf.model.ModelFactory
  import scala.jdk.CollectionConverters._
  import scribe._

  scribe.Logger.root
        .clearHandlers()
        .clearModifiers()
        .withHandler(minimumLevel = Some(Level.Trace))
        .replace()

  //JenaSystem.init() Not needed done, in factory of any submodule e.g. ModelFactory
  //JenaSystem.DEBUG_INIT = true Won't be needed will use Logging unless initialization issue
  //Logger(classOf[org.apache.jena.util.FileManager].getName).withMinimumLevel(Level.Error).replace()


  val prog = for {

    //_          <- IO {JenaSystem.init()} // Not Needed but leave it here to remember to better understand it.
    ontDoc       <- IO { OntDocumentManager.getInstance() } // Set your global Ontology Manager without any LocationMapper, so the reliance on the StreamMndgr is ensured. The process is broken
    _            <- IO { ontDoc.setProcessImports(false) } // We do not want to process the imports
    globalMapper <- IO { StreamManager.get().getLocationMapper }
    _            <- IO { globalMapper.addAltEntry("https://data.elsevier.com/lifescience/schema/rdbs", "elsevier_entellect_schema_rdbs.ttl") }

    model1       <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
    model2       <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
    _            <- IO { model1.read("https://data.elsevier.com/lifescience/schema/rdbs", Lang.TURTLE.getName) }
    _            <- IO { model2.read("https://data.elsevier.com/lifescience/schema/rdbs", Lang.TURTLE.getName) }
    union        <- IO { ModelFactory.createModelForGraph(new MultiUnion(Array(model1.getGraph, model2.getGraph)))}
    //sts = union.listStatements(union.getResource("https://data.elsevier.com/lifescience/schema/rdbs/Aggregate"), RDF.`type`, null).asScala.toList.map(_.asTriple().toString)
    _            <- IO { println(s"model size is: ${union.size()} and model1 size is: ${model1.size()} and model2 size ${model2.size()} is")}
    //_            <- IO { union.write(System.out, Lang.TURTLE.getName) }

  } yield ()

  //OntDocumentManager.getInstance().setProcessImports(false)

  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("Program failed - Model not processed properly", value)
    case Right(_) => info("Program Succeed - Model processed properly")
  }

}
