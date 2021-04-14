package JenaPlayGround

import org.apache.jena.graph.compose.{Difference, MultiUnion}
import org.apache.jena.ontology.{OntDocumentManager, OntModelSpec}
import org.apache.jena.riot.Lang
import org.apache.jena.riot.system.stream.StreamManager

/**
 * A MultiUnion Graph Deduplicate its sub-graphs upon answering requests
 *
 * It is a dynamic union, so nothing is copied
 *
 * see [[org.apache.jena.graph.compose.MultiUnion#graphBaseFind(org.apache.jena.graph.Triple)]]
 */
object JenaMultiUnionApp extends App {

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

    model1       <- IO { ModelFactory.createDefaultModel() }
    model2       <- IO { ModelFactory.createDefaultModel() }
    _            <- IO { model1.read("https://data.elsevier.com/lifescience/schema/rdbs", Lang.TURTLE.getName) }
    _            <- IO { model2.read("https://data.elsevier.com/lifescience/schema/rdbs", Lang.TURTLE.getName) }


    union        <- IO { ModelFactory.createModelForGraph(new MultiUnion(Array(model1.getGraph, model2.getGraph)))}
    //sts = union.listStatements(union.getResource("https://data.elsevier.com/lifescience/schema/rdbs/Aggregate"), RDF.`type`, null).asScala.toList.map(_.asTriple().toString)


    // There is a diff here because of the blank nodes of the shacl Shapes, Otherwise the important bit is that a multiUnion deduplicate its subGraphs
    _            <- IO { println(s"model size is: ${union.size()} and model1 size is: ${model1.size()}, model2 size is ${model2.size()}")}
    _            <- IO { println("Diff with model2******************"); ModelFactory.createModelForGraph(new Difference(union.getGraph, model2.getGraph)).write(System.out, Lang.NTRIPLES.getName) }
    _            <- IO { println("Diff with model1******************"); ModelFactory.createModelForGraph(new Difference(union.getGraph, model1.getGraph)).write(System.out, Lang.NTRIPLES.getName) }
    //_            <- IO { union.write(System.out, Lang.TURTLE.getName) }

  } yield ()



  //OntDocumentManager.getInstance().setProcessImports(false)

  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("Program failed - Model not processed properly", value)
    case Right(_) => info("Program Succeed - Model processed properly")
  }

}
