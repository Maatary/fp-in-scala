package JenaPlayGround

import org.apache.jena.rdf.model.Model
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.riot.system.stream.StreamManager


/**
 * =Reasoner & Ontology=
 *
 * see [[https://jena.apache.org/documentation/inference/ Reasoners and rule engines]]
 * see [[https://jena.apache.org/documentation/ontology/#creating-ontology-models Jena Ontology API]]
 *
 *
 * == Internal of creation of Ontology Model ==
 *
 * see [[ org.apache.jena.ontology.impl.OntModelImpl#generateGraph(org.apache.jena.ontology.OntModelSpec, org.apache.jena.graph.Graph) ]]
 *
 * see [[org.apache.jena.ontology.impl.OntModelImpl#OntModelImpl(org.apache.jena.ontology.OntModelSpec, org.apache.jena.rdf.model.Model, boolean) ]]
 *
 * From
 *
 *     [[org.apache.jena.rdf.model.ModelFactory#createOntologyModel(org.apache.jena.ontology.OntModelSpec)]]
 *
 * In short OntModel Create a union graph behind the scene, where the supply ontology document is base, and any import if loaded is added as a model to the union.
 * Then an Inference Graph is build out of it. That is, a reasoner is bound to the union model.
 *
 *
 */
object JenaInferenceApp {


  import cats.effect.IO
  import cats.effect.unsafe.implicits.global
  import org.apache.jena.rdf.model.ModelFactory
  import scribe._

  scribe.Logger.root
        .clearHandlers()
        .clearModifiers()
        .withHandler(minimumLevel = Some(Level.Error))
        .replace()

  //JenaSystem.init() Not needed done, in factory of any submodule e.g. ModelFactory
  //JenaSystem.DEBUG_INIT = true Won't be needed will use Logging unless initialization issue
  //Logger(classOf[org.apache.jena.util.FileManager].getName).withMinimumLevel(Level.Error).replace()


  val prog = for {
    model        <- IO { ModelFactory.createRDFSModel(ModelFactory.createDefaultModel()) }
    _            <- IO { model.setDerivationLogging(false) }
    globalMapper <- IO { StreamManager.get().getLocationMapper }
    _            <- IO { globalMapper.addAltEntry("https://data.elsevier.com/lifescience/schema/rdbs", "elsevier_entellect_schema_rdbs.ttl") }
    _            <- IO { model.read("https://data.elsevier.com/lifescience/schema/rdbs") }
    //cap          <- IO { ReasonerRegistry.getRDFSReasoner().getReasonerCapabilities}
    //cap          <- IO { RDFDataMgr.write(System.out, cap, Lang.TTL) }
    none         <- IO[Model] {???}
    deduction    <- IO {model.getDeductionsModel.setNsPrefixes(model.getNsPrefixMap)}
    _            <- IO {RDFDataMgr.write(System.out, deduction, Lang.TURTLE) }

  } yield ()



  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("program failed", value)
    case Right(_) => info("Model processed properly")
  }

}
