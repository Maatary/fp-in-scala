package jenaPlayGround

import org.apache.jena.rdf.model.Model
import org.apache.jena.reasoner.{ReasonerFactory, ReasonerRegistry}
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.riot.system.stream.StreamManager


/**
 * = Inference Model & Reasoner =
 *
 * see [[https://jena.apache.org/documentation/inference/ Reasoners and rule engines]]
 *
 *
 * == Obtaining a Reasoner - Package [[https://jena.apache.org/documentation/javadoc/jena/org/apache/jena/reasoner/package-summary.html Reasoner]] ==
 *
 * see [[https://jena.apache.org/documentation/inference/#reasonerAPI]]
 * === The Low Level Approach (Only for customization maybe) ===
 *
 * For each type of reasoner there is a factory class (which conforms to the interface ReasonerFactory)
 * an instance of which can be used to create instances of the associated Reasoner.
 * The factory instances can be located by going directly to a known factory class and using the static theInstance()
 * method or by retrieval from a global ReasonerRegistry which stores factory instances indexed by URI assigned to the reasoner.
 *
 * see [[https://jena.apache.org/documentation/javadoc/jena/org/apache/jena/reasoner/ReasonerFactory.html ReasonerFactory]]
 * and all its subclasses e.g.
 * [[https://jena.apache.org/documentation/javadoc/jena/org/apache/jena/reasoner/rulesys/RDFSRuleReasonerFactory.html RDFSRuleReasonerFactory]]
 *
 *   -- [[org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory#theInstance]] to get the Factory Instance
 *
 *   -- [[org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory#create(org.apache.jena.rdf.model.Resource)]] to create the Reasoner from that Factory Instance
 *
 *   -- [[org.apache.jena.reasoner.rulesys.RDFSRuleReasonerFactory#URI]] The unique URI mention above
 *
 *  e.g.
 *  {{{RDFSRuleReasonerFactory.theInstance.create(null)}}}
 *  {{{ReasonerRegistry.theRegistry().create(RDFSRuleReasonerFactory.URI)}}}
 *  === The SPI Application Level Approach (The right way) ===
 *
 *  `There are convenience methods on the ReasonerRegistry for locating a prebuilt instance of each of the main reasoners
 *  (getTransitiveReasoner, getRDFSReasoner, getRDFSSimpleReasoner, getOWLReasoner, getOWLMiniReasoner, getOWLMicroReasoner).`
 *
 *  see [[org.apache.jena.reasoner.ReasonerRegistry]] [[org.apache.jena.reasoner.rulesys.RDFSRuleReasoner RDFSRuleReasoner]]
 *
 *  e.g.
 *  {{{ReasonerRegistry.getRDFSSimpleReasoner()}}}
 *
 *  ===Note: ===
 *  Once you have a reasoner instance, the same instance can reused multiple times
 *  by binding it to different datasets, without risk of interference - there is no need to create a new reasoner instance each time
 *
 *  === The Model Application Level Approach (working with ModelFactory) ===
 *
 *  If working with the Ontology API it is not always necessary to explicitly locate a reasoner.
 *  The prebuilt instances of OntModelSpec provide easy access to the appropriate reasoners
 *  to use for different Ontology configurations.
 *
 *
 *  If all that is needed is a plain RDF Model with RDFS inference included then
 *  the convenience methods ModelFactory should be used:
 *
 *  e.g.
 *  {{{ModelFactory.createRDFSModel(ModelFactory.createDefaultModel())}}}
 *
 *
 *
 *  === Configuring a reasoner ===
 *  see [[https://jena.apache.org/documentation/inference/#reasonerAPI Section Configuring a reasoner]]
 *  see [[https://jena.apache.org/documentation/javadoc/jena/org/apache/jena/vocabulary/ReasonerVocabulary.html ReasonerVocabulary]]
 *
 *  == Applying a reasoner to data ==
 *
 *  - Once you have an instance of a reasoner it can then be attached to a set of RDF data to create an inference model.
 *    This can either be done by putting all the RDF data into one Model or by separating into two components - schema and instance data.
 *
 *
 *  - The prime value of this separation is to allow some deductions from one set of data (typically some schema definitions)
 *    to be efficiently applied to several subsidiary sets of data (typically sets of instance data).
 *
 *
 *  - This is done by partially-applying a reasoner to a set schema data using the method the `Reasoner.bindSchema(model)` method which returns a new, specialized, reasoner.
 *
 *
 *  - `To bind the reasoner to the final data set (usually instance data) and get the InfModel the method `reasoner.bind(model)` is used.
 *
 *
 *  - The Actual inference model with the reasoner attached to it, is created trough the ModelFactory methods `ModelFactory.createInfModel.`
 *
 *
 *  E.g.
 *
 *  -- ModelFactory.createInfModel( Reasoner reasoner, Model model ) // Call `reasoner.bind(model.getGraph());return new InfModelImpl( graph );`
 *
 *
 *  -- ModelFactory.createInfModel( Reasoner reasoner, Model schema, Model model ) // `reasoner.bindSchema(schema.getGraph()).bind(model.getGraph()); return new InfModelImpl( graph );`
 *
 * see [[org.apache.jena.rdf.model.ModelFactory]]
 *
 *  === Note ===
 *
 *  - `When Creating an InfModel via createInfModel( Reasoner reasoner, Model schema, Model model )` any subsequent modification to the model only affect the instance data i.e. model here.
 *  - `When Creating an InfModel via createInfModel( Reasoner reasoner, Model model )` Operation on the InfModel Change it all, cause there is no special reasoner bound to some schema data.
 *     Behind the scene, rebind is used automatically. In all cases, if the underlying model is changed, i.e. not via the InfModel, rebind must be called manually
 *
 *
 *  == Accessing inferences ==
 *
 *  - Having created a inference model then any API operations which access RDF statements will be able to access additional statements
 *    which are entailed from the bound data by means of the reasoner.
 *
 *  - Depending on the reasoner these additional virtual statements may all be precomputed the first time the model is touched,
 *    may be dynamically recomputed each time or may be computed on-demand but cached.
 *
 *
 *
 *  Some usage examples API can be found here [[https://jena.apache.org/documentation/inference/#reasonerAPI Section Some small examples]]
 *
 *  see [[org.apache.jena.rdf.model.InfModel InfModel]]
 *  see [[org.apache.jena.rdf.model.impl.InfModelImpl InfModelImpl]]
 *
 *  == What happens when the Reasoner Bind data ==
 *
 *  - see [[org.apache.jena.reasoner.rulesys.RDFSRuleReasoner#bind(org.apache.jena.graph.Graph)]] [[org.apache.jena.reasoner.rulesys.RDFSRuleReasoner RDFSRuleReasoner]]
 *
 *  - It returns an InfGraph [[org.apache.jena.reasoner.InfGraph]] and example Implementation [[org.apache.jena.reasoner.rulesys.FBRuleInfGraph]]
 *
 *  == Operations only offered by the InfModel ==
 *
 *  - Validation e.g. `infModel.validate().isValid()`
 *  - Extended list statements
 *  - Direct and indirect relationships
 *  - Derivations e.g. `InfModel.getDerivation(Statement)` or `InfModel.setDerivationLogging(true)`
 *  - Accessing Raw or Deduction Model Separately e.g. `InfModel.getDeductionsModel()` or  `InfModel.getRawModel()`
 *
 *  see [[https://jena.apache.org/documentation/inference/#operationsOnInferenceModels Operations on inference models]]
 *
 *  //TODO Reasoners (3) see [[https://jena.apache.org/documentation/inference/#rdfs]]
 *
 */
object JenaInferenceApp extends App {


  import cats.effect.IO
  import cats.effect.unsafe.implicits.global
  import org.apache.jena.rdf.model.ModelFactory
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
    globalMapper <- IO { StreamManager.get().getLocationMapper }
    _            <- IO { globalMapper.addAltEntry("https://data.elsevier.com/lifescience/schema/rdbs", "elsevier_entellect_schema_rdbs.ttl") }
    model        <- IO { ModelFactory.createRDFSModel(ModelFactory.createDefaultModel()) }
    _            <- IO { model.setDerivationLogging(false) }
    _            <- IO { model.read("https://data.elsevier.com/lifescience/schema/rdbs") }
    //cap          <- IO { ReasonerRegistry.getRDFSReasoner().getReasonerCapabilities}
    //cap          <- IO { RDFDataMgr.write(System.out, cap, Lang.TTL) }
    //none         <- IO[Model] {???}
    deduction    <- IO {model.getDeductionsModel.setNsPrefixes(model.getNsPrefixMap)}
    _            <- IO {RDFDataMgr.write(System.out, deduction, Lang.TURTLE) }

  } yield ()



  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("program failed", value)
    case Right(_) => info("Model processed properly")
  }

}
