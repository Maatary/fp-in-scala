package JenaPlayGround

import org.apache.jena.ontology.{OntDocumentManager, OntModelSpec}
import org.apache.jena.rdf.model.ResourceFactory
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.riot.system.stream.StreamManager
import org.apache.jena.sys.JenaSystem
import org.apache.jena.vocabulary.RDF

/**
 * = OntModel =
 *
 * see [[https://jena.apache.org/documentation/ontology/ Jena Ontology API]]
 *
 * see [[org.apache.jena.ontology.OntModel OntModel]] and [[org.apache.jena.ontology.impl.OntModelImpl OntModelImpl]]
 *
 * == Loading Ontology & The OntDocumentManager ==
 *
 * E.g. {{{ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM)}}}
 *
 * Behind the scene the OntModel relies on the OntDocumentManager
 * to handle the location & opening of the file including the imports.
 *
 * Each ontology model has an associated document manager which assists with the processing and handling of ontology documents and related concerns.
 * For convenience, there is one global document manager which is used by default by ontology models.
 * You can get a reference to this shared instance through OntDocumentManager.getInstance().
 * In many cases, it will be sufficient to simply change the settings on the global document manager to suit your application’s needs.
 *
 * The OntDocumentManager provides further option to handle Ontology loading configuration.
 * This is necessary because ontology introduce import mechanics and notion of document on top of Model.
 *
 * However Jena is now based on on RDFDataMg StreamManager FileLocator LocationMapper Mechanics to handle
 * locating and opening file. This cause an issue with OntDocumentManager which was developped based on FileManager
 * and other mechanics as explained below.
 *
 * see [[org.apache.jena.ontology.OntDocumentManager OntDocumentManager]]
 *
 * === On the issue of the File Locator (including Location Mapper) mechanic of the OntModel ===
 *
 * Provides services for managing ontology documents, including loading imported
 * documents, and locally caching documents from resolvable URL's to improve
 * load performance. This class now delegates some of the responsibility for
 * resolving URI's and caching models to {@link org.apache.jena.util.FileManager FileManager}.
 * By default, OntDocumentManager will use a copy of the
 * singleton global FileManager. Alternatively, a specific <code>FileManager</code>
 * can be given to a document manager instance to use when resolving URI's and file paths.
 * Note that the default behaviour is to hold a <strong>copy</strong> of the global file
 * manager. In order to ensure that the document manager directly uses the global
 * file manager (e.g. so that document manager sees updates to the location mappings
 * held by the file manager), use the {@link #setFileManager ( FileManager )} method. For
 * example:
 *
 * OntDocumentManager dm = OntDocumentManager.getInstance();
 * dm.setFileManager( FileManager.get() );
 *
 * Issue with LocationMapper and OntImpl
 *
 * Solution:
 *
 * -- Set your global Ontology Manager without any LocationMapper AND
 *    BEFORE any SETUP of the StreamManager AND IT'S LocationMapper,
 *    so the reliance on the StreamManager (new and recommended mechanism) is ensured.
 *
 * -- This is because the migration to the StreamManager is not done for the OntImpl.
 *    It relies on an very approach where the Locator (Files, ClassLoader, URL) are not used.
 *    {{{
 *    dm.addAltEntry(
 *    "https://data.elsevier.com/lifescience/schema/rdbs",
 *    "file:/Users/maatari/Dev/IdeaProjects/fp-in-scala/src/main/resources/elsevier_entellect_schema_rdbs.ttl");
 *    }}}
 *    Then you would also need in case of turtle serialization
 *    {{{
 *      model.read("https://data.elsevier.com/lifescience/schema/rdbs", Lang.TURTLE.getName)
 *    }}}
 *
 *
 * -- Possible because as explained above, OntDocumentManager will use a copy of the singleton global FileManager.
 *    So instantiating it before will ensure that modification to the StreamManager won't be picked up.
 *    If picked up, it will pick the location, but try to read it without the file locator and fail,
 *    hence it would need the above set up
 *
 *
 * see [[org.apache.jena.riot.RDFParser#openTypedInputStream(java.lang.String, java.nio.file.Path)]]
 *
 * which make use of the streamManager {{{streamManager.mapURI(urlStr)}}}
 * as opposed to the mechanics of the OntModel inside its read method trough the assumption made about OntDocumentManager
 * that does the wrong thing.
 *
 * see [[org.apache.jena.ontology.impl.OntModelImpl#read(java.lang.String, java.lang.String, java.lang.String)]]
 *
 *
 *
 *
 * == Creating Ontology & the Management of the Load of the Imports (Include MultiUnion) ==
 *
 * see [[https://jena.apache.org/documentation/ontology/#compound-ontology-documents-and-imports-processing]]
 *
 * Jena helps ontology developers to work with modular ontologies by automatically handling the imports statements in ontology models.
 * The key idea is that the an Ontology model is actually a collection of models, one per imported model.
 *
 * By default, when an ontology model reads an ontology document, it will also locate and load the document’s imports.
 *
 * When an OntModel reads this document, it will notice the owl:imports line and attempt to load the imported ontology
 * into a sub-model of the ontology model being constructed.
 * The definitions from both the base ontology and all of the imports will be visible to the reasoner.
 *
 * Each imported ontology document is held in a separate graph structure.
 * This is important: we want to keep the original source ontology separate from the imports.
 * When we write the model out again, normally only the base model is written (the alternative is that all you see is a confusing union of everything).
 * And when we update the model, only the base model changes. To get the base model or base graph from an OntModel, use:
 *
 * {{{ val base: Model = myOntModel.getBaseModel }}}
 *
 *
 * In order for the document manager to build the union of the imported documents (which we sometimes refer to as the imports closure),
 * there must be some means of creating new graphs to store the imported ontologies. Loading a new import means that a new graph needs to be added.
 *
 * There are two cases in which we may want to create storage for models on-demand. The first is when creating the OntModel for the first time.
 * Some variants of createOntologyModel will allocate space for the base model (instead of, for example, being handed a base model to use as one of the method arguments).
 * The second case when storage must be allocated is when adding an imported document to the union of imports. These cases often require different policies,
 * so the OntModelSpec contains two model maker parameters: the base model maker and imports model maker, available via getBaseModelMaker() and getImportsModelMaker() methods respectively.
 *
 * The default specifications in OntModelSpec which begin MEM_ use an in-memory model maker for the both the base model and the imported documents.
 *
 * === Implementation note: ===
 * internally to Jena, we use Graph as a primary data structure.
 * However, application code will almost always refer to models, not graphs.
 * What’s happening is that a Model is a wrapper around the Graph, which balances a rich, convenient programming interface (Model) with a simple, manageable internal data structure (Graph).
 * Hence some potential confusion in that Figure 4, above, refers to a structure containing graphs, but we use a ModelMaker to generate new stores.
 * The document manager extracts the appropriate graph from the containing model.
 * Except in cases where you are extending Jena’s internal structures, you should think of Model as the container of RDF and ontology data.
 *
 * === Controlling Imports: ===
 * By default, loading imports during the read() call is automatic.
 * To read() an ontology without building the imports closure,
 * call the method setProcessImports( false ) on the document manager object before calling read().
 *
 * === Internal ===
 *
 * see [[org.apache.jena.ontology.impl.OntModelImpl#generateGraph(org.apache.jena.ontology.OntModelSpec, org.apache.jena.graph.Graph)]]
 *
 * from [[org.apache.jena.ontology.impl.OntModelImpl#OntModelImpl(org.apache.jena.ontology.OntModelSpec, org.apache.jena.rdf.model.Model, boolean)]]
 *
 * from [[org.apache.jena.ontology.impl.OntModelImpl#OntModelImpl(org.apache.jena.ontology.OntModelSpec)]]
 *
 * from [[org.apache.jena.rdf.model.ModelFactory#createOntologyModel(org.apache.jena.ontology.OntModelSpec)]]
 *
 * In Nutshell create the MultiUnion (set the baseGraph in the process), the reasoner, and bind the reasoner to the MultiUnion
 * after the load of the imports below rebind() to force the inference engine, if we have one, to see the new graph data
 *
 * Then
 *
 * see [[org.apache.jena.ontology.impl.OntModelImpl#addSubModel(org.apache.jena.rdf.model.Model, boolean)]]
 *
 * from [[org.apache.jena.ontology.OntDocumentManager#loadImport(org.apache.jena.ontology.OntModel, java.lang.String, java.util.List)]]
 *
 * from [[org.apache.jena.ontology.OntDocumentManager#loadImports(org.apache.jena.ontology.OntModel, java.util.List)]]
 *
 * After adding the model to the union call model.rebind() to ensure that the reasoner gets to see the updated axioms
 *
 *  === Implementation Note ===
 *
 * `The separation between schema and data, is not done in the management of the load of ontology documents, because well, it is just an Ontology`
 *
 * `This however can happen if you create an InfGraph, with as schema an Ontology, and data, well the instance data`
 *
 * `In the later scenario, care will need to be taken to figure out which reasoner will have the last word :) `
 *
 *
 *
 * == Creating Ontology and Inference Implication ==
 *
 * The definitions from both the base ontology and all of the imports will be visible to the reasoner.
 * model.rebind() to ensure that the reasoner gets to see the updated axioms
 *
 *
 *
 **/
object JenaOntModelApp extends App {


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

    model        <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
    _            <- IO { model.read("https://data.elsevier.com/lifescience/schema/rdbs", Lang.TURTLE.getName) }
    _            <- IO { model.write(System.out, Lang.TURTLE.getName) }

  } yield ()

 //OntDocumentManager.getInstance().setProcessImports(false)

  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("Program failed - Model not processed properly", value)
    case Right(_) => info("Program Succeed - Model processed properly")
  }

}
