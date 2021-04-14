package JenaPlayGround

import org.apache.jena.ontology.{OntDocumentManager, OntModelSpec}
import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.riot.system.stream.StreamManager
import org.apache.jena.sys.JenaSystem

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
 * == Loading Ontology & the Management of the Imports (Include MultiUnion) ==
 *
 *
 *
 *
 * == Creating Ontology and Inference Implication ==
 *
 *
 *
 *
 **/
object JenaOntModelApp extends App {


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
