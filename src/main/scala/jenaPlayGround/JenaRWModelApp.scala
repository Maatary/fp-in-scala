package jenaPlayGround

import org.apache.jena.riot.{Lang, RDFDataMgr}
import org.apache.jena.riot.system.stream.StreamManager

/**
 * =[[https://jena.apache.org/documentation/io/rdf-input.html Readying RDF in Apache Jena ]]=
 *
 *
 *
 *
 * Much of the functionality is accessed via the Jena Model API
 * Package [[https://jena.apache.org/documentation/javadoc/jena/org/apache/jena/rdf/model/package-summary.html org.apache.jena.rdf.model]])
 * see [[https://jena.apache.org/documentation/javadoc/jena/org/apache/jena/rdf/model/ModelFactory.html ModelFactory]]
 * e.g. ModelFactory.createDefaultModel().read(model)
 *
 * Direct calling of the RIOT subsystem isn’t needed.
 *
 * A resource name with no URI scheme is assumed to be a local file name.
 *
 * Applications typically use at most RDFDataMgr to read RDF datasets.
 *
 * Model -use- > RDFDataMgr -use-> [StreamManager, RDFParser]
 *
 * RDFParser -use-> StreamManager
 *
 * =Package org.apache.jena.riot [[https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/riot/package-summary.html RIOT]]=
 *
 * ==RDFDataMngr==
 *
 * RDFDataMgr provides operations to load, read and write models and datasets.
 * RDFDataMgr “load” operations create an in-memory container (model, or dataset as appropriate); “read” operations add data into an existing model or dataset.
 *
 * see [[https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/riot/RDFDataMgr.html RDFDataMgr]]
 *
 * ==RDFParser==
 *
 * Detailed control over the setup of the parsing process is provided by RDFParser which provides a builder pattern.
 * see [[https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/riot/RDFParser.html RDFParser]]
 *
 * =Package org.apache.jena.riot.system.stream [[https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/riot/system/stream/package-summary.html Stream]]=
 *
 * ==StreamManager==
 *
 * The StreamManager is a utility to find and read files into models.
 * There is a standard global StreamManager and applications may also define specific ones by constructing additional StreamManagers.
 * The LocationMapper provides alternative locations for RDF data.
 *
 * Operations to read RDF data can be redirected to local copies and to other URLs. This is useful to provide local copies of remote resources.
 * By default, the RDFDataMgr uses the global StreamManager to open typed InputStreams.
 *
 * The StreamManager can be reconfigured with different places to look for files.
 * The default configuration used for the global StreamManager is a file access class,
 * where the current directory is that of the java process, a URL accessor for reading from the web, and a class loader-based accessor.
 *
 * Different setups can be built and used either as the global set up, or on a per request basis.
 *
 * There is also a LocationMapper for rewriting file names and URLs before use to allow placing known names in different places (e.g. having local copies of import http resources).
 *
 * see [[https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/riot/system/stream/StreamManager.html StreamManager ]]
 *
 * ==File Locator==
 *
 * Files are named by a string, according to the conventions of their storage system. Typically this is by URI.
 * There are a number of storage system adapters provided:
 *
 * -- File locator (with own current directory)
 * -- URL locator (HTTP and FTP)
 * -- Class loader locator
 * -- Zip file locator
 *
 * The global stream manager has a file location, a URL locator and a class loader (tried in that order).
 *
 * see [[https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/riot/system/stream/Locator.html Locator]]
 *
 * ==LocationMapper ==
 *
 * A StreamManager can have an associated LocationMapper that transforms names before use. This means local copies of documents can be used transparently to the rest of the application.
 * A StreamManager provides an “open” operation to get an InputStream to the resource.
 * see [[https://jena.apache.org/documentation/javadoc/arq/org/apache/jena/riot/system/stream/LocationMapper.html  LocationMapper]]
 *
 *
 */
object JenaRWModelApp extends App {


  import cats.effect.IO
  import cats.effect.unsafe.implicits.global
  import org.apache.jena.rdf.model.ModelFactory
  import scribe._

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Trace))
    .replace()

  //JenaSystem.init() Not needed, done in Factory of any submodule e.g. ModelFactory
  //JenaSystem.DEBUG_INIT = true Won't be needed will use Logging unless initialization issue, then useful


  //Selectively set to Error the Logging for FileManager Module
  //Logger(classOf[org.apache.jena.util.FileManager].getName).withMinimumLevel(Level.Error).replace()


  val prog = for {
    globalMapper <- IO { StreamManager.get().getLocationMapper }
    _            <- IO { globalMapper.addAltEntry("https://data.elsevier.com/lifescience/schema/rdbs", "elsevier_entellect_schema_rdbs.ttl") }
    model        <- IO { ModelFactory.createDefaultModel()}
    _            <- IO { model.read("https://data.elsevier.com/lifescience/schema/rdbs") }
   // _            <- IO {RDFDataMgr.write(System.out, model, Lang.TURTLE) } //not necessary
    _            <- IO { model.write(System.out, Lang.TURTLE.getName) }

  } yield ()



  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("program failed", value)
    case Right(_) => info("Model processed properly")
  }

}
