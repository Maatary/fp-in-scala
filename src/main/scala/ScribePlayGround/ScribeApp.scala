package ScribePlayGround

import org.apache.jena.sys.JenaSystem

object ScribeApp extends App {

  import scribe._

  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Debug))
    .replace()

  Logger(classOf[org.apache.jena.util.FileManager].getName).withMinimumLevel(Level.Error).replace()

  info("info Log")

  trace("trace log")

  JenaSystem.init()






}
