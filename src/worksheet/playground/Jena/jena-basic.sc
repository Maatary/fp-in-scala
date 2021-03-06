import ammonite.ops._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.sys.JenaSystem
import scribe._

/*scribe.Logger.root
  .clearHandlers()
  .clearModifiers()
  .withHandler(minimumLevel = Some(Level.Error))
  .replace()*/

/*JenaSystem.init()



val prog = for {
  model <- IO {ModelFactory.createDefaultModel()}
} yield model

prog.attempt.unsafeRunSync() match {
  case Left(value) => error("program failed", value)
  case Right(value) => info("model red")
}

ls! pwd*/


//getClass.getResource("elsevier_entellect_schema_rdbs.ttl").getPath

 class Weekday( val name: String,
                               val abbreviation: String,
                               val isWorkDay: Boolean)

class Weeky extends Weekday("Monday", "Mo.", true)

