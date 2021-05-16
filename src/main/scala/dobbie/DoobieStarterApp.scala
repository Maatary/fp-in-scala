package dobbie


import scala.concurrent.Future


object DoobieStarterApp extends App {

  import doobie._
  import doobie.implicits._
  import cats.effect._
  import cats.syntax.all._
  import cats.effect.unsafe.implicits.global

  val transactor = Transactor.fromDriverManager[IO](
    "oracle.jdbc.driver.OracleDriver",
    "jdbc:oracle:thin:@localhost:1521:ORCL",
    "ro_pse",
    "ro_pse"
    )

  case class PropertyType(id: Int, `type`: String)
  case class ObjectType(id: Int, `type`: String)

  val q1 = sql"""select "ID", "sPropertyName" from "PropTypes"""".query[PropertyType].accumulate[List]

  val q2 = sql"""select "ID", "sPropertyName" from "PropTypes"""".query[ObjectType].accumulate[List]



 val schemaQueries =  (q1.transact(transactor), q2.transact(transactor)).parTupled

 val prog = for {

    res <- schemaQueries
    (properties, objects) = res

    _ <- IO { println(s"properties: $properties")}

    _ <- IO { println(s"Objects: $objects") }

  } yield ()



  prog.unsafeRunSync()

  println(List("hello", "World", "Maat").mkString("select * from (\n\n" , "\n Union All \n", " \n)" ))





}
