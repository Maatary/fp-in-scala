package dobbie

object DoobieStarterApp extends App {

  import doobie._
  import doobie.implicits._
  import cats.effect.IO
  import cats.effect.unsafe.implicits.global

  val transactor = Transactor.fromDriverManager[IO](
    "oracle.jdbc.driver.OracleDriver",
    "jdbc:oracle:thin:@localhost:1521:ORCL",
    "ro_pse",
    "ro_pse"
    )


  val q = sql"""select "ID", "sPropertyName" from "PropTypes"""".query[(Int, String)].option


  val res = q.transact(transactor).unsafeRunSync()

  println(res)

}
