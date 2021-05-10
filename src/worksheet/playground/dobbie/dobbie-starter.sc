import doobie._
import doobie.implicits._
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._

val transactor = Transactor.fromDriverManager[IO](
  "oracle.jdbc.driver.OracleDriver",
  "jdbc:oracle:thin:@localhost:1521:ORCL",
  "ro_pse",
  "ro_pse"
  )


val q = sql"""select "ID", "sPropertyName" from "PropTypes"""".query[(Int, String)]

//Description of a computation that when given a connection
//produces description of IO action that produce value of Type A

val resl = q.accumulate[List].transact(transactor).unsafeRunSync()

q.stream.compile.toList.transact(transactor).unsafeRunSync() //Stream[ConnectionIO, (Int, String)]





q.stream.map(e => e._1 + 1 -> e._2).transact(transactor).compile.toList.unsafeRunSync()

q.stream.transact(transactor) //Stream[IO, (Int, String)]


toDoobieStreamOps(q.stream).transact(transactor).map(e => e._1 + 1 -> e._2).compile.toList.unsafeRunSync()

//val program1 = 42.pure[ConnectionIO]
//program1.transact(transactor).unsafeRunSync()






