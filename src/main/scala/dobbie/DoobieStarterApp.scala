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

 /* val q1 = sql"""select "ID", "sPropertyName" from "PropTypes"""".query[PropertyType].accumulate[List]
  val q2 = sql"""select "ID", "sPropertyName" from "PropTypes"""".query[ObjectType].accumulate[List]*/

  val qx = sql"""select *
                |
                |from (
                |         select distinct "sPropertyName" as "AttributeType", "AttributeDataType" from "StrNodeProps"
                |                  UNPIVOT (
                |                  "sPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                  FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                  IN ( -- unpivot_in_clause
                |                      "sPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                      )
                |
                |                  )
                |                  join "NodeProps" ON "idProperty" = "NodeProps".ID
                |                  join "PropTypes" On "NodeProps"."idPropertyType" = "PropTypes".ID
                |
                |        Union All
                |
                |        select distinct "sPropertyName" as "AttributeType", "AttributeDataType" from "DictNodeProps"
                |                 UNPIVOT (
                |                 "idPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                 FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                 IN ( -- unpivot_in_clause
                |                     "idPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                     )
                |
                |                 )
                |                 join "NodeProps" ON "idProperty" = "NodeProps".ID
                |                 join "PropTypes" On "NodeProps"."idPropertyType" = "PropTypes".ID
                |
                |        Union All
                |
                |        select distinct "sPropertyName" as "AttributeType", "AttributeDataType" from "IntNodeProps"
                |                 UNPIVOT (
                |                 "lPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                 FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                 IN ( -- unpivot_in_clause
                |                     "lPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                     )
                |
                |                 )
                |                 join "NodeProps" ON "idProperty" = "NodeProps".ID
                |                 join "PropTypes" On "NodeProps"."idPropertyType" = "PropTypes".ID
                |
                |         Union All
                |
                |        select distinct "sPropertyName" as "AttributeType", "AttributeDataType" from "MemoNodeProps"
                |                 UNPIVOT (
                |                 "sLongPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                 FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                 IN ( -- unpivot_in_clause
                |                     "sLongPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                     )
                |
                |                 )
                |                 join "NodeProps" ON "idProperty" = "NodeProps".ID
                |                 join "PropTypes" On "NodeProps"."idPropertyType" = "PropTypes".ID
                |
                |         Union All
                |
                |        select distinct "sPropertyName" as "AttributeType", "AttributeDataType" from "NumNodeProps"
                |                 UNPIVOT (
                |                 "rPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                 FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                 IN ( -- unpivot_in_clause
                |                     "rPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                     )
                |
                |                 )
                |                 join "NodeProps" ON "idProperty" = "NodeProps".ID
                |                 join "PropTypes" On "NodeProps"."idPropertyType" = "PropTypes".ID
                |        )""".stripMargin.query[(String, String)].accumulate[List]


  val qy = sql"""select *
                |
                |from (
                |         select distinct "sPropertyName" as "AttributeType", "AttributeDataType"
                |         from "StrControlProps"
                |                  UNPIVOT (
                |                  "sPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                  FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                  IN ( -- unpivot_in_clause
                |                      "sPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                      )
                |
                |                  )
                |                  join "ControlProps" ON "idProperty" = "ControlProps".ID
                |                  join "PropTypes" On "ControlProps"."idPropertyType" = "PropTypes".ID
                |
                |         Union All
                |
                |         select distinct "sPropertyName" as "AttributeType", "AttributeDataType"
                |         from "DictControlProps"
                |                  UNPIVOT (
                |                  "idPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                  FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                  IN ( -- unpivot_in_clause
                |                      "idPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                      )
                |
                |                  )
                |                  join "ControlProps" ON "idProperty" = "ControlProps".ID
                |                  join "PropTypes" On "ControlProps"."idPropertyType" = "PropTypes".ID
                |
                |         Union All
                |
                |         select distinct "sPropertyName" as "AttributeType", "AttributeDataType"
                |         from "IntControlProps"
                |                  UNPIVOT (
                |                  "lPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                  FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                  IN ( -- unpivot_in_clause
                |                      "lPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                      )
                |
                |                  )
                |                  join "ControlProps" ON "idProperty" = "ControlProps".ID
                |                  join "PropTypes" On "ControlProps"."idPropertyType" = "PropTypes".ID
                |
                |         Union All
                |
                |         select distinct "sPropertyName" as "AttributeType", "AttributeDataType"
                |         from "MemoControlProps"
                |                  UNPIVOT (
                |                  "sLongPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                  FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                  IN ( -- unpivot_in_clause
                |                      "sLongPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                      )
                |
                |                  )
                |                  join "ControlProps" ON "idProperty" = "ControlProps".ID
                |                  join "PropTypes" On "ControlProps"."idPropertyType" = "PropTypes".ID
                |
                |         Union All
                |
                |         select distinct "sPropertyName" as "AttributeType", "AttributeDataType"
                |         from "NumControlProps"
                |                  UNPIVOT (
                |                  "rPropertyValue" -- unpivot_clause - The column that represents the unpivoted values
                |                  FOR "AttributeDataType" --  unpivot_for_clause - The column that will hold the measure’s values
                |                  IN ( -- unpivot_in_clause
                |                      "rPropertyValue" /*AS 'sPropertyValue'*/
                |
                |                      )
                |
                |                  )
                |                  join "ControlProps" ON "idProperty" = "ControlProps".ID
                |                  join "PropTypes" On "ControlProps"."idPropertyType" = "PropTypes".ID
                |     )""".stripMargin.query[(String, String)].accumulate[List]

 /*val schemaQueries =  (q1.transact(transactor), q2.transact(transactor)).parTupled

 val prog = for {

    res <- schemaQueries
    (properties, objects) = res

    _ <- IO { println(s"properties: $properties")}

    _ <- IO { println(s"Objects: $objects") }

  } yield ()*/

  println(qy.transact(transactor).unsafeRunSync() )

  //prog.unsafeRunSync()







}
