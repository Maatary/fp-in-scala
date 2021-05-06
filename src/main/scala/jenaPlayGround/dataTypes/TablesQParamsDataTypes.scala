package jenaPlayGround.dataTypes
import org.apache.jena.vocabulary.RDFS


object TablesQParamsDataTypes {


  /**
   *
   *      TablesQParams
   *
   */



  sealed abstract class TablesQParams

  sealed abstract class EdgeAttributeTablesQParams(
                                                    val typedAttributeValueTableLabel: String,
                                                    val tavTattributeIdColumnLabel: String,
                                                    val typedAttributeValueColumnLabel: String,
                                                    val attributeTableLabel: String,
                                                    val aTattributeIdColumnLabel: String,
                                                    val aTattributeTypeIdColumnLabel: String,
                                                    val attributeTypeTableLabel: String,
                                                    val atTattributeTypeIdColumnLabel: String,
                                                    val attributeTypeColumnLabel: String
                                                  ) extends TablesQParams

  sealed abstract class VerticeAttributeTablesQParams(
                                                    val typedAttributeValueTableLabel: String,
                                                    val tavTattributeIdColumnLabel: String,
                                                    val typedAttributeValueColumnLabel: String,
                                                    val attributeTableLabel: String,
                                                    val aTattributeIdColumnLabel: String,
                                                    val aTattributeTypeIdColumnLabel: String,
                                                    val attributeTypeTableLabel: String,
                                                    val atTattributeTypeIdColumnLabel: String,
                                                    val attributeTypeColumnLabel: String
                                                  ) extends TablesQParams

  case object EdgeAttributeTablesQParams extends EdgeAttributeTablesQParams(
    typedAttributeValueTableLabel = "typedAttributeValueTableLabel",
    tavTattributeIdColumnLabel = "tavTattributeIdColumnLabel",
    typedAttributeValueColumnLabel = "typedAttributeValueColumnLabel",
    attributeTableLabel = "attributeTableLabel",
    aTattributeIdColumnLabel = "aTattributeIdColumnLabel",
    aTattributeTypeIdColumnLabel = "aTattributeTypeIdColumnLabel",
    attributeTypeTableLabel = "attributeTypeTableLabel",
    atTattributeTypeIdColumnLabel = "atTattributeTypeIdColumnLabel",
    attributeTypeColumnLabel = "attributeTypeColumnLabel"
    )

  case object VerticeAttributeTablesQParams extends VerticeAttributeTablesQParams(
    typedAttributeValueTableLabel = "typedAttributeValueTableLabel",
    tavTattributeIdColumnLabel = "tavTattributeIdColumnLabel",
    typedAttributeValueColumnLabel = "typedAttributeValueColumnLabel",
    attributeTableLabel = "attributeTableLabel",
    aTattributeIdColumnLabel = "aTattributeIdColumnLabel",
    aTattributeTypeIdColumnLabel = "aTattributeTypeIdColumnLabel",
    attributeTypeTableLabel = "attributeTypeTableLabel",
    atTattributeTypeIdColumnLabel = "atTattributeTypeIdColumnLabel",
    attributeTypeColumnLabel = "attributeTypeColumnLabel"
    )




  /**
   *
   *      TablesQString
   *
   */



  sealed abstract class TablesQString(val qs: String)

  case object VerticeAttributeTablesQString extends TablesQString(
    qs = {
      import VerticeAttributeTablesQParams._
      s"""
         |PREFIX rdbs: <https://data.elsevier.com/lifescience/schema/rdbs/>
         |PREFIX rdfs: <${RDFS.uri}>
         |SELECT
         |
         |?$typedAttributeValueTableLabel  ?$tavTattributeIdColumnLabel  ?$typedAttributeValueColumnLabel
         |
         |?$attributeTableLabel  ?$aTattributeIdColumnLabel ?$aTattributeTypeIdColumnLabel
         |
         |?$attributeTypeTableLabel  ?$atTattributeTypeIdColumnLabel  ?$attributeTypeColumnLabel
         |
         |WHERE {
         |
         |
         |
         |	#typedAttributeValueTable and columns
         |
         |	?typedAttributeValueTable
         |                            a rdbs:VerticeTypedAttributeValueTable ;
         |                            rdfs:label ?$typedAttributeValueTableLabel ;
         |                            rdbs:hasColumn ?tavTattributeIdColumn ;
         |                            rdbs:hasColumn ?typedAttributeValueColumn .
         |
         |  ?tavTattributeIdColumn
         |                            a rdbs:AttributeIdColumn ;
         |                            rdfs:label ?$tavTattributeIdColumnLabel .
         |
         |	?typedAttributeValueColumn
         |                            a rdbs:TypedAttributeValueColumn ;
         |                            rdfs:label ?$typedAttributeValueColumnLabel .
         |
         |
         |
         |
         |	#AttributeTable and columns
         |
         |	?attributeTable a rdbs:VerticeAttributeTable ;
         |                    rdfs:label ?$attributeTableLabel ;
         |                    rdbs:hasColumn ?aTattributeIdColumn ;
         |                    rdbs:hasColumn ?aTattributeTypeIdColumn .
         |
         |	?aTattributeIdColumn
         |                    a rdbs:AttributeIdColumn ;
         |                    rdfs:label ?$aTattributeIdColumnLabel .
         |
         |	?aTattributeTypeIdColumn
         |                    a rdbs:AttributeTypeIdColumn ;
         |                    rdfs:label ?$aTattributeTypeIdColumnLabel .
         |
         |
         |
         |	#AttributeTypeTable and columns
         |
         |	?attributeTypeTable a rdbs:AttributeTypeTable ;
         |                        rdfs:label ?$attributeTypeTableLabel ;
         |                        rdbs:hasColumn ?atTattributeTypeIdColumn;
         |                        rdbs:hasColumn ?attributeTypeColumn .
         |
         |	?atTattributeTypeIdColumn
         |                        a rdbs:AttributeTypeIdColumn;
         |                        rdfs:label ?$atTattributeTypeIdColumnLabel .
         |
         |
         |	?attributeTypeColumn
         |                        a rdbs:AttributeTypeColumn;
         |                        rdfs:label ?$attributeTypeColumnLabel .
         |
         |
         |
         |}""".stripMargin
    }
    )

  case object EdgeAttributeTablesQString extends TablesQString(
    qs = {
      import EdgeAttributeTablesQParams._
      s"""
         |PREFIX rdbs: <https://data.elsevier.com/lifescience/schema/rdbs/>
         |PREFIX rdfs: <${RDFS.uri}>
         |SELECT
         |
         |?$typedAttributeValueTableLabel  ?$tavTattributeIdColumnLabel  ?$typedAttributeValueColumnLabel
         |
         |?$attributeTableLabel  ?$aTattributeIdColumnLabel ?$aTattributeTypeIdColumnLabel
         |
         |?$attributeTypeTableLabel  ?$atTattributeTypeIdColumnLabel  ?$attributeTypeColumnLabel
         |
         |WHERE {
         |
         |
         |
         |	#typedAttributeValueTable and columns
         |
         |	?typedAttributeValueTable
         |                            a rdbs:EdgeTypedAttributeValueTable ;
         |                            rdfs:label ?$typedAttributeValueTableLabel ;
         |                            rdbs:hasColumn ?tavTattributeIdColumn ;
         |                            rdbs:hasColumn ?typedAttributeValueColumn .
         |
         |  ?tavTattributeIdColumn
         |                            a rdbs:AttributeIdColumn ;
         |                            rdfs:label ?$tavTattributeIdColumnLabel .
         |
         |	?typedAttributeValueColumn
         |                            a rdbs:TypedAttributeValueColumn ;
         |                            rdfs:label ?$typedAttributeValueColumnLabel .
         |
         |
         |
         |
         |	#AttributeTable and columns
         |
         |	?attributeTable a rdbs:EdgeAttributeTable ;
         |                    rdfs:label ?$attributeTableLabel ;
         |                    rdbs:hasColumn ?aTattributeIdColumn ;
         |                    rdbs:hasColumn ?aTattributeTypeIdColumn .
         |
         |	?aTattributeIdColumn
         |                    a rdbs:AttributeIdColumn ;
         |                    rdfs:label ?$aTattributeIdColumnLabel .
         |
         |	?aTattributeTypeIdColumn
         |                    a rdbs:AttributeTypeIdColumn ;
         |                    rdfs:label ?$aTattributeTypeIdColumnLabel .
         |
         |
         |
         |	#AttributeTypeTable and columns
         |
         |	?attributeTypeTable a rdbs:AttributeTypeTable ;
         |                        rdfs:label ?$attributeTypeTableLabel ;
         |                        rdbs:hasColumn ?atTattributeTypeIdColumn;
         |                        rdbs:hasColumn ?attributeTypeColumn .
         |
         |	?atTattributeTypeIdColumn
         |                        a rdbs:AttributeTypeIdColumn;
         |                        rdfs:label ?$atTattributeTypeIdColumnLabel .
         |
         |
         |	?attributeTypeColumn
         |                        a rdbs:AttributeTypeColumn;
         |                        rdfs:label ?$attributeTypeColumnLabel .
         |
         |
         |
         |}""".stripMargin
    }
    )







  /**
   *
   *      TablesQuery
   *
   */

  sealed abstract class TableQuery

  sealed abstract class EdgeAttributeTablesQuery(val qp: EdgeAttributeTablesQParams, val qString: EdgeAttributeTablesQString.type) extends TableQuery
  sealed abstract class VerticeAttributeTablesQuery(val qp: VerticeAttributeTablesQParams, val qString: VerticeAttributeTablesQString.type) extends TableQuery

  case object EdgeAttributeTablesQuery extends EdgeAttributeTablesQuery(EdgeAttributeTablesQParams, EdgeAttributeTablesQString)
  case object VerticeAttributeTablesQuery extends VerticeAttributeTablesQuery(VerticeAttributeTablesQParams, VerticeAttributeTablesQString)





}
