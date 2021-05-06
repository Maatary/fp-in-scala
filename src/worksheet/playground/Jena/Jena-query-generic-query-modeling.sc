import org.apache.jena.vocabulary.RDFS


//object EdgeAttributeSchemaQParams

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







sealed abstract class TablesQString
sealed abstract class EdgeAttributeTablesQString(val qs: String)
case object EdgeAttributeTablesQString extends EdgeAttributeTablesQString(
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


sealed abstract class SparqlQuery(val qp: EdgeAttributeTablesQParams, val qString: EdgeAttributeTablesQString)
case object EdgeAttributeTablesQuery extends SparqlQuery(EdgeAttributeTablesQParams, EdgeAttributeTablesQString)


