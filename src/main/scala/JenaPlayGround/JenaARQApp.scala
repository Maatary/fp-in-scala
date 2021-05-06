package JenaPlayGround

import cats.data.{Kleisli, StateT}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.apache.jena.query.{DatasetFactory, QueryExecutionFactory, QuerySolution}
import org.apache.jena.rdf.model.{Model, ModelFactory}
import cats.syntax.all._
import org.apache.jena.riot.Lang
import org.apache.jena.vocabulary.{RDF, RDFS}
import scala.jdk.CollectionConverters._
import scribe._

/**
 *  == Basic classes in ARQ ==
 *
 *
 *  --  '''Query''': Represents a single SPARQL query.
 *
 *  --  '''Dataset''': The knowledge base on which queries are executed (Equivalent to RDF Models)
 *
 *  --  '''DatasetFactory''': Factory to create Datasets
 *
 *  --  '''QueryFactory''': Factory to create Query objects from SPARQL strings
 *
 *  --  '''QueryExecution''': Represents one execution of a query. Provides methods for the execution of queries
 *
 *  --  '''QueryExecutionFactory''': Factory to create QueryExecution instances.
 *
 *  --  '''ResultSet''': Contains the results (''see QuerySolution'') obtained from an executed query (possibly many)
 *
 *  --  '''QuerySolution''': Represents a row of query results.
 *
 *  --  '''Note''' : When queries are executed over a model directly, the model is wrapped into a Dataset.
 *
 *  -- ''' ResultSetFormatter''':  Turn a ResultSet into various forms; into json, text, or as plain XML.
 *
 *
 *
 *
 *
 *  == Notes On Resource usage ==
 *
 *  -- `It is important to cleanly close the QueryExecution when finished.`
 *
 *  -- `System resources connected to persistent storage may need to be released.`
 *
 *  -- `As a Auto-Closable Resource, try-resources is used as a pattern by Jena for this.`
 *
 *  -- ''**Might be that when working against a memory model it is not necessary**.''
 *
 *  == Notes On usage ==
 *
 *  -- Use directly QueryExecutionFactory.create(queryString, model) which invoke QueryFactory
 *
 *  -- Updates to the model must be carried out after the query execution has finished.
 *
 *  -- Typically, this involves
 *
 *  - Collecting results of interest in a local dataStructure
 *
 *  - Looping over that structure after the query execution has finished and been closed.
 *
 *
 *
 */

object JenaARQApp extends App {

  import queryParameterExtraction._

  case class VerticeAttributeTypeQueryParameter( typedAttributeValueTable: String,
                                                 tavTattributeIdColumn: String,
                                                 typedAttributeValueColumn: String,
                                                 attributeTable: String,
                                                 aTattributeIdColumn: String,
                                                 aTattributeTypeIdColumn: String,
                                                 attributeTypeTable: String,
                                                 atTattributeTypeIdColumn: String,
                                                 attributeTypeColumn: String
                                               )

  def buildVATAQueryParamter(sol: QuerySolution): VerticeAttributeTypeQueryParameter = {
    VerticeAttributeTypeQueryParameter(
      sol.getLiteral("typedAttributeValueTableLabel").getString,
      sol.getLiteral("tavTattributeIdColumnLabel").getString,
      sol.getLiteral("typedAttributeValueColumnLabel").getString,
      sol.getLiteral("attributeTableLabel").getString,
      sol.getLiteral("aTattributeIdColumnLabel").getString,
      sol.getLiteral("aTattributeTypeIdColumnLabel").getString,
      sol.getLiteral("attributeTypeTableLabel").getString,
      sol.getLiteral("atTattributeTypeIdColumnLabel").getString,
      sol.getLiteral("attributeTypeColumnLabel").getString
    )
  }



  val prog = for {
    model        <- IO { ModelFactory.createDefaultModel() }
    _            <- IO { model.read("elsevier_entellect_enriched_dbschema_resnet_basic_with_tag.ttl") }

    qexec        <- IO { QueryExecutionFactory.create(verticeAttributeAssociationQuery, model ) }
    solSet       <- IO { qexec.execSelect() }
    t = solSet.asScala.toList.map(buildVATAQueryParamter(_))

    _            <- IO { qexec.close() }

    _            <- IO { println(t) }


  } yield ()

  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("program failed", value)
    case Right(_) => info("Model processed properly")
  }







object queryParameterExtraction {

  val verticeAttributeAssociationQuery
                        = s"""
                       |PREFIX rdbs: <https://data.elsevier.com/lifescience/schema/rdbs/>
                       |PREFIX rdfs: <${RDFS.uri}>
                       SELECT
                       |
                       |?typedAttributeValueTableLabel  ?tavTattributeIdColumnLabel  ?typedAttributeValueColumnLabel
                       |
                       |?attributeTableLabel  ?aTattributeIdColumnLabel ?aTattributeTypeIdColumnLabel
                       |
                       |?attributeTypeTableLabel  ?atTattributeTypeIdColumnLabel  ?attributeTypeColumnLabel
                       |
                       |WHERE {
                       |
                       |
                       |
                       |	#typedAttributeValueTable and columns
                       |
                       |	?typedAttributeValueTable
                       |                            a rdbs:VerticeTypedAttributeValueTable ;
                       |                            rdfs:label ?typedAttributeValueTableLabel ;
                       |                            rdbs:hasColumn ?tavTattributeIdColumn ;
                       |                            rdbs:hasColumn ?typedAttributeValueColumn .
                       |
                       |  ?tavTattributeIdColumn
                       |                            a rdbs:AttributeIdColumn ;
                       |                            rdfs:label ?tavTattributeIdColumnLabel .
                       |
                       |	?typedAttributeValueColumn
                       |                            a rdbs:TypedAttributeValueColumn ;
                       |                            rdfs:label ?typedAttributeValueColumnLabel .
                       |
                       |
                       |
                       |
                       |	#AttributeTable and columns
                       |
                       |	?attributeTable a rdbs:VerticeAttributeTable ;
                       |                    rdfs:label ?attributeTableLabel ;
                       |                    rdbs:hasColumn ?aTattributeIdColumn ;
                       |                    rdbs:hasColumn ?aTattributeTypeIdColumn .
                       |
                       |	?aTattributeIdColumn
                       |                    a rdbs:AttributeIdColumn ;
                       |                    rdfs:label ?aTattributeIdColumnLabel .
                       |
                       |	?aTattributeTypeIdColumn
                       |                    a rdbs:AttributeTypeIdColumn ;
                       |                    rdfs:label ?aTattributeTypeIdColumnLabel .
                       |
                       |
                       |
                       |	#AttributeTypeTable and columns
                       |
                       |	?attributeTypeTable a rdbs:AttributeTypeTable ;
                       |                        rdfs:label ?attributeTypeTableLabel ;
                       |                        rdbs:hasColumn ?atTattributeTypeIdColumn;
                       |                        rdbs:hasColumn ?attributeTypeColumn .
                       |
                       |	?atTattributeTypeIdColumn
                       |                        a rdbs:AttributeTypeIdColumn;
                       |                        rdfs:label ?atTattributeTypeIdColumnLabel .
                       |
                       |
                       |	?attributeTypeColumn
                       |                        a rdbs:AttributeTypeColumn;
                       |                        rdfs:label ?attributeTypeColumnLabel .
                       |
                       |
                       |
                       |}""".stripMargin


}




}
