package jenaPlayGround

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

  import dataTypes.TablesQParamsDataTypes._






  val prog = for {
    model        <- IO { ModelFactory.createDefaultModel() }
    _            <- IO { model.read("elsevier_entellect_enriched_dbschema_resnet_basic_with_tag.ttl") }
    lv           <- execVerticeAttributeTablesQuery(model)
    le           <- execEdgeAttributeTablesQuery(model)
    _            <- IO { println(lv) }
    _            <- IO { println(le) }


  } yield ()

  prog.attempt.unsafeRunSync() match {
    case Left(value) => error("program failed", value)
    case Right(_) => info("Model processed properly")
  }



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

  case class EdgeAttributeTypeQueryParameter(    typedAttributeValueTable: String,
                                                 tavTattributeIdColumn: String,
                                                 typedAttributeValueColumn: String,
                                                 attributeTable: String,
                                                 aTattributeIdColumn: String,
                                                 aTattributeTypeIdColumn: String,
                                                 attributeTypeTable: String,
                                                 atTattributeTypeIdColumn: String,
                                                 attributeTypeColumn: String
                                               )

  def execVerticeAttributeTablesQuery(model: Model): IO[List[VerticeAttributeTypeQueryParameter]] = {

    import VerticeAttributeTablesQuery._
    for {

      qexec        <- IO { QueryExecutionFactory.create(qString.qs, model ) }

      solSet       <- IO { qexec.execSelect() }

      l = solSet.asScala.toList.map{ sol =>

        VerticeAttributeTypeQueryParameter(
          sol.getLiteral(qp.typedAttributeValueTableLabel).getString,
          sol.getLiteral(qp.tavTattributeIdColumnLabel).getString,
          sol.getLiteral(qp.typedAttributeValueColumnLabel).getString,
          sol.getLiteral(qp.attributeTableLabel).getString,
          sol.getLiteral(qp.aTattributeIdColumnLabel).getString,
          sol.getLiteral(qp.aTattributeTypeIdColumnLabel).getString,
          sol.getLiteral(qp.attributeTypeTableLabel).getString,
          sol.getLiteral(qp.atTattributeTypeIdColumnLabel).getString,
          sol.getLiteral(qp.attributeTypeColumnLabel).getString
          )

      }
      _            <- IO { qexec.close() }

    } yield l

  }

  def execEdgeAttributeTablesQuery(model: Model): IO[List[EdgeAttributeTypeQueryParameter]] = {

    import EdgeAttributeTablesQuery._
    for {

      qexec        <- IO { QueryExecutionFactory.create(qString.qs, model ) }

      solSet       <- IO { qexec.execSelect() }

      l = solSet.asScala.toList.map{ sol =>

        EdgeAttributeTypeQueryParameter(
          sol.getLiteral(qp.typedAttributeValueTableLabel).getString,
          sol.getLiteral(qp.tavTattributeIdColumnLabel).getString,
          sol.getLiteral(qp.typedAttributeValueColumnLabel).getString,
          sol.getLiteral(qp.attributeTableLabel).getString,
          sol.getLiteral(qp.aTattributeIdColumnLabel).getString,
          sol.getLiteral(qp.aTattributeTypeIdColumnLabel).getString,
          sol.getLiteral(qp.attributeTypeTableLabel).getString,
          sol.getLiteral(qp.atTattributeTypeIdColumnLabel).getString,
          sol.getLiteral(qp.attributeTypeColumnLabel).getString
          )

      }
      _            <- IO { qexec.close() }

    } yield l

  }



}
