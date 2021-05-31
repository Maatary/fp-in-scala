package jenaPlayGround

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.rdf.model.ModelFactory
import org.apache.jena.sparql.pfunction.PropertyFunctionRegistry
import org.apache.jena.sparql.pfunction.library.strSplit
import org.apache.jena._
import org.apache.jena.graph.compose.MultiUnion
import org.apache.jena.query.ARQ
import org.apache.jena.sparql.mgt.Explain
import org.topbraid.shacl.arq.SHACLFunctions
import org.topbraid.shacl.util.SHACLSystemModel
import cats.syntax.all._
import org.apache.jena.riot.Lang

import scala.jdk.CollectionConverters._
import scribe._

import scala.util.chaining.scalaUtilChainingOps


object JenaQueryMeasurement extends App {

  scribe.Logger.root
        .clearHandlers()
        .clearModifiers()
        .withHandler(minimumLevel = Some(Level.Info))
        .replace()






  val queryNoMagic =
    """
      |PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
      |PREFIX rdbs:<https://data.elsevier.com/lifescience/schema/rdbs/>
      |PREFIX sh:<http://www.w3.org/ns/shacl#>
      |
      |SELECT ?table_label ?dynamic_attribute_type_column_label
      |
      |WHERE {
      |# Fint Input Topic Table
      |	?this a rdbs:VerticeWithTypeTable ;
      |		rdfs:label ?origin ;
      |		rdbs:hasColumn ?vertice_type_column .
      |	?vertice_type_column a rdbs:ObjectTypeColumn ;
      |		rdfs:label ?vertice_type_column_label .
      |
      |# Get all the Vertice Types
      |	?vertice_type a rdbs:VerticeType ;
      |		rdfs:label ?vertice_type_label .
      |
      |# Mint Vertice CBE IRI
      |	BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label, rdbs:VerticeCBETable ) AS ?table_label ) .
      |	BIND ( rdbs:createIRIFromString( ?table_label, "base" ) AS ?table ) .
      |
      |## Temporary Filter Fix until Kemo Add topology based filter.
      |	#FILTER NOT EXISTS { ?table a rdbs:VerticeCBETable }
      |
      |# Get the associated Vertice Type Associations and mint related IRIs
      |    OPTIONAL
      |	{
      |		?vertice_type_association a rdbs:VerticeTypeAssociation ;
      |								  rdbs:hasVerticeType ?vertice_type ;
      |								  rdbs:hasAttributeType ?attribute_type ;
      |								  rdbs:hasAttributeDataType ?attribute_data_type .
      |
      |		?attribute_type rdfs:label ?attribute_type_label .
      |		?attribute_data_type rdfs:label ?attribute_data_type_label .
      |
      |		# Find Associated Data Type
      |
      |		?typed_attribute_value_table a rdbs:EdgeTypedAttributeValueTable ;
      |				rdbs:hasColumn [
      |					a rdbs:TypedAttributeValueColumn ;
      |					rdfs:label ?attribute_data_type_label ;
      |					rdbs:hasDataType ?dynamic_attribute_type_column_data_type
      |				] .
      |
      |		#BIND ( rdbs:mintClassifiedColumn( ?table_label, ?attribute_data_type_label, ?attribute_type ) AS ?dynamic_attribute_type_column ) .
      |
      |		#BIND ( rdbs:mintClassifiedColumnLabel( ?attribute_data_type_label, ?attribute_type ) AS ?dynamic_attribute_type_column_label ) .
      |
      |   BIND(CONCAT(?attribute_data_type_label,"_",?attribute_type_label) AS ?dynamic_attribute_type_column_label ) .
      |
      |	}
      |
      |}
      |""".stripMargin

  val queryMagic =
    """
      |PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
      |PREFIX rdbs:<https://data.elsevier.com/lifescience/schema/rdbs/>
      |PREFIX sh:<http://www.w3.org/ns/shacl#>
      |PREFIX spif: <http://spinrdf.org/spif#>
      |
      |SELECT ?table_label ?dynamic_attribute_type_column_label
      |
      |WHERE {
      |
      |# Fint Input Topic Table
      |	?this a rdbs:VerticeWithTypeTable ;
      |		rdfs:label ?origin ;
      |		rdbs:hasColumn ?vertice_type_column .
      |	?vertice_type_column a rdbs:ObjectTypeColumn ;
      |		rdfs:label ?vertice_type_column_label .
      |
      |# Get all the Vertice Types
      |	?vertice_type a rdbs:VerticeType ;
      |		rdfs:label ?vertice_type_label .
      |
      |# Mint Vertice CBE IRI
      |	BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label, rdbs:VerticeCBETable ) AS ?table_label ) .
      |	BIND ( rdbs:createIRIFromString( ?table_label, "base" ) AS ?table ) .
      |
      |# Get the associated Vertice Type Associations and mint related IRIs
      |    OPTIONAL
      |	{
      |
      |        {
      |            SELECT ?vertice_type (group_concat(?attribute_type_and_data_type_label; separator=";") AS ?list_dynamic_attribute_type_column_label)
      |            WHERE {
      |
      |                ?vertice_type_association a rdbs:VerticeTypeAssociation ;
      |								  rdbs:hasVerticeType ?vertice_type ;
      |								  rdbs:hasAttributeType ?attribute_type ;
      |								  rdbs:hasAttributeDataType ?attribute_data_type .
      |
      |                ?attribute_type rdfs:label ?attribute_type_label .
      |                ?attribute_data_type rdfs:label ?attribute_data_type_label .
      |
      |				BIND(CONCAT(?attribute_type_label, ":", ?attribute_data_type_label) AS ?attribute_type_and_data_type_label)
      |
      |            } GROUP BY ?vertice_type
      |        }
      |
      |        ?attribute_type_and_data_type_label spif:split (?list_dynamic_attribute_type_column_label  ";").
      |		BIND(STRBEFORE(?attribute_type_and_data_type_label, ":") AS ?attribute_type_label)
      |		BIND(STRAFTER(?attribute_type_and_data_type_label, ":") AS ?attribute_data_type_label)
      |
      |		?typed_attribute_value_table a rdbs:EdgeTypedAttributeValueTable ;
      |				rdbs:hasColumn [
      |					a rdbs:TypedAttributeValueColumn ;
      |					rdfs:label ?attribute_data_type_label ;
      |					rdbs:hasDataType ?dynamic_attribute_type_column_data_type
      |				] .
      |
      |		BIND(CONCAT(?attribute_data_type_label,"_",?attribute_type_label) AS ?dynamic_attribute_type_column_label ) .
      |
      |	}
      |
      |}
      |""".stripMargin

  def execSelectQuery(query: String) = {

    for {

      //Just registering Shacl functions
      rdbsShapesModel   <- IO { ModelFactory.createDefaultModel()}
      _                 <- IO { rdbsShapesModel.read("elsevier_entellect_schema_rdbs.ttl") }
      shaclSystemModel  <- IO { SHACLSystemModel.getSHACLModel }
      shapesUnionGraph  <- IO { new MultiUnion(Array(shaclSystemModel.getGraph, rdbsShapesModel.getGraph))}
      shapesModel       <- IO { ModelFactory.createModelForGraph(shapesUnionGraph)}
      _                 <- IO { SHACLFunctions.registerFunctions(shapesModel)}

      //Registering Custom function and setting the log level
      //_                 <- IO { ARQ.setExecutionLogging(Explain.InfoLevel.ALL) }
      _                 <- IO { PropertyFunctionRegistry.get().put("http://spinrdf.org/spif#split", classOf[strSplit]) }


      model             <- IO { ModelFactory.createDefaultModel()}
      _                 <- IO { model.read("elsevier_entellect_enriched_dbschema_resnet_basic_with_tag_with_dynamic.ttl")}
      _                 <- IO { model.read("topologyInferenceModel.ttl") }

      qexec             <- IO  { QueryExecutionFactory.create(query, model ) }


      cResult            <- IO {

        val startTime = System.currentTimeMillis()
        val solSet    = qexec.execSelect()
        val solList   = solSet.asScala.toList
        val resList   = solList.map{ sol =>
          sol.varNames().asScala.toList.size pipe {
            case 1 => sol.getLiteral("table_label").getString -> ""
            case _ => sol.getLiteral("table_label").getString -> sol.getLiteral("dynamic_attribute_type_column_label").getString
          }
        }

        qexec.close()

        val endTime = System.currentTimeMillis()

        (resList, startTime, endTime)

      }

      (resList, startTime, endTime) = cResult

      _                 <-  IO { println(resList.toString()) }

      execTime          <-  IO { endTime - startTime}


    } yield execTime

  }

  /*(execSelectQuery(queryMagic) flatTap(excTime => IO{info(s"Execution time for queryMagic was: $excTime \n")}),
    execSelectQuery(queryNoMagic) flatTap(excTime => IO{info(s"Execution time for queryNoMagic was: $excTime \n")})).parTupled.unsafeRunSync()*/

  /*(for {

    _ <- execQuery(queryMagic) flatTap(excTime => IO{ info(s"Execution time for queryMagic was: $excTime \n")})

    _ <- execQuery(queryNoMagic) flatTap(excTime => IO{ info(s"Execution time for queryNoMagic was: $excTime \n")})

  } yield()).unsafeRunSync()*/


  val unionQuery =
    """
      |PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
      |PREFIX rdbs:<https://data.elsevier.com/lifescience/schema/rdbs/>
      |PREFIX sh:<http://www.w3.org/ns/shacl#>
      |
      |CONSTRUCT {
      |# The base node to describe topologies of temporary topics
      |	?topology a rdbs:Topology ;
      |			rdbs:hasOutput ?output ;
      |			rdbs:hasTopic ?origin ;
      |			rdbs:hasJoin ?membership_join ;
      |			rdbs:hasJoin ?attribute_type_join .
      |# The description of the membership join element of the topology
      |    ?membership_join a rdbs:Join ;
      |			rdbs:hasTopic ?membership_topic ;
      |			rdbs:hasKind rdbs:LeftJoin .
      |# The description of the attribute type join element of the topology
      |    ?attribute_type_join a rdbs:Join ;
      |			rdbs:hasTopic ?attribute_type_topic ;
      |			rdbs:hasKind rdbs:LeftJoin .
      |# The description of the table or topic resulting from the join
      |	?table a rdbs:VerticeCBETable ;
      |			rdbs:createdWith ?topology ;
      |			rdfs:label ?table_label ;
      |# Adding the copies of the origin topic columns and the selected foreign topic columns
      |			rdbs:hasColumn ?copied_static_membership_column ;
      |			rdbs:hasColumn ?dynamic_membership_vertice_container_column ;
      |			rdbs:hasColumn ?dynamic_membership_vertice_containee_column ;
      |			rdbs:hasColumn ?dynamic_membership_edge_containee_column ;
      |			rdbs:hasColumn ?dynamic_attribute_type_column .
      |# Set up copies of the origin columns
      |	?copied_static_membership_column rdbs:belongsToTable ?table ;
      |		?static_membership_predicate ?static_membership_object .
      |# Set up Vertice Container Id Per Vertice Container Type Columns
      |	?dynamic_membership_vertice_container_column a rdbs:ContainerVerticeIdPerVerticeTypeColumn ;
      |		a rdbs:ObjectPropertyColumn ;
      |#		a rdbs:ForeignKeyColumn ;
      |#		rdbs:refersToPrimaryKey ?vertice_primary_key_column;
      |		rdfs:label ?dynamic_membership_vertice_container_column_label ;
      |		rdbs:hasObjectSemanticName ?container_vertice_type_label ;
      |		rdbs:hasSemanticName "isMemberOf" ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType rdbs:INT ;
      |		rdbs:isNullable false .
      |# Set up Vertice Containee Id Per Vertice Containee Type Columns
      |	?dynamic_membership_vertice_containee_column a rdbs:ContaineeVerticeIdPerVerticeTypeColumn ;
      |		a rdbs:ObjectPropertyColumn ;
      |#		a rdbs:ForeignKeyColumn ;
      |#		rdbs:refersToPrimaryKey ?vertice_primary_key_column;
      |		rdfs:label ?dynamic_membership_vertice_containee_column_label ;
      |		rdbs:hasObjectSemanticName ?containee_vertice_type_label ;
      |		rdbs:hasSemanticName "contains" ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType rdbs:INT ;
      |		rdbs:isNullable false .
      |# Set up Edge Containee Id Per Edge Containee Type Columns
      |	?dynamic_membership_edge_containee_column a rdbs:ContaineeEdgeIdPerEdgeTypeColumn ;
      |		a rdbs:ObjectPropertyColumn ;
      |#		a rdbs:ForeignKeyColumn ;
      |#		rdbs:refersToPrimaryKey ?vertice_primary_key_column;
      |		rdfs:label ?dynamic_membership_edge_containee_column_label ;
      |		rdbs:hasObjectSemanticName ?containee_edge_type_label ;
      |		rdbs:hasSemanticName "contains" ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType rdbs:INT ;
      |		rdbs:isNullable false .
      |# Set up Typed Attribute Value Per Attribute Type Columns
      |	?dynamic_attribute_type_column a rdbs:TypedAttributeValuePerAttributeTypeColumn ;
      |		a rdbs:DataPropertyColumn ;
      |		rdfs:label ?dynamic_attribute_type_column_label ;
      |		rdbs:hasSemanticName ?attribute_type_label ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType ?dynamic_attribute_type_column_data_type ;
      |		rdbs:isNullable false .
      |}
      |WHERE {
      |# Fint Input Topic Table
      |	?this a rdbs:VerticeWithTypeTable ;
      |		rdfs:label ?origin ;
      |		rdbs:hasColumn ?vertice_type_column .
      |	?vertice_type_column a rdbs:ObjectTypeColumn ;
      |		rdfs:label ?vertice_type_column_label .
      |# Mint output topic
      |	BIND ( CONCAT( "${", ?vertice_type_column_label, "}" ) AS ?output ) .
      |# Find Membership Topic Table
      |	?membership_table a rdbs:VerticeWithTypeWithMembershipTable ;
      |		rdfs:label ?membership_topic ;
      |		rdbs:hasColumn ?static_membership_column .
      |# Get only the static data of the membership table
      |	FILTER NOT EXISTS { ?static_membership_column a rdbs:ContainerVerticeIdPerVerticeTypeColumn }
      |	FILTER NOT EXISTS { ?static_membership_column a rdbs:ContaineeVerticeIdPerVerticeTypeColumn }
      |	FILTER NOT EXISTS { ?static_membership_column a rdbs:ContaineeEdgeIdPerEdgeTypeColumn }
      |# Get the predicates and objects of the static data
      |?static_membership_column ?static_membership_predicate ?static_membership_object .
      |# Filter the table belonging
      |	FILTER ( ?static_membership_predicate != rdbs:belongsToTable )
      |# Find Typed Attribute Value Per Attribute Type Topics
      |	?dynamic_attribute_type_table a rdbs:TypedVerticeValuePerAttributeTypeTable ;
      |		rdfs:label ?attribute_type_topic .
      |# Create Topology
      |	BIND ( rdbs:mintTopologyJoin( "vertice-CBE" ) AS ?topology ) .
      |	BIND ( rdbs:mintTopologyJoin( "vertice-CBE" , ?membership_topic ) AS ?membership_join ) .
      |	BIND ( rdbs:mintTopologyJoin( "vertice-CBE" , ?attribute_type_topic ) AS ?attribute_type_join ) .
      |# Get all the Vertice Types
      |	?vertice_type a rdbs:VerticeType ;
      |		rdfs:label ?vertice_type_label .
      |# Mint Vertice CBE IRI
      |	BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label, rdbs:VerticeCBETable ) AS ?table_label ) .
      |	BIND( rdbs:createIRIFromString( ?table_label, "base" ) AS ?table ) .
      |
      |## Temporary Filter Fix until Kemo Add topology based filter.
      |	#FILTER NOT EXISTS { ?table a rdbs:VerticeCBETable }
      |
      |# Create the copies of the static data columns
      |	BIND (  rdbs:mintCopyOfColumn(?static_membership_column, ?table_label) AS ?copied_static_membership_column ) .
      |# Get the associated Vertice Type Associations and mint related IRIs
      |    OPTIONAL
      |	{
      |        ?membership_type_association a rdbs:MembershipTypeAssociation .
      |
      |        {
      |
      |            ?membership_type_association rdbs:ContainerVerticeType ?vertice_type . #Container
      |            {
      |                ?membership_type_association rdbs:ContaineeVerticeType ?containee_vertice_type . ## Vertice
      |                ?containee_vertice_type rdfs:label ?containee_vertice_type_label .
      |# Create Dynamic Data Column
      |                ?vertice_type rdfs:label ?vertice_type_label_optional .
      |                BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |                BIND ( rdbs:mintClassifiedColumn( ?table_label, "contains", ?containee_vertice_type ) AS ?dynamic_membership_vertice_containee_column ) .
      |                BIND ( rdbs:mintClassifiedColumnLabel( "contains", ?containee_vertice_type ) AS ?dynamic_membership_vertice_containee_column_label ) .
      |            }
      |            UNION ##Either the The Containee is an Edge or a Vertice
      |            {
      |                ?membership_type_association rdbs:ContaineeEdgeType ?containee_edge_type . ##Edge
      |                ?containee_edge_type rdfs:label ?containee_edge_type_label .
      |# Create Dynamic Data Column
      |                ?vertice_type rdfs:label ?vertice_type_label_optional .
      |                BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |                BIND ( rdbs:mintClassifiedColumn( ?table_label, "contains", ?containee_edge_type ) AS ?dynamic_membership_edge_containee_column ) .
      |                BIND ( rdbs:mintClassifiedColumnLabel( "contains", ?containee_edge_type ) AS ?dynamic_membership_edge_containee_column_label ) .
      |            }
      |
      |        }
      |
      |		UNION ## Either the VerticeType is a Container or Containee
      |
      |		{
      |            ?membership_type_association rdbs:ContaineeVerticeType ?vertice_type .  #Containee
      |            ?membership_type_association rdbs:ContainerVerticeType ?container_vertice_type .
      |            ?container_vertice_type rdfs:label ?container_vertice_type_label .
      |# Create Dynamic Data Column
      |            ?vertice_type rdfs:label ?vertice_type_label_optional .
      |            BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |            BIND ( rdbs:mintClassifiedColumn( ?table_label, "isMemberOf", ?container_vertice_type ) AS ?dynamic_membership_vertice_container_column ) .
      |            BIND ( rdbs:mintClassifiedColumnLabel( "isMemberOf", ?container_vertice_type ) AS ?dynamic_membership_vertice_container_column_label ) .
      |        }
      |
      |		UNION
      |		{
      |			?vertice_type_association a rdbs:VerticeTypeAssociation ;
      |				rdbs:hasVerticeType ?vertice_type ;
      |				rdbs:hasAttributeType ?attribute_type ;
      |				rdbs:hasAttributeDataType ?attribute_data_type .
      |			?attribute_type rdfs:label ?attribute_type_label .
      |			?attribute_data_type rdfs:label ?attribute_data_type_label .
      |# Find Associated Data Type
      |			?typed_attribute_value_table a rdbs:EdgeTypedAttributeValueTable ;
      |				rdbs:hasColumn [
      |					a rdbs:TypedAttributeValueColumn ;
      |					rdfs:label ?attribute_data_type_label ;
      |					rdbs:hasDataType ?dynamic_attribute_type_column_data_type
      |				] .
      |# Get the associated Membership Type Associations and mint related IRIs
      |        	?vertice_type rdfs:label ?vertice_type_label_optional .
      |        	BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |			BIND ( rdbs:mintClassifiedColumn( ?table_label, ?attribute_data_type_label, ?attribute_type ) AS ?dynamic_attribute_type_column ) .
      |			BIND ( rdbs:mintClassifiedColumnLabel( ?attribute_data_type_label, ?attribute_type ) AS ?dynamic_attribute_type_column_label ) .
      |		}
      |
      |
      |    } #Optional
      |}
      |""".stripMargin

  val experimentalQuery =
    """
      |PREFIX rdfs:<http://www.w3.org/2000/01/rdf-schema#>
      |PREFIX rdf:   <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      |PREFIX owl: <http://www.w3.org/2002/07/owl#>
      |PREFIX rdbs:<https://data.elsevier.com/lifescience/schema/rdbs/>
      |PREFIX sh:<http://www.w3.org/ns/shacl#>
      |
      |CONSTRUCT {
      |# The base node to describe topologies of temporary topics
      |	?topology a rdbs:Topology ;
      |			rdbs:hasOutput ?output ;
      |			rdbs:hasTopic ?origin ;
      |			rdbs:hasJoin ?membership_join ;
      |			rdbs:hasJoin ?attribute_type_join .
      |# The description of the membership join element of the topology
      |    ?membership_join a rdbs:Join ;
      |			rdbs:hasTopic ?membership_topic ;
      |			rdbs:hasKind rdbs:LeftJoin .
      |# The description of the attribute type join element of the topology
      |    ?attribute_type_join a rdbs:Join ;
      |			rdbs:hasTopic ?attribute_type_topic ;
      |			rdbs:hasKind rdbs:LeftJoin .
      |# The description of the table or topic resulting from the join
      |	?table a rdbs:VerticeCBETable ;
      |			rdbs:createdWith ?topology ;
      |			rdfs:label ?table_label ;
      |# Adding the copies of the origin topic columns and the selected foreign topic columns
      |			rdbs:hasColumn ?copied_static_membership_column ;
      |			rdbs:hasColumn ?dynamic_membership_vertice_container_column ;
      |			rdbs:hasColumn ?dynamic_membership_vertice_containee_column ;
      |			rdbs:hasColumn ?dynamic_membership_edge_containee_column ;
      |			rdbs:hasColumn ?dynamic_attribute_type_column .
      |# Set up copies of the origin columns
      |	?copied_static_membership_column rdbs:belongsToTable ?table ;
      |		?static_membership_predicate ?static_membership_object .
      |# Set up Vertice Container Id Per Vertice Container Type Columns
      |	?dynamic_membership_vertice_container_column a rdbs:ContainerVerticeIdPerVerticeTypeColumn ;
      |		a rdbs:ObjectPropertyColumn ;
      |#		a rdbs:ForeignKeyColumn ;
      |#		rdbs:refersToPrimaryKey ?vertice_primary_key_column;
      |		rdfs:label ?dynamic_membership_vertice_container_column_label ;
      |		rdbs:hasObjectSemanticName ?container_vertice_type_label ;
      |		rdbs:hasSemanticName "isMemberOf" ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType rdbs:INT ;
      |		rdbs:isNullable false .
      |# Set up Vertice Containee Id Per Vertice Containee Type Columns
      |	?dynamic_membership_vertice_containee_column a rdbs:ContaineeVerticeIdPerVerticeTypeColumn ;
      |		a rdbs:ObjectPropertyColumn ;
      |#		a rdbs:ForeignKeyColumn ;
      |#		rdbs:refersToPrimaryKey ?vertice_primary_key_column;
      |		rdfs:label ?dynamic_membership_vertice_containee_column_label ;
      |		rdbs:hasObjectSemanticName ?containee_vertice_type_label ;
      |		rdbs:hasSemanticName "contains" ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType rdbs:INT ;
      |		rdbs:isNullable false .
      |# Set up Edge Containee Id Per Edge Containee Type Columns
      |	?dynamic_membership_edge_containee_column a rdbs:ContaineeEdgeIdPerEdgeTypeColumn ;
      |		a rdbs:ObjectPropertyColumn ;
      |#		a rdbs:ForeignKeyColumn ;
      |#		rdbs:refersToPrimaryKey ?vertice_primary_key_column;
      |		rdfs:label ?dynamic_membership_edge_containee_column_label ;
      |		rdbs:hasObjectSemanticName ?containee_edge_type_label ;
      |		rdbs:hasSemanticName "contains" ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType rdbs:INT ;
      |		rdbs:isNullable false .
      |# Set up Typed Attribute Value Per Attribute Type Columns
      |	?dynamic_attribute_type_column a rdbs:TypedAttributeValuePerAttributeTypeColumn ;
      |		a rdbs:DataPropertyColumn ;
      |		rdfs:label ?dynamic_attribute_type_column_label ;
      |		rdbs:hasSemanticName ?attribute_type_label ;
      |		rdbs:belongsToTable ?table ;
      |		rdbs:hasDataType ?dynamic_attribute_type_column_data_type ;
      |		rdbs:isNullable false .
      |}
      |WHERE {
      |{
      |# Get all the Vertice Types
      |	?vertice_type a rdbs:VerticeType ;
      |		rdfs:label ?vertice_type_label .
      |# Mint Vertice CBE IRI
      |	BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label, rdbs:VerticeCBETable ) AS ?table_label ) .
      |	BIND( rdbs:createIRIFromString( ?table_label, "base" ) AS ?table ) .
      |	#FILTER NOT EXISTS {?table a rdbs:VerticeCBETable}
      |# Fint Input Topic Table
      |	?this a rdbs:VerticeWithTypeTable ;
      |		rdfs:label ?origin ;
      |		rdbs:hasColumn ?vertice_type_column .
      |	?vertice_type_column a rdbs:ObjectTypeColumn ;
      |		rdfs:label ?vertice_type_column_label .
      |# Mint output topic
      |	BIND ( CONCAT( "${", ?vertice_type_column_label, "}" ) AS ?output ) .
      |# Find Membership Topic Table
      |	?membership_table a rdbs:VerticeWithTypeWithMembershipTable ;
      |		rdfs:label ?membership_topic ;
      |		rdbs:hasColumn ?static_membership_column .
      |# Get only the static data of the membership table
      |	FILTER NOT EXISTS { ?static_membership_column a rdbs:ContainerVerticeIdPerVerticeTypeColumn }
      |	FILTER NOT EXISTS { ?static_membership_column a rdbs:ContaineeVerticeIdPerVerticeTypeColumn }
      |	FILTER NOT EXISTS { ?static_membership_column a rdbs:ContaineeEdgeIdPerEdgeTypeColumn }
      |# Get the predicates and objects of the static data
      |?static_membership_column ?static_membership_predicate ?static_membership_object .
      |# Filter the table belonging
      |	FILTER ( ?static_membership_predicate != rdbs:belongsToTable )
      |# Find Typed Attribute Value Per Attribute Type Topics
      |	?dynamic_attribute_type_table a rdbs:TypedVerticeValuePerAttributeTypeTable ;
      |		rdfs:label ?attribute_type_topic .
      |# Create Topology
      |	BIND ( rdbs:mintTopologyJoin( "vertice-CBE" ) AS ?topology ) .
      |	BIND ( rdbs:mintTopologyJoin( "vertice-CBE" , ?membership_topic ) AS ?membership_join ) .
      |	BIND ( rdbs:mintTopologyJoin( "vertice-CBE" , ?attribute_type_topic ) AS ?attribute_type_join ) .
      |	BIND ( rdbs:mintCopyOfColumn(?static_membership_column, ?table_label) AS ?copied_static_membership_column ) .
      |}
      |UNION
      |{
      |# Get all the Vertice Types
      |	?vertice_type a rdbs:VerticeType ;
      |		rdfs:label ?vertice_type_label .
      |# Mint Vertice CBE IRI
      |	BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label, rdbs:VerticeCBETable ) AS ?table_label ) .
      |	BIND( rdbs:createIRIFromString( ?table_label, "base" ) AS ?table ) .
      |	#FILTER NOT EXISTS {?table a rdbs:VerticeCBETable}
      |	?membership_table a rdbs:VerticeWithTypeWithMembershipTable ;
      |# Create the copies of the static data columns
      |#	BIND (  rdbs:mintCopyOfColumn(?static_membership_column, ?table_label) AS ?copied_static_membership_column ) .
      |# Get the associated Vertice Type Associations and mint related IRIs
      |	OPTIONAL {
      |		?vertice_type_association a rdbs:VerticeTypeAssociation ;
      |			rdbs:hasVerticeType ?vertice_type ;
      |			rdbs:hasAttributeType ?attribute_type ;
      |			rdbs:hasAttributeDataType ?attribute_data_type .
      |		?attribute_type rdfs:label ?attribute_type_label .
      |		?attribute_data_type rdfs:label ?attribute_data_type_label .
      |# Find Associated Data Type
      |		?typed_attribute_value_table a rdbs:EdgeTypedAttributeValueTable ;
      |#			rdbs:hasAttributeDataType ?attribute_data_type ;
      |			rdbs:hasColumn [
      |				a rdbs:TypedAttributeValueColumn ;
      |				rdfs:label ?attribute_data_type_label ;
      |				rdbs:hasDataType ?dynamic_attribute_type_column_data_type
      |			] .
      |# Get the associated Membership Type Associations and mint related IRIs
      |        ?vertice_type rdfs:label ?vertice_type_label_optional .
      |        BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |		BIND ( rdbs:mintClassifiedColumn( ?table_label, ?attribute_data_type_label, ?attribute_type ) AS ?dynamic_attribute_type_column ) .
      |		BIND ( rdbs:mintClassifiedColumnLabel( ?attribute_data_type_label, ?attribute_type ) AS ?dynamic_attribute_type_column_label ) .
      |	}
      |}
      |UNION
      |{
      |# Get all the Vertice Types
      |	?vertice_type a rdbs:VerticeType ;
      |		rdfs:label ?vertice_type_label .
      |# Mint Vertice CBE IRI
      |	BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label, rdbs:VerticeCBETable ) AS ?table_label ) .
      |	BIND( rdbs:createIRIFromString( ?table_label, "base" ) AS ?table ) .
      |	#FILTER NOT EXISTS {?table a rdbs:VerticeCBETable}
      |	?membership_table a rdbs:VerticeWithTypeWithMembershipTable ;
      |    OPTIONAL
      |  {
      |       ?membership_type_association a rdbs:MembershipTypeAssociation .
      |        {
      |            ?membership_type_association rdbs:ContainerVerticeType ?vertice_type .
      |            {
      |                ?membership_type_association rdbs:ContaineeVerticeType ?containee_vertice_type .
      |                ?containee_vertice_type rdfs:label ?containee_vertice_type_label .
      |# Create Dynamic Data Column
      |                ?vertice_type rdfs:label ?vertice_type_label_optional .
      |                BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |                BIND ( rdbs:mintClassifiedColumn( ?table_label, "contains", ?containee_vertice_type ) AS ?dynamic_membership_vertice_containee_column ) .
      |                BIND ( rdbs:mintClassifiedColumnLabel( "contains", ?containee_vertice_type ) AS ?dynamic_membership_vertice_containee_column_label ) .
      |            }
      |            UNION
      |            {
      |                ?membership_type_association rdbs:ContaineeEdgeType ?containee_edge_type .
      |                ?containee_edge_type rdfs:label ?containee_edge_type_label .
      |# Create Dynamic Data Column
      |                ?vertice_type rdfs:label ?vertice_type_label_optional .
      |                BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |                BIND ( rdbs:mintClassifiedColumn( ?table_label, "contains", ?containee_edge_type ) AS ?dynamic_membership_edge_containee_column ) .
      |                BIND ( rdbs:mintClassifiedColumnLabel( "contains", ?containee_edge_type ) AS ?dynamic_membership_edge_containee_column_label ) .
      |            }
      |        }
      |        UNION
      |        {
      |            ?membership_type_association rdbs:ContaineeVerticeType ?vertice_type .
      |            ?membership_type_association rdbs:ContainerVerticeType ?container_vertice_type .
      |            ?container_vertice_type rdfs:label ?container_vertice_type_label .
      |# Create Dynamic Data Column
      |            ?vertice_type rdfs:label ?vertice_type_label_optional .
      |            BIND ( rdbs:mintGraphCBEDescriptionTable( ?vertice_type_label_optional, rdbs:VerticeCBETable ) AS ?table_label ) .
      |            BIND ( rdbs:mintClassifiedColumn( ?table_label, "isMemberOf", ?container_vertice_type ) AS ?dynamic_membership_vertice_container_column ) .
      |            BIND ( rdbs:mintClassifiedColumnLabel( "isMemberOf", ?container_vertice_type ) AS ?dynamic_membership_vertice_container_column_label ) .
      |        }
      |     }
      |	}
      |}
      |""".stripMargin



  def execConstructQuery(query: String) = {

    for {

      //Just registering Shacl functions
      rdbsShapesModel   <- IO { ModelFactory.createDefaultModel()}
      _                 <- IO { rdbsShapesModel.read("elsevier_entellect_schema_rdbs.ttl") }
      shaclSystemModel  <- IO { SHACLSystemModel.getSHACLModel }
      shapesUnionGraph  <- IO { new MultiUnion(Array(shaclSystemModel.getGraph, rdbsShapesModel.getGraph))}
      shapesModel       <- IO { ModelFactory.createModelForGraph(shapesUnionGraph)}
      _                 <- IO { SHACLFunctions.registerFunctions(shapesModel)}

      //Registering Custom function and setting the log level
      //_                 <- IO { ARQ.setExecutionLogging(Explain.InfoLevel.ALL) }
      _                 <- IO { PropertyFunctionRegistry.get().put("http://spinrdf.org/spif#split", classOf[strSplit]) }


      model             <- IO { ModelFactory.createDefaultModel()}
      _                 <- IO { model.read("elsevier_entellect_enriched_dbschema_resnet_basic_with_tag_with_dynamic.ttl")}
      _                 <- IO { model.read("topologyInferenceModel.ttl") }

      qexec             <- IO  { QueryExecutionFactory.create(query, model ) }


      cResult            <- IO {

        val startTime         =  System.currentTimeMillis()
        val constructModel    = qexec.execConstruct()
        qexec.close()

        val endTime = System.currentTimeMillis()

        (constructModel, startTime, endTime)

      }

      (constructModel, startTime, endTime) = cResult

      _                 <-  IO { constructModel.setNsPrefixes(model); constructModel.write(System.out, Lang.TTL.getName) }

      execTime          <-  IO { endTime - startTime}


    } yield execTime



  }



  (execConstructQuery(experimentalQuery) flatTap(excTime => IO{info(s"Execution time for experimentalQuery was: $excTime \n")}),
    execConstructQuery(unionQuery) flatTap(excTime => IO{info(s"Execution time for unionQuery was: $excTime \n")})).parTupled.unsafeRunSync()



}
