package circe

import cats.syntax.all._
import cats.Show
import cats.effect.IO
import cats.kernel.Monoid

import io.circe._
import io.circe.Json._
import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.generic.semiauto._

import jenaPlayGround.DataTypes._
import org.apache.jena.ontology._
import org.apache.jena.datatypes.RDFDatatype
import org.apache.jena.datatypes.xsd.XSDDatatype
import org.apache.jena.datatypes.xsd.impl._
import org.apache.jena.rdf.model.{Literal, ModelFactory}
import org.apache.jena.riot.Lang
import org.apache.jena.shared.PrefixMapping

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import scala.util.chaining.scalaUtilChainingOps
import scribe._

object TgDataTypes {

  type ResourceUri        = String
  type ResourceMessage    = String

  type EntityResource     = OntResource
  type RelationResource   = OntResource


  type EntityUri          = String
  type RelationUri        = String

  /**
   *
   * TYPES ALIASES
   *
   */
  type ObjectType         = String
  type VertexType         = String
  type EdgeType           = String
  type AttributeType      = String
  type VertexId           = String
  type AttributeValue     = String



  case class EntityResourceData(eUri: EntityUri, eType: ResourceType)

  /**
   *
   *  TG DATA TYPES
   *
   */

  case class TgMessage(vertices: List[Vertex], edges: List[Edge])

  sealed abstract class TgObject(val objectType: ObjectType)
  final case class Vertex(vType: VertexType, id: VertexId, attributes: List[TgAttribute]) extends TgObject(vType)
  final case class Edge(eType: EdgeType, sourceVertexId: VertexId, sourceVertexType: VertexType, targetVertexId: VertexId, targetVertexType: VertexType, attributes: List[TgAttribute]) extends TgObject(eType)


  sealed trait TgDataType
  case object NUMBER extends TgDataType
  case object STRING extends TgDataType


  sealed abstract class TgAttribute(val aType: AttributeType)
  final case class SingleValuedAttribute(override val aType: AttributeType, value: AttributeValue, dataType: TgDataType) extends TgAttribute(aType)
  final case class MultiValuedAttribute(override val aType: AttributeType, values: List[AttributeValue], dataType: TgDataType) extends TgAttribute(aType)
  final case class UserDefinedAttribute(override val aType: AttributeType, values: List[(AttributeType, TgAttribute)]) extends TgAttribute(aType)


  /**
   *  SHOW TYPE CLASSES
   */

  implicit val showTgMessage: Show[TgMessage] = (tgMsg: TgMessage) => {
    s"""
       |TgMessage: [\n
       |Vertices: ${tgMsg.vertices.map(_.show).mkString("\n","\n","\n")}
       |Edges: ${tgMsg.edges.map(_.show).mkString("\n","\n","\n")}
       |]""".stripMargin
  }

  implicit val showVertex: Show[Vertex] = (vertex: Vertex) => {
    s"""
       |Vertex: [
       |Id: ${vertex.id}
       |VertexType: ${vertex.vType}
       |Attributes: ${vertex.attributes.map(_.show).mkString("\n ", "\n ", "")}
       |]""".stripMargin
  }

  implicit val showEdge: Show[Edge] = (edge: Edge) => {
    s"""
       |Edge: [
       |EdgeType: ${edge.eType}
       |SourceVertex: [Id: ${edge.sourceVertexId} | Type: ${edge.sourceVertexType}]
       |TargetVertex: [Id: ${edge.targetVertexId} | Type: ${edge.targetVertexType}]
       |Attributes: ${edge.attributes.map(_.show).mkString("\n", "\n", "")}
       |]""".stripMargin
  }

  implicit val ShowMultiValuedAttribute: Show[MultiValuedAttribute] = (mAttr: MultiValuedAttribute) => {
    s"""MultiValuedAttribute(${ mAttr.aType }, [${ mAttr.values.mkString(", ") }], ${ mAttr.dataType })""".stripMargin
  }

  implicit val ShowSingleValuedAttribute: Show[SingleValuedAttribute] = (sAttr: SingleValuedAttribute) => {
    sAttr.toString
  }

  implicit val ShowUserDefinedAttribute: Show[UserDefinedAttribute] = (udAttr: UserDefinedAttribute) => {
    udAttr.toString
  }

  implicit val showTgAttribute: Show[TgAttribute] = {
    case sAttr: SingleValuedAttribute => sAttr.show
    case mAttr: MultiValuedAttribute  => mAttr.show
    case udAttr: UserDefinedAttribute => udAttr.show
  }

  implicit class EmptyMonoidOps(dataType: TgDataType) {

    def empty: String = dataType match {
      case NUMBER => Monoid[Double].empty.toString
      case STRING => Monoid[String].empty
    }

  }



  implicit class PrettyModelOps(model: OntModel) {
    def toPrettyString: IO[String] = {
      for {
        outStream <- IO.pure { new ByteArrayOutputStream() }
        _         <- IO { model.write(outStream, Lang.TTL.getName) }
        outString <- IO { outStream.toString(StandardCharsets.UTF_8)}
      } yield outString
    }
  }

  implicit class OntModelOps(resourceMessage: ResourceMessage) {
    def asOntModel: IO[OntModel] = {
      for {
        _            <- IO { OntDocumentManager.getInstance().setProcessImports(false) }
        messageModel <- IO { ModelFactory.createOntologyModel(OntModelSpec.OWL_MEM) }
        _            <- IO { messageModel.read(new ByteArrayInputStream(resourceMessage.getBytes(StandardCharsets.UTF_8)), null, Lang.TTL.getName) }
      } yield messageModel
    }
  }


  /**
   * DSL to quickly convert RDF DataTypes to TG DataType
   *
   * Assume that the RDFDataType have been created with Jena TypeMapper Global Instance.
   */
  implicit class RDFDatatypeOps(rdfDataType: RDFDatatype) {
    def asTgDataType: IO[TgDataType] = rdfDataType match {
      case _: RDFLangString       => IO.pure(STRING)
      case _: XSDBaseStringType   => IO.pure(STRING)
      case _: XSDFloat            => IO.pure(NUMBER)
      case _: XSDDouble           => IO.pure(NUMBER)
      case _: XSDBaseNumericType  => IO.pure(NUMBER)
      case _: XSDDateTimeType     => IO.pure(STRING)
      case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDboolean) => IO.pure(STRING)
      case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDanyURI) => IO.pure(STRING)
      case e                      => IO { warn(s"unknown XSD: $e defaulting to String")} *> IO.pure(STRING)
    }
  }


  /**
   * DSL to quickly convert Literal to its JavaValue
   *
   * Assume that the RDFDataType have been created with Jena TypeMapper Global Instance.
   */
  implicit class LiteralOps(literal: Literal) {
    def asJavaValue(rdfDataType: RDFDatatype): IO[Any] = {
      rdfDataType match {
        case _: RDFLangString       => IO { literal.getString }
        case _: XSDBaseStringType   => IO { literal.getString }
        case _: XSDFloat            => IO { literal.getDouble }
        case _: XSDDouble           => IO { literal.getDouble }
        case _: XSDBaseNumericType  => IO { literal.getDouble }
        case _: XSDDateTimeType     => IO { literal.getString }
        case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDboolean) => IO { literal.getString } //Not quite right but we don't support boolean yet, not even sure jena will let it fly.
        case dataType: XSDDatatype if dataType.equals(XSDDatatype.XSDanyURI) => IO { literal.getString }
        case e                      => IO { warn(s"unknown XSD: $e defaulting to trying conversion to a java String")  } *> IO { literal.getString }
      }
    } onError { _ => IO { error(s"""Can't convert literal [${literal.getLexicalForm}] of DataType [${literal.getDatatypeURI}] to supplied DataType [${rdfDataType.getURI}]""") } }
  }

  implicit class TgTypeOps(resourceType: ResourceType) {
    import st.process.encase.Encase._
    def asTgType(pm: PrefixMapping): ObjectType = {
      pm.shortForm(resourceType).split(':') pipe { array => s"${toUpperCamel(array(0))}_${array(1)}" }
    }
  }



  /**
   *  CLIENT JSON SERIALIZATION TYPE CLASSES
   */

  case class EdgeGroupKey(sourceVertexType: VertexType, sourceVertexId: VertexId, eType: EdgeType, targetVertexType: VertexType)


  /**
   * SingleValuedAttribute
   */
  implicit val encodeSingleValuedAttribute: Encoder[SingleValuedAttribute] = new Encoder[SingleValuedAttribute] {
    override def apply(a: SingleValuedAttribute): Json = {
      obj(
        a.aType -> obj("value" -> (if (a.dataType == NUMBER) a.value.toDouble.asJson else a.value.asJson ) )
      )
    }
  }

  /**
   * MultiValuedAttribute
   */
  implicit val encodeMultiValuedAttribute: Encoder[MultiValuedAttribute] = new Encoder[MultiValuedAttribute] {
    override def apply(a: MultiValuedAttribute): Json = {
      obj(
        a.aType -> obj("value" -> (if (a.dataType == NUMBER) a.values.map(_.toDouble).asJson else a.values.asJson ) )
      )
    }
  }

  /**
   * UserDefinedAttribute (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/intro#formatting-data-in-json TG Formatting Advanced DataTypes]])
   *
   * E.g. The Tuple Definition
   *
   * {{{
   *    TYPEDEF TUPLE <name STRING, age INT> Person
   * }}}
   * Translates to
   *
   * {{{
   *    {
   *      "keyList: ["name", "age"],
   *      "valueList": ["Sam", 24]
   *    }
   * }}}
   *
   * __Rule__:
   *
   * `In TG the Order of the Fields of a User Define Type matter at Ingestion time, hence we sort it alphabetically by default to ensure an order.`
   * `The sorting is first done upstream in the schema builder, so we do it downstream when inserting data.`
   *
   */
  implicit val encodeUserDefinedAttribute: Encoder[UserDefinedAttribute] = new Encoder[UserDefinedAttribute] {

    override def apply(a: UserDefinedAttribute): Json = {
      obj(a.aType -> obj("value" -> makeKeysValuesObject (a.values.sortBy(_._1)) ))
    }

    private def makeKeysValuesObject (values: List[(AttributeType, TgAttribute)]): Json =  {
      values
        .unzip
        .pipe { case (types, attributes) => (types map {_.asJson}) -> ( attributes map makeJsonValueFromAttributeValue ) }
        .pipe { case (keys, values) => obj("keylist" -> keys.asJson) -> obj("valuelist" -> values.asJson) }
        .pipe { case (keyListObject, valueListObject) => keyListObject.deepMerge(valueListObject) }
    }

    private def makeJsonValueFromAttributeValue(attribute: TgAttribute): Json = attribute match {
      case SingleValuedAttribute(_, value, dataType) => if (dataType == NUMBER) value.toDouble.asJson else value.asJson
      case MultiValuedAttribute(_, values, dataType) => if (dataType == NUMBER) values.map(_.toDouble).asJson else values.asJson
      case UserDefinedAttribute(_, _)           => "unsupported".asJson
    }
  }

  /**
   *  TgAttribute ( i.e. Attribute ADT)
   */
  implicit val encodeTgAttribute: Encoder[TgAttribute] = new Encoder[TgAttribute] {
    override def apply(a: TgAttribute):Json = a match {
      case s@SingleValuedAttribute(_, _, _) => s.asJson
      case m@MultiValuedAttribute(_, _, _) => m.asJson
      case uda@UserDefinedAttribute(_, _) => uda.asJson
    }
  }

  /**
   * TgAttributeList
   */
  implicit val encodeTgAttributeList: Encoder[List[TgAttribute]] = new Encoder[List[TgAttribute]] {
    override def apply(a: List[TgAttribute]): Json = {
      a.map(_.asJson).foldLeft(obj()){_.deepMerge(_)}
    }
  }




  /**
   * Vertex Standalone (not used in batch)
   */
  implicit val encodeStandAloneVertex: Encoder[Vertex] = new Encoder[Vertex] {
    override def apply(a: Vertex): Json = {
      obj(a.vType -> obj(a.id ->  a.attributes.asJson))
    }
  }

  /**
   * Edge StandAlone (not used in batch)
   */
  implicit val encodeStandAloneEdge: Encoder[Edge] = new Encoder[Edge] {
    override def apply(a: Edge): Json = {
      obj(a.sourceVertexType -> obj(a.sourceVertexId -> obj(a.eType -> obj(a.targetVertexType -> obj(a.targetVertexId -> a.attributes.asJson)))))
    }
  }





  /**
   *
   * Nested Vertices Message (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/built-in-endpoints#request-body-2  TG Json Message Specification]])
   *
   * {{{
   *  "vertices": {
   *     "<vertex_type>": {
   *         "<vertex_id>": {
   *             "<attribute>": {
   *                 "value": <value>,
   *                 "op": <opcode>
   *             }
   *         }
   *      }
   *  }
   * }}}
   *
   * __Rule__:
   *
   * `The nested hierarchy means that vertices are grouped by type`
   *
   */
  implicit val encodeNestedVertices: Encoder[List[Vertex]] = new Encoder[List[Vertex]] {
    override def apply(a: List[Vertex]): Json = {
      a.groupMapReduce
      { vertex => vertex.vType }
      { vertex => obj(vertex.id -> vertex.attributes.asJson) }
      { _.deepMerge(_) } pipe (_.asJson)
    }
  }

  /**
   * Nested Edges Message (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/built-in-endpoints#request-body-2  TG Json Message Specification]])
   *
   * {{{
   *
   *  "edges": {
   *     "<source_vertex_type>": {
   *        "<source_vertex_id>": {
   *           "<edge_type>": {
   *              "<target_vertex_type>": {
   *                 "<target_vertex_id>": {
   *                    "<attribute>": {
   *                       "value": <value>,
   *                       "op": <opcode>
   *                    }
   *                 }
   *              }
   *           }
   *        }
   *     }
   *  }
   * }}}
   *
   * __Rule__:
   *
   * `The nested hierarchy means Edges are first grouped by source vertex type, then vertex ID, then edge type, then target vertex type`
   *
   */
  implicit val encodeNestedEdges: Encoder[List[Edge]] = new Encoder[List[Edge]] {
    override def apply(a: List[Edge]): Json = {
      a.groupMapReduce
      { edge => EdgeGroupKey(edge.sourceVertexType, edge.sourceVertexId, edge.eType, edge.targetVertexType) }
      { edge => obj(edge.targetVertexId -> edge.attributes.asJson ) }
      { _.deepMerge(_) }
        .toList
        .map { case (egk, edgeGroup) => obj(egk.sourceVertexType -> obj(egk.sourceVertexId -> obj(egk.eType -> obj(egk.targetVertexType -> edgeGroup))))}
        .foldLeft(obj())(_.deepMerge(_))
    }
  }


  /**
   * TG Message As Nested Objects (see [[https://docs.tigergraph.com/v/3.1/dev/restpp-api/built-in-endpoints#request-body-2  TG Json Message Specification]])
   *
   * {{{
   * {
   *
   *  "vertices": {
   *     "<vertex_type>": {
   *         "<vertex_id>": {
   *             "<attribute>": {
   *                 "value": <value>,
   *                 "op": <opcode>
   *             }
   *         }
   *      }
   *  }
   *
   *  "edges": {
   *     "<source_vertex_type>": {
   *        "<source_vertex_id>": {
   *           "<edge_type>": {
   *              "<target_vertex_type>": {
   *                 "<target_vertex_id>": {
   *                    "<attribute>": {
   *                       "value": <value>,
   *                       "op": <opcode>
   *                    }
   *                 }
   *              }
   *           }
   *        }
   *     }
   *  }
   *
   * }
   * }}}
   *
   * @see [[encodeNestedVertices]]
   * @see [[encodeNestedEdges]]
   */
  implicit val encodeTgMessageAsNestedObjects: Encoder[TgMessage] = new Encoder[TgMessage] {
    override def apply(a: TgMessage): Json = {
      obj("vertices" -> a.vertices.asJson, "edges" -> a.edges.asJson)
    }
  }

  /**
   * TG RESPONSE MESSAGE DATATYPE
   */

  case class Result(accepted_vertices: Int, accepted_edges: Int)
  case class Version(edition: String, api: String, schema: Int)
  case class TgResponse(version: Version, error: Boolean, message: String,  results: List[Result], code: String)


  implicit val tgResponseDecoder: Decoder[TgResponse] = deriveDecoder[TgResponse]
  implicit val tgResponseEncoder: Encoder[TgResponse] = deriveEncoder[TgResponse]

}
