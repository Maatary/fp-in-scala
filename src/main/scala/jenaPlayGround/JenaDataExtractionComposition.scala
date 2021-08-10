package jenaPlayGround

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import org.apache.jena.rdf.model.{Model, ModelFactory, Resource, ResourceFactory}
import org.apache.jena.riot.Lang
import org.apache.jena.vocabulary.{OWL, RDF, RDFS}

import scala.jdk.CollectionConverters._
import scribe._

import scala.util.chaining.scalaUtilChainingOps


object AlignmentDataTypes {


  case class TableAlignment(tableLabel: TableLabel, isCBE: isCBE, primaryFieldAlignment: PrimaryFieldAlignment, dataFieldAlignments: List[DataFieldAlignment], entityFieldAlignments: List[EntityFieldAlignment])

  trait FieldAlignment
  case class PrimaryFieldAlignment(fieldLabel: String,  fieldDataType: String, mapsToClass: String) extends FieldAlignment
  case class DataFieldAlignment(fieldLabel: String,  fieldDataType: String, isArrayField: Boolean, mapsToProperty: String) extends FieldAlignment
  case class EntityFieldAlignment(fieldLabel: String,  fieldClass: String, isArrayField: Boolean, mapsToProperty: String) extends FieldAlignment


  type TableLabel           = String
  type isCBE                = Boolean
  type TablePrimaryKeyLabel = String
  type TablePrimaryKeyType  = String
  type MapsToClass          = String


  type FieldAlignmentResource       = Resource
  type DataFieldAlignmentResource   = Resource
  type EntityFieldAlignmentResource = Resource
  type TableAlignmentResource       = Resource

  import cats.Show

  implicit val showTableAlignment: Show[TableAlignment] = (ta: TableAlignment) => {
    s"""
       |TableAlignment(
       |tableLabel: ${ta.tableLabel},
       |isCBE: ${ta.isCBE},
       |primaryFieldAlignment: ${ta.primaryFieldAlignment},
       |dataFieldAlignments: ${ta.dataFieldAlignments},
       |entityFieldAlignments: ${ta.entityFieldAlignments}
       |)""".stripMargin
  }

}







object JenaDataExtractionComposition extends App {

  import AlignmentDataTypes._


  scribe.Logger.root
    .clearHandlers()
    .clearModifiers()
    .withHandler(minimumLevel = Some(Level.Info))
    .replace()

  val programLogic = for {

    model                                 <- IO { ModelFactory.createDefaultModel() }

    _                                     <- IO { model.read("karmaMappingInfoInferenceModel.ttl") }

    tableAlignmentResources = getTableAlignmentResources(model)


    tableAlignmentResourceDescriptions    <- IO.fromEither { tableAlignmentResources traverse getTableAlignmentResourceDescription }

    tableAlignments                       <- IO.fromEither { tableAlignmentResourceDescriptions.traverse { rd => getFieldAlignments(rd._1) map { case (ld, le) => TableAlignment(rd._2, rd._3, PrimaryFieldAlignment(rd._4, rd._5, rd._6), ld, le)} } }


    _                                     <- IO { println(tableAlignments.show) }



    //_     <- IO {model.write(System.out, Lang.TTL.getName)}


  } yield ()

  programLogic.unsafeRunSync()








  def getTableAlignmentResources(model: Model): List[TableAlignmentResource] = {
    model
      .listResourcesWithProperty(RDF.`type`, model.getResource("https://data.elsevier.com/lifescience/schema/rdbs/EntityTableAlignmentData"))
      .asScala
      .toList
  }

  def getTableAlignmentResourceDescription(resource:  TableAlignmentResource): Either[Throwable, (TableAlignmentResource, TableLabel, isCBE, TablePrimaryKeyLabel, TablePrimaryKeyType, MapsToClass)]  = {

    for {

      tableLabel           <- Either.catchNonFatal { resource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasTableLabel")).getString }

      isCbe                <- Either.catchNonFatal { resource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/isCBE")).getBoolean }

      tablePrimaryKeyLabel <- Either.catchNonFatal { resource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasTablePrimaryKeyLabel")).getString }

      tablePrimaryKeyType  <- Either.catchNonFatal { resource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasTablePrimaryKeyType")).getResource.getURI }

      mapsToClass          <- Either.catchNonFatal { resource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/mapsToClass")).getResource.getURI }


    } yield (resource, tableLabel, isCbe, tablePrimaryKeyLabel, tablePrimaryKeyType, mapsToClass)

  }

  /*def makePrimaryFieldAlignment(tableAlignmentResource:  TableAlignmentResource): Either[Throwable, PrimaryFieldAlignment] = {

    Either.catchNonFatal{
      PrimaryFieldAlignment(
        fieldLabel     = tableAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasTablePrimaryKeyLabel")).getString,
        fieldDataType  = tableAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasTablePrimaryKeyType")).getResource.getURI,
        mapsToClass    = tableAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/mapsToClass")).getResource.getURI,
      )
    }
  }*/

  def getFieldAlignments(tableAlignmentResource: TableAlignmentResource): Either[Throwable, (List[DataFieldAlignment], List[EntityFieldAlignment])] = {

    val fieldAlignmentResources       = getFieldAlignmentResources(tableAlignmentResource)
    val dataFieldAlignmentResources   = filterDataFieldAlignmentResources(fieldAlignmentResources)
    val entityFieldAlignmentResources = filterEntityFieldAlignmentResources(fieldAlignmentResources)

    (dataFieldAlignmentResources traverse makeDataFieldAlignment , entityFieldAlignmentResources traverse makeEntityFieldAlignment).tupled
  }


  def getFieldAlignmentResources(tableAlignmentResource: TableAlignmentResource):  List[FieldAlignmentResource] = {

    tableAlignmentResource
      .listProperties(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasFieldAlignmentData"))
      .asScala
      .toList.map(_.getResource)

  }

  def filterDataFieldAlignmentResources(fieldAlignmentsResources: List[FieldAlignmentResource]): List[DataFieldAlignmentResource] = {
      fieldAlignmentsResources
        .filter { _.listProperties(RDF.`type`).asScala.exists(_.getResource.equals(ResourceFactory.createResource("https://data.elsevier.com/lifescience/schema/rdbs/DataFieldAlignmentData"))) }
  }

  def filterEntityFieldAlignmentResources(fieldAlignmentsResources: List[FieldAlignmentResource]): List[EntityFieldAlignmentResource] = {
      fieldAlignmentsResources
        .filter { _.listProperties(RDF.`type`).asScala.exists(_.getResource.equals(ResourceFactory.createResource("https://data.elsevier.com/lifescience/schema/rdbs/EntityFieldAlignmentData"))) }
  }

  def makeDataFieldAlignment(dataFieldAlignmentResource: DataFieldAlignmentResource): Either[Throwable, DataFieldAlignment] = Either.catchNonFatal {
    DataFieldAlignment(
      fieldLabel     = dataFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasFieldLabel")).getString,
      fieldDataType  = dataFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasFieldDataType")).getResource.getURI,
      isArrayField   = dataFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/isArrayField")).getBoolean,
      mapsToProperty = dataFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/mapsToProperty")).getResource.getURI
    )
  }

  def makeEntityFieldAlignment(entityFieldAlignmentResource: EntityFieldAlignmentResource): Either[Throwable, EntityFieldAlignment] = Either.catchNonFatal {
    EntityFieldAlignment(
      fieldLabel     = entityFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasFieldLabel")).getString,
      fieldClass     = entityFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/hasFieldClass")).getResource.getURI,
      isArrayField   = entityFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/isArrayField")).getBoolean,
      mapsToProperty = entityFieldAlignmentResource.getRequiredProperty(ResourceFactory.createProperty("https://data.elsevier.com/lifescience/schema/rdbs/mapsToProperty")).getResource.getURI
    )
  }

}
