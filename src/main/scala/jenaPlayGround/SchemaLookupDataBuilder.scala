package jenaPlayGround

import cats.effect.IO
import jenaPlayGround.DataTypes.{FdnGraphSchema, IndividualType, ResourceType}
import jenaPlayGround.SchemaLookupDataBuilder.DataTypes.SchemaLookupData
import cats.syntax.all._
import scribe._

import org.apache.jena.shared.PrefixMapping
import scala.collection.immutable.SortedMap

object  SchemaLookupDataBuilder {

  object DataTypes {

    final case class SchemaLookupData(prefixMapping: PrefixMapping, lookup:  SortedMap[ResourceType, IndividualType])

  }

  def makeLookupDataFromFdnSchemas(fdnGraphSchemas: List[FdnGraphSchema]): IO[SchemaLookupData] = {
    for {

      fdnSchemas          <- IO.pure { fdnGraphSchemas }
      prefixUriPairs      = fdnSchemas map { fdnSchema => fdnSchema.ontPrefix -> s"${fdnSchema.ontUri}/" }
      resTypeObjTypePairs = fdnSchemas flatMap makeResourceTypeObjectTypePairs
      prefixes            = prefixUriPairs map (_._1)

      _                   <- IO { info(s"Making SchemaLookupData with Ontologies: ${prefixes.mkString(" | ")}") }

      prefixMapping       <- IO { PrefixMapping.Factory.create() }
      _                   <- prefixUriPairs traverse { prefixUriPair => IO { prefixMapping.setNsPrefix(prefixUriPair._1, prefixUriPair._2)} }

      lookupData          <- IO.pure { SchemaLookupData(prefixMapping, SortedMap(resTypeObjTypePairs: _* )(scala.math.Ordering.comparatorToOrdering(String.CASE_INSENSITIVE_ORDER))) }

      _                   <- IO { info(s"SchemaLookupData was Built successfully") }

    } yield  lookupData
  }

  private def makeResourceTypeObjectTypePairs(fdnGraphSchema: FdnGraphSchema): List[(ResourceType, IndividualType)] = {
    List (
      fdnGraphSchema.entityTypes.map(entityType => (entityType.entityType, entityType)),
      fdnGraphSchema.relationTypes.map(relationType => (relationType.relationType, relationType))
    ).flatten
  }

}
