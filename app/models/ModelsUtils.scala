package models

import java.util.UUID
import scala.util.control.NonFatal
import scala.concurrent.Future

import org.joda.time.DateTime
import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._, io.circe.parser._
import models.repos.ArangoDbService
import scala.concurrent.ExecutionContext

object ModelsUtils {
  //DateTime formatter in UTC for auto circe encoding/decoding
  val utcDateFormatter =
    org.joda.time.format.DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss")
  implicit val encodeInstant: Encoder[DateTime] =
    Encoder.encodeString
      .contramap[DateTime](utcDateFormatter.print(_))

  implicit val decodeInstant: Decoder[DateTime] = Decoder.decodeString.emap {
    s =>
      try {
        Right(DateTime.parse(s, utcDateFormatter))
      } catch {
        case NonFatal(e) => Left(e.getMessage)
      }
  }

  implicit val encodeUUID: Encoder[UUID] =
    Encoder.encodeString
      .contramap[UUID](_.toString)

  implicit val decodeUUID: Decoder[UUID] = Decoder.decodeString.emap { s =>
    try {
      Right(UUID.fromString(s))
    } catch {
      case NonFatal(e) => Left(e.getMessage)
    }
  }

  def executeQueryForSingleResult(
    collectionName: String,
    query: String
  )(
    db: ArangoDbService
  )(implicit ec: ExecutionContext): Future[Option[Json]] = {
    val q = com.charlesahunt.proteus.models
      .Query(
        query = query,
        count = true,
        batchSize = 1
      )
    //println(s"Query: $query")
    db.client
      .getQueryResult(db.dbName, collectionName, q)
      .map(r => {
        //println(s"executeQueryForSingleResult: $r")
        r match {
          case Left(value) =>
            throw value
          case Right(value) =>
            val doc: Json = parse(value).getOrElse(Json.Null)
            val result: Vector[Json] = doc.hcursor
              .downField("result")
              .focus
              .flatMap(_.asArray)
              .getOrElse(Vector.empty)
            result.headOption
        }
      })
  }

  def executeQueryForResults(
    collectionName: String,
    query: String,
    batchSize: Int = 25
  )(
    db: ArangoDbService
  )(
    implicit
    ec: ExecutionContext
  ): Future[Tuple2[Option[models.repos.CursorID], Vector[Json]]] = {
    val q = com.charlesahunt.proteus.models
      .Query(
        query = query,
        count = true,
        batchSize = batchSize
      )
    //println(s"Query: $query")
    db.client
      .getQueryResult(db.dbName, collectionName, q)
      .map(r => {
        //println(s"executeQueryForResults: $r")
        r match {
          case Left(value) =>
            throw value
          case Right(value) =>
            val doc: Json = parse(value).getOrElse(Json.Null)
            val id: Option[models.repos.CursorID] =
              doc.hcursor
                .get[String]("id")
                .toOption
                .map(models.repos.CursorID(_))
            doc.hcursor.get[String]("error").toOption match {
              case Some(e) =>
                (None, Vector.empty)
              case _ =>
                val result: Vector[Json] = doc.hcursor
                  .downField("result")
                  .focus
                  .flatMap(_.asArray)
                  .getOrElse(Vector.empty)
                (id, result)
            }
        }
      })
  }
}
