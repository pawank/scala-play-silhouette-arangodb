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

}
