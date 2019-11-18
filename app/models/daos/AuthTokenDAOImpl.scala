package models.daos

import java.util.UUID
import javax.inject.Inject

import models.ModelsUtils._
import models.AuthToken
import models.daos.AuthTokenDAOImpl._
import org.joda.time.DateTime

import scala.collection.mutable
import scala.concurrent.Future

import scala.collection.mutable
import scala.concurrent.Future
import com.charlesahunt.proteus.DocumentClient
import models.repos.ArangoDbService

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._, io.circe.parser._

/**
 * Give access to the [[AuthToken]] object.
 */
class AuthTokenDAOImpl @Inject() (db: ArangoDbService) extends AuthTokenDAO {
  private[this] val client: DocumentClient = db.client
  val collectionName: String = "tokens"
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val printer = Printer.noSpaces.copy(dropNullValues = true)

  def parse(result: Future[Option[Json]]): Future[Option[AuthToken]] = {
    result.map(result => {
      result match {
        case Some(json) =>
          json.as[AuthToken] match {
            case Left(error) =>
              println(error)
              None
            case Right(obj) => Some(obj)
          }
        case _ =>
          //throw new Exception("No token found")
          None
      }
    })
  }

  def parseResults(result: Future[Seq[Json]]): Future[Seq[AuthToken]] = {
    result.map(result => {
      result.map(json => {
        json.as[AuthToken] match {
          case Left(error) =>
            println(error)
            throw new Exception(error.toString())
          case Right(obj) => obj
        }
      })
    })
  }

  /**
   * Finds a token by its ID.
   *
   * @param id The unique token ID.
   * @return The found token or None if no token for the given ID could be found.
   */
  def find(id: UUID): Future[Option[AuthToken]] = {
    //Future.successful(tokens.get(id))
    val query = s"""FOR p IN $collectionName FILTER p.id == "${
      id
        .toString()
    }" RETURN p"""
    parse(executeQueryForSingleResult(collectionName, query)(db))
  }

  /**
   * Finds expired tokens.
   *
   * @param dateTime The current date time.
   */
  def findExpired(dateTime: DateTime): Future[Seq[AuthToken]] = {
    //Future.successful(Seq.empty)
    /*Future.successful {
    tokens
      .filter {
        case (_, token) =>
          token.expiry.isBefore(dateTime)
      }
      .values
      .toSeq
  }*/
    val query = s"""FOR p IN $collectionName FILTER p.expiry < "${
      dateTime
        .toString()
    }" RETURN p"""
    parseResults(
      executeQueryForResults(collectionName, query, 999)(db)
        .map(r => r._2.toSeq)
    )
  }

  /**
   * Saves a token.
   *
   * @param token The token to save.
   * @return The saved token.
   */
  def save(token: AuthToken) = {
    //tokens += (token.id -> token)
    println(s"Saving token: $token")
    client
      .createDocument(
        db.dbName,
        collectionName,
        printer.print(token.asJson)
      )
      .map(x => {
        //println(s"x: $x")
        println(s"Saved token: $token")
        token
      })
    //Future.successful(token)
  }

  /**
   * Removes the token for the given ID.
   *
   * @param id The ID for which the token should be removed.
   * @return A future to wait for the process to be completed.
   */
  def remove(id: UUID) = {
    //Future.successful(())
    //println(s"Removing token $id")
    val r = for {
      result <- find(id)
      delete <- client
        .deleteDocument(
          db.dbName,
          collectionName,
          result.map(s => s._key.getOrElse("")).getOrElse("")
        )
    } yield ()
    r
  }
}

/**
 * The companion object.
 */
object AuthTokenDAOImpl {

  /**
   * The list of tokens.
   */
  //val tokens: mutable.HashMap[UUID, AuthToken] = mutable.HashMap()
}
