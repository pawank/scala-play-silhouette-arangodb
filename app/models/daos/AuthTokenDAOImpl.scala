package models.daos

import java.util.UUID
import javax.inject.Inject

import models.AuthToken
import models.daos.AuthTokenDAOImpl._
import org.joda.time.DateTime
import models.ModelsUtils._

import scala.collection.mutable
import scala.concurrent.Future

import scala.collection.mutable
import scala.concurrent.Future
import models.repos.ArangoDbService
import com.outr.arango.{
  ArangoDB,
  ArangoDatabase,
  Credentials,
  ArangoException,
  DatabaseState,
  Document,
  DocumentModel,
  Id,
  Index,
  IndexType,
  Serialization
}

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._, io.circe.parser._

/**
 * Give access to the [[AuthToken]] object.
 */
class AuthTokenDAOImpl @Inject() (db: ArangoDbService) extends AuthTokenDAO {
  //val db: ArangoDbService = ArangoDbService("Auth")
  private[this] val client: ArangoDatabase = db.client
  val collectionName: String = "tokens"
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val printer = Printer.noSpaces.copy(dropNullValues = true)
  val collection = db.setupCollection(collectionName)
  implicit val serialization: Serialization[AuthToken] =
    Serialization.auto[AuthToken]

  /**
   * Finds a token by its ID.
   *
   * @param id The unique token ID.
   * @return The found token or None if no token for the given ID could be found.
   */
  def find(id: UUID): Future[Option[AuthToken]] = {
    import com.outr.arango._
    import com.outr.arango.api.APITransaction
    /*
    val query = s"""FOR p IN $collectionName FILTER p.id == "${
      id
        .toString()
    }" RETURN p"""
     */
    val pk = id.toString()
    //val queryAQL = aql"FOR p IN $collectionName FILTER p.id == $pk RETURN p"
    Future.successful(None)
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
    Future.successful(Seq.empty)
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
    val result: Future[AuthToken] = collection.document
      .upsertOne(token)
      .map(u => token.copy(_key = u._id.map(_._key)))
    result
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
    collection.document
      .deleteOne(Id[String](id.toString(), collectionName))
      .map { id =>
        println(s"ID after delete: $id")
        id._id
      }
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
