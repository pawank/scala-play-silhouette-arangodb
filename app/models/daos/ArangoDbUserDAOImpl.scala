package models.daos

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api.LoginInfo
import models.User
import models.daos.UserDAOImpl._
import models.ModelsUtils._
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

import scala.collection.mutable
import scala.concurrent.Future
import models.repos.ArangoDbService

import io.circe.generic.auto._
import io.circe.parser.decode
import io.circe.syntax._
import cats.syntax.either._
import io.circe._, io.circe.parser._

/**
 * Give access to the user object.
 */
class ArangoDbUserDAOImpl @Inject() (db: ArangoDbService) extends UserDAO {
  //val db: ArangoDbService = ArangoDbService("User")
  private[this] val client: ArangoDatabase = db.client
  val collectionName: String = "users"
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val printer = Printer.noSpaces.copy(dropNullValues = true)
  val collection = db.setupCollection(collectionName)
  implicit val serialization: Serialization[User] =
    Serialization.auto[User]
  implicit val serializationLoginInfo: Serialization[LoginInfo] =
    Serialization.auto[LoginInfo]

  /**
   * Finds a user by its login info.
   *
   * @param loginInfo The login info of the user to find.
   * @return The found user or None if no user for the given login info could be found.
   */
  def find(loginInfo: LoginInfo): Future[Option[User]] = {
    //Future.successful(users.find { case (_, user) => user.loginInfo == loginInfo }.map(_._2))
    val query =
      s"""FOR p IN $collectionName FILTER p.loginInfo.providerID == "${loginInfo.providerID}" AND p.loginInfo.providerKey == "${loginInfo.providerKey}" RETURN p"""
    Future.successful(None)
  }

  /**
   * Finds a user by its user ID.
   *
   * @param userID The ID of the user to find.
   * @return The found user or None if no user for the given ID could be found.
   */
  def find(userID: UUID): Future[Option[User]] = {
    val query = s"""FOR p IN $collectionName FILTER p.userID == "${
      userID
        .toString()
    }" RETURN p"""
    Future.successful(None)
    //Future.successful(users.get(userID))
  }

  /**
   * Saves a user.
   * Ideally it should use UPSERT feature of the backend store but as of now it checks whether User exists or not and accordingly insert/update the record
   *
   * @param user The user to save.
   * @return The saved user.
   */
  def save(user: User): Future[User] = {
    println(s"Saving user: $user")
    val result: Future[User] = collection.document
      .upsertOne(user)
      .map(u => user.copy(_key = u._id.map(_._key)))
    result
  }
}

object ArangoDbUserDAOImpl {

  /**
   * The list of users.
   */
  //val users: mutable.HashMap[UUID, User] = mutable.HashMap()
}
