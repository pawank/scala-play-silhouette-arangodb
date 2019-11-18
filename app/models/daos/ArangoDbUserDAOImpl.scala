package models.daos

import java.util.UUID
import javax.inject.Inject

import com.mohiva.play.silhouette.api.LoginInfo
import models.User
import models.daos.UserDAOImpl._

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
 * Give access to the user object.
 */
class ArangoDbUserDAOImpl @Inject() (db: ArangoDbService) extends UserDAO {
  private[this] val client: DocumentClient = db.client
  val collectionName: String = "users"
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global
  val printer = Printer.noSpaces.copy(dropNullValues = true)

  def parse(result: Future[Option[Json]]): Future[Option[User]] = {
    result.map(result => {
      result match {
        case Some(json) =>
          json.as[User] match {
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

    parse(
      models.ModelsUtils.executeQueryForSingleResult(collectionName, query)(db)
    )
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
    parse(
      models.ModelsUtils.executeQueryForSingleResult(collectionName, query)(db)
    )
    //Future.successful(users.get(userID))
  }

  /**
   * Saves a user.
   *
   * @param user The user to save.
   * @return The saved user.
   */
  def save(user: User): Future[User] = {
    println(s"Saving user: $user")
    val r = for {
      u <- find(user.userID)
      savedResult <- {
        u match {
          case Some(found) =>
            client
              .replaceDocument(
                db.dbName,
                collectionName,
                user._key.getOrElse(""),
                printer.print(user.asJson)
              )
              .map(x => {
                println(s"x: $x")
                println(s"Updated user: $user")
                user
              })
          case _ =>
            client
              .createDocument(
                db.dbName,
                collectionName,
                printer.print(user.asJson)
              )
              .map(x => {
                println(s"x: $x")
                println(s"Saved user: $user")
                user
              })
        }
      }
    } yield {
      savedResult
    }
    r
  }
}

object ArangoDbUserDAOImpl {

  /**
   * The list of users.
   */
  //val users: mutable.HashMap[UUID, User] = mutable.HashMap()
}
