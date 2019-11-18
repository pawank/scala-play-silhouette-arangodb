package models.repos

import com.charlesahunt.proteus.DocumentClient

case class CursorID(id: String)

trait ArangoDbService {
  def dbName: String
  def client: DocumentClient
}

class ArangoDbRepoService extends ArangoDbService {
  import com.typesafe.config.ConfigFactory

  @Override val dbName: String =
    ConfigFactory.load().getString("proteus.db")
  val username: String =
    ConfigFactory.load().getString("proteus.user")
  private[this] val password: String =
    ConfigFactory.load().getString("proteus.password")

  val client: DocumentClient = DocumentClient(name = dbName)
  println(s"ArangoDbRepoService: db = $dbName, username = $username")

  def setupCollection(collectionName: String): Boolean = {
    import scala.concurrent.{ Await, ExecutionContext }
    import scala.concurrent.duration._
    import scala.language.postfixOps
    val result = client.createCollection(dbName, collectionName)
    val res = Await.result(result, 5 second)

    res match {
      case Left(err) =>
        println(err)
        false
      case Right(ok) =>
        !res.right.get.name.get.isEmpty()
    }
  }
}

object ArangoDbRepoService {
  /*
  val init = {
    val db = new ArangoDbRepoService()
    db.setupCollection("tokens")
    db.setupCollection("users")
  }*/
}
