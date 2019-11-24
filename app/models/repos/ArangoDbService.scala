package models.repos

import profig._
import com.outr.arango.{ ArangoDB, ArangoDatabase, ArangoException, Credentials, DatabaseState, Document, DocumentModel, Id, Index, IndexType, Serialization }
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._
import com.outr.arango.ArangoCollection
import javax.inject.Singleton
import profig.{ LoadType, Profig, ProfigLookupPath }

case class CursorID(id: String)

@Singleton
class ArangoDbService {
  import com.typesafe.config.ConfigFactory
  implicit val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.global

  Profig.load(ProfigLookupPath("config.json", FileType.Json, LoadType.Merge))

  val dbName: String =
    ConfigFactory.load().getString("proteus.db")
  val username: String =
    ConfigFactory.load().getString("proteus.user")
  private[this] val password: String =
    ConfigFactory.load().getString("proteus.password")

  println(s"ArangoDbRepoService: db = $dbName, username = $username")
  //val db = new ArangoDB()
  val db = new ArangoDB(credentials = Some(Credentials(username, password)))

  def setup() = {
    db.init().map { state =>
      /*
      val stateOk = state == DatabaseState.Initialized
      val sessionOk = db.session.client.request.headers
        .first(Headers.Request.Authorization) != None
      stateOk && sessionOk
       */
      println(s"ArangoDbRepoService: init db = $dbName, username = $username")
      db.api.db(dbName)
    }
  }

  val client: ArangoDatabase = {
    val status = setup()
    Await.result(status, Duration.Inf)
  }

  def setupCollection(collectionName: String): ArangoCollection = {
    import scala.language.postfixOps
    val collection = client.collection(collectionName)
    val result: Future[ArangoCollection] =
      collection.create(waitForSync = Some(true)).map { info =>
        val status = info.name.equals(collectionName)
        collection
      }
    Await.result(result, Duration.Inf)
  }
}

