package visualize

import ta.algorithm.TP
import ta.grid.Edge
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import io.circe.generic.auto._
import io.circe.syntax._
import scalax.collection.edge.WLkUnDiEdge
import scalax.collection.immutable.Graph
import ta.{RawEdge, RawNode}
import ta.stream.RawObject

import scala.util.{Failure, Success}
import scala.concurrent.Future

final case class Interval(s: Double, e: Double, sp: List[Int])
final case class EdgeSend(edge: Int, SP: List[Interval], TP: List[Double])

object ObjectConverter {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  def TPtoEdgeSend(turningPoints: List[TP], edge: Edge): EdgeSend = {
    val SP = turningPoints.map { tp =>
      val ids = tp.SP.map(_.id).toList.sorted
      Interval(tp.dStart, tp.dEnd, ids)
    }

    val TP = turningPoints.map(_.dEnd).sorted.dropRight(1)

    EdgeSend(edge.id, SP, TP)
  }

  def objectToRawCoords(objects: Set[RawObject]) = {
    objects.map { o =>
      List(o.id, o.edgeId, o.position)
    }
  }

  def sendRawObject(objects: Set[RawObject]) = {
    val rawCoords = objectToRawCoords(objects)

    val uri = "http://localhost:8080/objects"

    val data = rawCoords.asJson.toString()

    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(uri = uri,method = HttpMethods.POST,
        entity = HttpEntity(ContentTypes.`application/json`, data)
      )
    )

    responseFuture
      .onComplete {
        case Success(res) => None
        case Failure(_)   => sys.error("something wrong")
      }
  }

  def sendTP(turningPoints: List[TP], edge: Edge): Unit = {
    val edgeSend = TPtoEdgeSend(turningPoints, edge)

    val uri = "http://localhost:8080/turningpoints"

    val data = edgeSend.asJson.toString()

    val responseFuture: Future[HttpResponse] = Http().singleRequest(
      HttpRequest(uri = uri,method = HttpMethods.POST,
        entity = HttpEntity(ContentTypes.`application/json`, data)
      )
    )

    responseFuture
      .onComplete {
        case Success(res) => None
        case Failure(_)   => sys.error("something wrong")
      }
  }
}