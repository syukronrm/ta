package visualize

import ta.algorithm.TP
import ta.grid.Edge
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import spray.json.DefaultJsonProtocol._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._

import scala.util.{ Failure, Success }
import scala.concurrent.{ Future, Promise }

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
        case Success(res) => println(res)
        case Failure(_)   => sys.error("something wrong")
      }

//    respEntity.andThen {
//      case Success(entity) =>
//        println(s"""{"content": "${entity.toStringUtf8}"}""")
//      case Failure(ex) =>
//        println(s"""{"error": "${ex.getMessage}"}""")
//    }
  }
}