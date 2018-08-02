package visualize

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer

import scala.io.StdIn
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import visualize.DataNodeEdge._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.HttpMethods._
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.headers.`Access-Control-Allow-Credentials`
import akka.http.scaladsl.model.headers.`Access-Control-Allow-Methods`
import akka.http.scaladsl.model.headers.`Access-Control-Allow-Origin`
import akka.http.scaladsl.model.headers.`Access-Control-Allow-Headers`
import akka.http.scaladsl.model.headers.Origin
import akka.http.scaladsl.server.{Directive0, MethodRejection, RejectionHandler, Route}
import akka.http.scaladsl.server.Directives._


trait CORSHandler{
  private val corsResponseHeaders = List(
    `Access-Control-Allow-Origin`.*,
    `Access-Control-Allow-Credentials`(true),
    `Access-Control-Allow-Headers`("Authorization",
      "Content-Type", "X-Requested-With")
  )
  //this directive adds access control headers to normal responses
  private def addAccessControlHeaders: Directive0 = {
    respondWithHeaders(corsResponseHeaders)
  }
  //this handles preflight OPTIONS requests.
  private def preflightRequestHandler: Route = options {
    complete(HttpResponse(StatusCodes.OK).
      withHeaders(`Access-Control-Allow-Methods`(OPTIONS, POST, PUT, GET, DELETE)))
  }
  // Wrap the Route with this method to enable adding of CORS headers
  def corsHandler(r: Route): Route = addAccessControlHeaders {
    preflightRequestHandler ~ r
  }
  // Helper method to add CORS headers to HttpResponse
  // preventing duplication of CORS headers across code
  def addCORSHeaders(response: HttpResponse):HttpResponse =
    response.withHeaders(corsResponseHeaders)
}

import scala.collection.mutable

final case class Data(data: Set[EdgeSend])
final case class DataObject(data: Set[List[Double]])
final case class GetPath(src: Int, dst: Int)
final case class Path(path: List[Int])

object WebServer {
  implicit val intervalFormat = jsonFormat3(Interval)
  implicit val edgeSendFormat = jsonFormat3(EdgeSend)
  implicit val getPathFormat = jsonFormat2(GetPath)

  val cors = new CORSHandler {}

  def main(args: Array[String]) {
    val TheGraph = new RoadNetwork
    TheGraph.addNodes(table_nodes.toSet)
    TheGraph.addEdges(table_edges.toSet)

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val datas: mutable.Map[Int, EdgeSend] = this.synchronized {
      mutable.Map()
    }

    var objects: Set[List[Double]] = this.synchronized {
      Set()
    }

    def update(edgeSend: EdgeSend): Unit = {
      this.synchronized {
        datas(edgeSend.edge) = edgeSend
      }
    }

    val route = cors.corsHandler(
        path("hello") {
          get {
            complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
          }
      } ~
        post {
          path("turningpoints") {
            entity(as[EdgeSend]) { edgeSend =>
              update(edgeSend)
              complete("update " + edgeSend.edge)
            }
          }
        } ~
      get {
        path("data") {
          val a = datas.values.toSet
          val str = Data(datas.values.toSet).asJson.toString
          complete(HttpEntity(ContentTypes.`application/json`, str))
        }
      } ~
      get {
        path("objects") {
          val str = DataObject(objects).asJson.toString()
          complete(HttpEntity(ContentTypes.`application/json`, str))
        }
      } ~
      post {
        path("objects") {
          entity(as[Set[List[Double]]]) { data =>
            println(data.map(_.lift(0).get).toList.sorted)
            objects = data
            complete("ok")
          }
        }
      } ~
      get {
        path("getPath") {
          parameters('src.as[Int], 'dst.as[Int]) { (src, dst) =>
            val path = TheGraph.getPath(src, dst).asJson.toString
            complete(HttpEntity(ContentTypes.`application/json`, path))
          }
        }
      }
    )

    val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
