package visualize

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.io.StdIn
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import spray.json.DefaultJsonProtocol._

final case class TestPost(id: Int, data: String)

object WebServer {
  implicit val intervalFormat = jsonFormat3(Interval)
  implicit val edgeSendFormat = jsonFormat3(EdgeSend)
  implicit val testPostFormat = jsonFormat2(TestPost)

  def main(args: Array[String]) {

    implicit val system = ActorSystem("my-system")
    implicit val materializer = ActorMaterializer()
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher

    val route =
      path("hello") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
        }
    } ~
      post {
        path("test") {
          entity(as[TestPost]) { order =>
            complete("order ID " + order.id + " " + order.data)
          }
        }

        path("turningpoints") {
          entity(as[EdgeSend]) { edgeSend =>
            println(edgeSend)
            complete(edgeSend.toString)
          }
        }
      }

    val bindingFuture = Http().bindAndHandle(route, "localhost", 8080)

    println(s"Server online at http://localhost:8080/\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}
