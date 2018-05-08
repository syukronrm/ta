import scalax.collection.edge.WUnDiEdge
import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.GraphPredef._

import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

object GridGraph {
  val graph: Graph[String, WUnDiEdge] = Graph()

  private def n(g: Graph[String, WUnDiEdge], outer: String): g.NodeT = g get outer

  def addEdgeClass(graph: Graph[String, WUnDiEdge], edge: EdgeGrid): Graph[String, WUnDiEdge] =
    addEdge(graph, "n" + edge.nodei.toString, "n" + edge.nodej.toString, edge.length.get)

  def addEdge(graph: Graph[String, WUnDiEdge], nodei: String, nodej: String, weight: Double): Graph[String, WUnDiEdge] =
    graph ++ Graph(nodei~nodej % weight)

  def addEdges(graphSrc: Graph[String, WUnDiEdge], graphDst: Graph[String, WUnDiEdge]): Graph[String, WUnDiEdge] =
    graphSrc ++ graphDst

  def addObject(graph: Graph[String, WUnDiEdge], grid: GridIndex, obj: UncertainObject): Graph[String, WUnDiEdge] = {
    val edge = grid.findEdgeById(obj.edgeId)
    val nodei = grid.findNodeById(edge.get.nodei)
    val nodej = grid.findNodeById(edge.get.nodej)

    val lengthNodei: Double = edge.get.length.get * obj.pos
    val lengthNodej: Double = edge.get.length.get * (1 - obj.pos)

    val g = addEdge(graph,
      "n" + nodei.get.id.toString,
      "u" + obj.id.toString,
      lengthNodei)

    addEdge(g,
      "n" + nodej.get.id.toString,
      "u" + obj.id.toString,
      lengthNodej)
  }

  def getDistance(graph: Graph[String, WUnDiEdge], obj: UncertainObject, node: NodeGrid): Option[Double] = {
    val nodeStr = "n" + node.id.toString
    val objStr = "u" + obj.id.toString

    val sp = n(graph, objStr) shortestPathTo n(graph, nodeStr)

    sp match {
      case Some(s) => Some(s.weight)
      case None => None
    }
  }
}
