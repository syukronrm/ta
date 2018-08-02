package visualize

import scalax.collection.edge.WLkUnDiEdge
import scalax.collection.immutable.Graph
import ta.naive_approach.Edge
import ta.stream.RawObject
import ta.{RawEdge, RawNode}

class RoadNetwork {
  var graph: Graph[Int, WLkUnDiEdge] = Graph[Int, WLkUnDiEdge]()
  var rawNodes: Set[RawNode] = Set()

  def addNodes(ids: Set[RawNode]): Unit = {
    ids.foreach { rawNode =>
      rawNodes += rawNode
      addNode(rawNode.id)
    }
  }

  def addNode(i: Int): Unit = {
    graph = graph + i
  }

  def addEdges(edges: Set[RawEdge]): Unit = {
    edges.foreach { e =>
      val nodeI = rawNodes.find(_.id == e.i).get
      val nodeJ = rawNodes.find(_.id == e.j).get

      val diffX = nodeJ.x - nodeI.x
      val diffY = nodeJ.y - nodeJ.y

      val length = math.sqrt(diffX*diffX + diffY*diffY)

      addEdge(e.i, e.j, length, e.id)
    }
  }

  def addEdge(i: Int, j: Int, len: Double, id: Int): Unit = {
    graph = graph + WLkUnDiEdge(i, j)(len, id)
  }

  def nGraph(g: Graph[Int, WLkUnDiEdge], outer: Int) = g get outer

  def getPath(srcId: Int, dstId: Int): List[Int] = {
    val g = this.graph
    val sp = nGraph(g, srcId) shortestPathTo nGraph(g, dstId)
    val path = sp.get.nodes.map(_.toOuter)
    path.toList
  }
}
