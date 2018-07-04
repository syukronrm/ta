package ta.graph

import scalax.collection.immutable.Graph
import scalax.collection.edge.WLkUnDiEdge
import ta.grid.{Edge, Node}

import scala.collection.mutable

class TempGraph {
  var graphInt: Graph[Int, WLkUnDiEdge] = Graph()
  var nodesGraph: scala.collection.mutable.Map[Int, Node] = mutable.Map()
  var edgesGraph: scala.collection.mutable.Map[Int, Edge] = mutable.Map()

  def getNeighborNodesEdges(nodeId: Int): Map[Int, Double] = {
    graphInt.get(nodeId).edges.map { e =>
      if (e._1.toOuter == nodeId) {
        e._2.toOuter -> e.weight
      } else {
        e._1.toOuter -> e.weight
      }
    }.toMap
  }

  def updateNode(node: Node): Unit = {
    this.nodesGraph(node.id) = node
  }

  def getEdge(edgeId: Int): Edge = {
    this.edgesGraph(edgeId)
  }

  def addEdges(nodes: Set[Node], edges: Set[Edge]): Unit = {
    nodes.foreach { n =>
      this.nodesGraph(n.id) = n
    }

    edges.foreach { e =>
      this.graphInt = graphInt + WLkUnDiEdge(e.i, e.j)(e.length, e.id)
      this.edgesGraph(e.id) = e
    }
  }

  def getNode(nodeId: Int): Option[Node] = {
    this.nodesGraph.get(nodeId)
  }
}
