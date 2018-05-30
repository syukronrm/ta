package ta.graph

import archery.RTree
import collection.spatial.{HyperPoint, RTree}
import scalax.collection.GraphPredef.OuterNode
import scalax.collection.immutable.Graph
import scalax.collection.edge.WLkUnDiEdge
import ta.grid.{Edge, Node, TheTree}
import ta.stream.RawObject
import ta.grid.TheTree._
import scalax.collection.edge.Implicits._


class TempGraph {
  var graph: Graph[Node, WLkUnDiEdge] = Graph()

  def getNeighborNodesEdges(nodeId: Int): Map[Node, Double] = {
    this.synchronized {
      val node = this.graph.nodes.toOuter.find(_.id == nodeId).get
      this.graph.get(node).edges.map { e =>
        if (e._1.toOuter == node) {
          e._2.toOuter -> e.weight
        } else {
          e._1.toOuter -> e.weight
        }
      }.toMap
    }
  }

  def addEdge(graph: Graph[Node, WLkUnDiEdge], nodei: Node, nodej: Node, edge: Edge): Graph[Node, WLkUnDiEdge] = {
    graph ++ Graph(WLkUnDiEdge(nodei, nodej)(edge.length, edge))
  }

  def updateNode(graph: Graph[Node, WLkUnDiEdge], node: Node): Graph[Node, WLkUnDiEdge] = {
//    val deletedNode = graph.nodes.filter(_.toOuter.id == node.id).head
    val deletedNode = graph.nodes.toOuter.find(_.id == node.id).get

    val newEdges = graph.get(deletedNode).edges.map(e => {
      val nodeNeighbor = e.nodes.filterNot(n => n == deletedNode).head.toOuter
      val edgeWeight = e.weight
      val edgeLabel = e.label
      WLkUnDiEdge(node, nodeNeighbor)(edgeWeight, edgeLabel)
    })

    val a = graph - deletedNode ++ newEdges
    a
  }

  def updateNode(node: Node): Unit = {
    this.synchronized {
      this.graph = updateNode(graph, node)
    }
  }

  def getEdge(edgeId: Int): Edge = {
    this.graph.toOuterEdges.find(_.label.asInstanceOf[Edge].id == edgeId).get
  }.label.asInstanceOf[Edge]

  def addEdges(nodes: Set[Node], edges: Set[Edge]): Unit = {
    this.synchronized {
      val result = edges.foldLeft(graph)((acc, e) => {
        val nodei = nodes.find((n: Node) => n.id == e.i).get
        val nodej = nodes.find((n: Node) => n.id == e.j).get

        val existingNodei = graph.nodes.toOuter.find(_.id == nodei.id)
        val existingNodej = graph.nodes.toOuter.find(_.id == nodej.id)

        val updatedGraphWithNodei = existingNodei match {
          case None =>
            acc
          case _ =>
            val a = updateNode(acc, existingNodei.get)
            a
        }

        val updatedGraphWithNodej = existingNodej match {
          case None =>
            updatedGraphWithNodei
          case _ =>
            updateNode(updatedGraphWithNodei, existingNodej.get)
        }

        addEdge(updatedGraphWithNodej, nodei, nodej, e)
      })

      graph = result
    }
  }

  def n(g: Graph[Node, WLkUnDiEdge], outer: Node): g.NodeT = g get outer

  def getNode(nodeId: Int): Option[Node] = {
    this.graph.toOuterNodes.find(_.id == nodeId)
  }
}
