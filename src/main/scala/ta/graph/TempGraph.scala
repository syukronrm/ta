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

import scala.collection.mutable

class TempGraph {
  var graph: Graph[Node, WLkUnDiEdge] = Graph()

  var graphInt: Graph[Int, WLkUnDiEdge] = Graph()
  var nodesGraph: scala.collection.mutable.Map[Int, Node] = mutable.Map()
  var edgesGraph: scala.collection.mutable.Map[Int, Edge] = mutable.Map()

  def getNeighborNodesEdges(nodeId: Int): Map[Int, Double] = {
    graphInt.get(nodeId).edges.map { e =>
      if (e._1.toOuter == nodeId) {
//        val node = this.nodesGraph(e._2.toOuter)
        e._2.toOuter -> e.weight
      } else {
//        val node = this.nodesGraph(e._1.toOuter)
        e._1.toOuter -> e.weight
      }
    }.toMap
  }

//  def getNeighborNodesEdges(nodeId: Int): Map[Node, Double] = {
//    this.synchronized {
//      val node = this.graph.nodes.toOuter.find(_.id == nodeId).get
//      this.graph.get(node).edges.map { e =>
//        if (e._1.toOuter == node) {
//          e._2.toOuter -> e.weight
//        } else {
//          e._1.toOuter -> e.weight
//        }
//      }.toMap
//    }
//  }

//  def addEdge(graph: Graph[Node, WLkUnDiEdge], nodei: Node, nodej: Node, edge: Edge): Graph[Node, WLkUnDiEdge] = {
//    graph ++ Graph(WLkUnDiEdge(nodei, nodej)(edge.length, edge))
//  }

//  def updateNode(graph: Graph[Node, WLkUnDiEdge], node: Node): Graph[Node, WLkUnDiEdge] = {
////    val deletedNode = graph.nodes.filter(_.toOuter.id == node.id).head
//    val deletedNode = graph.nodes.toOuter.find(_.id == node.id).get
//
//    val newEdges = graph.get(deletedNode).edges.map(e => {
//      val nodeNeighbor = e.nodes.filterNot(n => n == deletedNode).head.toOuter
//      val edgeWeight = e.weight
//      val edgeLabel = e.label
//      WLkUnDiEdge(node, nodeNeighbor)(edgeWeight, edgeLabel)
//    })
//
//    val a = graph - deletedNode ++ newEdges
//    a
//  }

  def updateNode(node: Node): Unit = {
    this.nodesGraph(node.id) = node
  }

//  def updateNode(node: Node): Unit = {
//    this.synchronized {
//      this.graph = updateNode(graph, node)
//    }
//  }

  def getEdge(edgeId: Int): Edge = {
    this.edgesGraph(edgeId)
  }

//  def getEdge(edgeId: Int): Edge = {
//    this.graph.toOuterEdges.find(_.label.asInstanceOf[Edge].id == edgeId).get
//  }.label.asInstanceOf[Edge]

  def addEdges(nodes: Set[Node], edges: Set[Edge]): Unit = {
    nodes.foreach { n =>
      this.nodesGraph(n.id) = n
    }

    edges.foreach { e =>
      this.graphInt = graphInt + WLkUnDiEdge(e.i, e.j)(e.length, e.id)
      this.edgesGraph(e.id) = e

//      val nodeI = nodes.find(_.id == e.i).get
//      val nodeJ = nodes.find(_.id == e.j).get
//
//      this.nodesGraph(e.i) = nodeI
//      this.nodesGraph(e.j) = nodeJ
    }
  }

//  def addEdges(nodes: Set[Node], edges: Set[Edge]): Unit = {
//    this.synchronized {
//      val result = edges.foldLeft(graph)((acc, e) => {
//        val nodei = nodes.find((n: Node) => n.id == e.i).get
//        val nodej = nodes.find((n: Node) => n.id == e.j).get
//
//        val existingNodei = graph.nodes.toOuter.find(_.id == nodei.id)
//        val existingNodej = graph.nodes.toOuter.find(_.id == nodej.id)
//
//        val updatedGraphWithNodei = existingNodei match {
//          case None =>
//            acc
//          case _ =>
//            val a = updateNode(acc, existingNodei.get)
//            a
//        }
//
//        val updatedGraphWithNodej = existingNodej match {
//          case None =>
//            updatedGraphWithNodei
//          case _ =>
//            updateNode(updatedGraphWithNodei, existingNodej.get)
//        }
//
//        addEdge(updatedGraphWithNodej, nodei, nodej, e)
//      })
//
//      graph = result
//    }
//  }

  def getNode(nodeId: Int): Option[Node] = {
    this.nodesGraph.get(nodeId)
  }

//  def n(g: Graph[Node, WLkUnDiEdge], outer: Node): g.NodeT = g get outer
//
//  def getNode(nodeId: Int): Option[Node] = {
//    this.graph.toOuterNodes.find(_.id == nodeId)
//  }
}
