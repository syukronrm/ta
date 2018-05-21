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

  def getNeighborNodes(nodeId: Int): Set[Node] = {
    val node = this.graph.nodes.toOuter.find(_.id == nodeId).get
    this.graph.find(node).get
      .neighbors
      .map(_.toOuter)
  }

  def addEdge(graph: Graph[Node, WLkUnDiEdge], nodei: Node, nodej: Node, edge: Edge): Graph[Node, WLkUnDiEdge] = {
    graph ++ Graph(WLkUnDiEdge(nodei, nodej)(edge.length, edge))
  }

  def updateNode(graph: Graph[Node, WLkUnDiEdge], node: Node): Graph[Node, WLkUnDiEdge] = {
    val deletedNode = graph.nodes.filter(_.toOuter.id == node.id).head

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
    this.graph = updateNode(graph, node)
  }

  def addEdges(nodes: Set[Node], edges: Set[Edge]): Unit = {
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

  def n(g: Graph[Node, WLkUnDiEdge], outer: Node): g.NodeT = g get outer

  def calculateDistance(rawObject: RawObject, nodeId: Int): Double = {
    val node = graph.nodes.toOuter.find(_.id == nodeId).get

    val fakeNode = Node(0, 0, 0, createTree2D(), Set())

    val edgeFakeNodeMaybe = this.graph.toOuterEdges.find(e => {
      e.label.asInstanceOf[Edge].id == rawObject.edgeId
    })

    edgeFakeNodeMaybe match {
      case None =>
        val edgeId = rawObject.edgeId
        throw new Error("Edge " + edgeId + " does not exist on the graph")
      case _ =>
        None
    }

    val edgeFakeNode = edgeFakeNodeMaybe.get


    val node1 = edgeFakeNode._1
    val lenToNode1 = edgeFakeNode.weight * rawObject.position

    val node2 = edgeFakeNode._2
    val lenToNode2 = edgeFakeNode.weight * (1 - rawObject.position)

    val graphNode1: Graph[Node, WLkUnDiEdge] = Graph(WLkUnDiEdge(node1, fakeNode)(lenToNode1, 0))
    val graphNode2: Graph[Node, WLkUnDiEdge] = Graph(WLkUnDiEdge(node2, fakeNode)(lenToNode2, 0))

    val addedGraph = graph ++ graphNode1 ++ graphNode2

    val spO = n(addedGraph, fakeNode) shortestPathTo n(addedGraph, node)

    spO.get.weight
  }

  def getNode(nodeId: Int): Option[Node] = {
    val maybeNode = this.graph.toOuterNodes.find(_.id == nodeId)

    maybeNode match {
      case Some(node) =>
        Some(node)
      case None =>
        None
    }
  }
}
