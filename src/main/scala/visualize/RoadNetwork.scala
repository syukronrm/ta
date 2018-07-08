package visualize

import scalax.collection.edge.WLkUnDiEdge
import scalax.collection.immutable.Graph
import ta.naive_approach.Edge
import ta.stream.RawObject
import ta.{RawEdge, RawNode}

class RoadNetwork {
//  var graph: Graph[Int, WLkUnDiEdge] = Graph()
//  var rawNodes: Set[RawNode] = Set()
//
//  def addNodes(ids: Set[Int]): Unit = {
//    ids.foreach { id =>
//      addNode(id)
//    }
//  }
//
//  def addNode(i: Int): Unit = {
//    graph = graph + i
//  }
//
//  def loadGraph(edges: Set[RawEdge]): Unit = {
//    edges.foreach { e =>
//      val nodeI = rawNodes.find(_.id == e.i).get
//      val nodeJ = rawNodes.find(_.id == e.j).get
//
//      val diffX = nodeJ.x - nodeI.x
//      val diffY = nodeJ.y - nodeJ.y
//
//      val length = math.sqrt(diffX*diffX + diffY*diffY)
//
//      addEdge(e.i, e.j, length, e.id)
//    }
//  }
//
//  def addEdge(i: Int, j: Int, len: Double, id: Int): Unit = {
//    graph = graph + WLkUnDiEdge(i, j)(len, id)
//  }
//
//  def addObject(obj: RawObject, edge: Edge): Graph[Int, WLkUnDiEdge] = {
//    val lengthNodei: Double = edge.length * obj.position
//    val lengthNodej: Double = edge.length * (1 - obj.position)
//
//    val g1 = graph + WLkUnDiEdge(edge.i, 0)(lengthNodei, edge.id)
//    val g2 = g1 + WLkUnDiEdge(edge.j, 0)(lengthNodej, edge.id)
//
//    g2
//  }
//
//  private def n(g: Graph[Int, WLkUnDiEdge], outer: Int): g.NodeT = g get outer
//
//  def findDistance(obj: RawObject, edge: Edge, nodeId: Int): Option[Double] = {
//    val g2 = addObject(obj, edge)
//
//    val sp = n(g2, 0) shortestPathTo n(g2, nodeId)
//
//    sp match {
//      case Some(s) => Some(s.weight)
//      case None => None
//    }
//  }
//
//  def getPath(srcId: Int, dstId: Int): List[Int] = {
//    val sp = nGraph(this.graph, srcId) shortestPathTo nGraph(this.graph, dstId)
//    val path = sp.get.nodes.map(_.toOuter)
//    path.toList
//  }
//
//  def findDistance(g: Graph[Int, WLkUnDiEdge], nodeId: Int) = {
//    val sp = n(g, 0) shortestPathTo n(g, nodeId)
//
//    sp match {
//      case Some(s) => Some(s.weight)
//      case None => None
//    }
//  }
//
//  def getNodesByDistance(g: Graph[Int, WLkUnDiEdge], objId: Int, nodes: Set[RawNode]) = {
//    nodes.par.map { n =>
//      n.id -> findDistance(g, n.id)
//    }.toList.toMap
//  }

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
