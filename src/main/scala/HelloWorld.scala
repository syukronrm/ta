import scala.collection.immutable.Set
import archery._
import scalax.collection.edge.{WLkUnDiEdge, WUnDiEdge}
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

object Constants {
  // $COVERAGE-OFF$
  @inline final val D_EPSILON = 10
  // $COVERAGE-ON$
}

import Constants._

case class UTuple(x: Double, y: Double, p: Double)
case class GridLocation(x: Int, y: Int) {}
case class Node(id: Int, x: Double, y: Double, tree: RTree[UTuple])
case class Edge(id: Int, nodei: Int, nodej: Int, length: Option[Double], g: Option[GridLocation])
case class EdgesNodes(edges: Set[Edge], nodes: Set[Node])

abstract class UncertainStream {
  def getId: Int
}
case class StreamInsert(id: Int, edgeId: Int, pos: Double, tuples: Set[UTuple])
  extends UncertainStream {
  override def getId: Int = id
}
case class StreamDelete(id: Int)
  extends UncertainStream {
  override def getId: Int = id
}

object HelloWorld {
  val table_nodes: Set[Node] = Set(
    Node(1, 2, 1, RTree()),
    Node(2, 19, 1, RTree()),
    Node(3, 3, 3, RTree()),
    Node(4, 9, 5, RTree()),
    Node(5, 16, 5, RTree()),
    Node(6, 3, 8, RTree()),
    Node(7, 8, 12, RTree()),
    Node(8, 16, 12, RTree()),
  )

  val table_edges: Set[Edge] = Set(
    Edge(1, 1, 2, None, None),
    Edge(2, 1, 3, None, None),
    Edge(3, 2, 5, None, None),
    Edge(4, 3, 4, None, None),
    Edge(4, 3, 6, None, None),
    Edge(5, 4, 5, None, None),
    Edge(6, 4, 7, None, None),
    Edge(7, 5, 8, None, None),
    Edge(8, 6, 7, None, None),
    Edge(9, 7, 8, None, None),
  )

  val streams: Set[UncertainStream] = Set(
    StreamInsert(1, 1, 0.5, Set(UTuple(5, 7, .6), UTuple(4, 5, .1), UTuple(7, 6, .3))),
    StreamInsert(1, 2, 0.5, Set(UTuple(5, 7, .6), UTuple(4, 5, .1), UTuple(7, 6, .3))),
    StreamInsert(2, 2, 0.6, Set(UTuple(5, 6, .4), UTuple(5, 6, .2), UTuple(6, 6, .4))),
    StreamInsert(3, 3, 0.5, Set(UTuple(1, 3, .2), UTuple(3, 2, .3), UTuple(1, 4, .5))),
//    StreamDelete(1)
  )

  def addEdge(graph: Graph[Node, WLkUnDiEdge], nodei: Node, nodej: Node, weight: Double, key: Int): Graph[Node, WLkUnDiEdge] = {
    graph ++ Graph(WLkUnDiEdge(nodei, nodej)(weight, key))
  }

  def addEdges(graph: Graph[Node, WLkUnDiEdge], nodes: Set[Node], edges: Set[Edge]): Graph[Node, WLkUnDiEdge] = {
    edges.foldLeft(graph)((acc, e) => {
      val nodei = nodes.find((n: Node) => n.id == e.nodei).get
      val nodej = nodes.find((n: Node) => n.id == e.nodej).get

      addEdge(acc, nodei, nodej, e.length.get, e.id)
    })
  }

  def addGraphsFromNode(graph: Graph[Node, WLkUnDiEdge], grid: GridIndex, node: Node): Graph[Node, WLkUnDiEdge] = {
    val gridLoc = grid.getGridLocation(node)
    val EdgesNodes(edges, nodes) = grid.getGridEdges(grid, gridLoc)

    addEdges(graph, nodes, edges)
  }

  def n(g: Graph[Node, WLkUnDiEdge], outer: Node): g.NodeT = g get outer

  def calculateDistance(graph: Graph[Node, WLkUnDiEdge], obj: StreamInsert, node: Node): Double = {
    val fakeNode = Node(0, 0, 0, RTree())
    val edgeFakeNode = graph.edges.find(_.label == obj.edgeId).get

    val node1 = edgeFakeNode._1.toOuter
    val lenToNode1 = edgeFakeNode.weight * obj.pos

    val node2 = edgeFakeNode._2.toOuter
    val lenToNode2 = edgeFakeNode.weight * (1 - obj.pos)

    val graphNode1: Graph[Node, WLkUnDiEdge] = Graph(WLkUnDiEdge(node1, fakeNode)(lenToNode1, 0))
    val graphNode2: Graph[Node, WLkUnDiEdge] = Graph(WLkUnDiEdge(node2, fakeNode)(lenToNode2, 0))

    val addedGraph = graph ++ graphNode1 ++ graphNode2

    val spO = n(addedGraph, fakeNode) shortestPathTo n(addedGraph, node)
    spO.get.weight
  }

  def insertToRtree(): RTree[UTuple] = {
    val entries1 = Set(Entry(Point(5, 5), UTuple(5, 5, 0.5)), Entry(Point(4, 4), UTuple(5, 4, 0.5)))

    var l1 = Leaf(entries1.toVector, Box(5, 4, 5, 5))

    var b1 = Branch[UTuple](Vector(l1, l1), Box(4, 4, 5, 5))

    var tree = RTree[UTuple]()


//    var tree = rtree.insert(1, 1, UTuple(1, 1, 0.5))
//    tree = tree.insert(1, 2, UTuple(1, 2, 0.2))
//    tree = tree.insert(2, 1, UTuple(1, 1.5, 0.3))
//
//    var t1 = RTree[UTuple](Entry(Point(5, 5), UTuple(5, 5, 0.5)), Entry(Point(4, 4), UTuple(5, 4, 0.5)))
//
//    val entries1 = Set(Entry(Point(5, 5), UTuple(5, 5, 0.5)), Entry(Point(4, 4), UTuple(5, 4, 0.5)))
//
//    var l1 = Leaf(entries1.toVector, Box(5, 4, 5, 5))

    println(b1.pretty)
    RTree[UTuple]()
  }


  def theAlgorithm(grid: GridIndex, uncertainData: UncertainStream): GridIndex = {
    var gTmp = GridGraph
    var queue: scala.collection.mutable.Queue[Node] = scala.collection.mutable.Queue[Node]()
    var graph: Graph[Node, WLkUnDiEdge] = Graph()

    var updatedNodes: Set[Node] = Set()
    var visitedNodes: Set[Node] = Set()

    val edgeId: Int = uncertainData match {
      case StreamInsert(_, edgeId_, _, _) => edgeId_
    }

    var edge: Edge = grid.findEdgeById(edgeId).get

    // enqueue
    val nodei = grid.findNodeById(edge.nodei).get
    val nodej = grid.findNodeById(edge.nodej).get

    queue.enqueue(nodei)
    queue.enqueue(nodej)

    while (queue.nonEmpty) {
      var node = queue.dequeue()

      graph = addGraphsFromNode(graph, grid, node)
      val len = calculateDistance(graph, uncertainData.asInstanceOf[StreamInsert], node)

      if (len < D_EPSILON) {

      }
    }

    grid
  }

  def main(args: Array[String]): Unit = {
    var grid = new GridIndex()

    grid.addNodes(table_nodes)
    grid.addEdges(table_edges)

    grid.calculateEdgesLengthAndGrid()

    insertToRtree()

    streams.foldLeft(streams) {(acc, stream) => {
      theAlgorithm(grid, stream)
      acc
    }}
  }
}
