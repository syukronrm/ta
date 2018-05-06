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
  @inline final val P_THRESHOLD = 0.5
  // $COVERAGE-ON$
}

import Constants._

import HelloBox._

abstract class UncertainStream {
  def getId: Int
}
case class UncertainObject(id: Int, edgeId: Int, pos: Double, tuples: Set[UTuple], bbox: Box, isPossible: Boolean)
  extends UncertainStream {
  override def getId: Int = id
}
case class StreamDelete(id: Int)
  extends UncertainStream {
  override def getId: Int = id
}

case class UTuple(x: Double, y: Double, p: Double)
case class EntryTuple(n: Int, prob: Double)
case class NodeObject(obj: UncertainObject, skyProb: Double, isImpossible: Boolean)
case class NodeGrid(id: Int, x: Double, y: Double, tree: RTree[EntryTuple], objects: Set[NodeObject])

case class GridLocation(x: Int, y: Int) {}
case class Edge(id: Int, nodei: Int, nodej: Int, length: Option[Double], g: Option[GridLocation])
case class EdgesNodes(edges: Set[Edge], nodes: Set[NodeGrid])


object HelloBox {
  def expandPdr(bbox: Box): Box = {
    val xMin = bbox.lowerLeft.x
    val yMin = bbox.lowerLeft.y
    val xMax = Float.PositiveInfinity
    val yMax = Float.PositiveInfinity

    Box(xMin, yMin, xMax, yMax)
  }

  def expandDdr(bbox: Box): Box = {
    val xMin = bbox.upperRight.x
    val yMin = bbox.upperRight.y
    val xMax = Float.PositiveInfinity
    val yMax = Float.PositiveInfinity

    Box(xMin, yMin, xMax, yMax)
  }
}

object HelloWorld {
  def addEdge(graph: Graph[NodeGrid, WLkUnDiEdge], nodei: NodeGrid, nodej: NodeGrid, weight: Double, key: Int): Graph[NodeGrid, WLkUnDiEdge] = {
    graph ++ Graph(WLkUnDiEdge(nodei, nodej)(weight, key))
  }

  def addEdges(graph: Graph[NodeGrid, WLkUnDiEdge], nodes: Set[NodeGrid], edges: Set[Edge]): Graph[NodeGrid, WLkUnDiEdge] = {
    edges.foldLeft(graph)((acc, e) => {
      val nodei = nodes.find((n: NodeGrid) => n.id == e.nodei).get
      val nodej = nodes.find((n: NodeGrid) => n.id == e.nodej).get

      addEdge(acc, nodei, nodej, e.length.get, e.id)
    })
  }

  def addGraphsFromNodeGrid(graph: Graph[NodeGrid, WLkUnDiEdge], grid: GridIndex, node: NodeGrid): Graph[NodeGrid, WLkUnDiEdge] = {
    val gridLoc = grid.getGridLocation(node)
    val EdgesNodes(edges, nodes) = grid.getGridEdges(grid, gridLoc)

    addEdges(graph, nodes, edges)
  }

  def n(g: Graph[NodeGrid, WLkUnDiEdge], outer: NodeGrid): g.NodeT = g get outer

  def calculateDistance(graph: Graph[NodeGrid, WLkUnDiEdge], obj: UncertainObject, node: NodeGrid): Double = {
    val fakeNode = NodeGrid(0, 0, 0, RTree(), Set())
    val edgeFakeNode = graph.edges.find(_.label == obj.edgeId).get

    val node1 = edgeFakeNode._1.toOuter
    val lenToNode1 = edgeFakeNode.weight * obj.pos

    val node2 = edgeFakeNode._2.toOuter
    val lenToNode2 = edgeFakeNode.weight * (1 - obj.pos)

    val graphNode1: Graph[NodeGrid, WLkUnDiEdge] = Graph(WLkUnDiEdge(node1, fakeNode)(lenToNode1, 0))
    val graphNode2: Graph[NodeGrid, WLkUnDiEdge] = Graph(WLkUnDiEdge(node2, fakeNode)(lenToNode2, 0))

    val addedGraph = graph ++ graphNode1 ++ graphNode2

    val spO = n(addedGraph, fakeNode) shortestPathTo n(addedGraph, node)
    spO.get.weight
  }

  def findPdrOverlappedObjects(node: NodeGrid, obj: UncertainObject): Set[NodeObject] = {
    val boxStream = obj.bbox
    val tree = node.tree

    val pdrBox = expandPdr(boxStream)
    val overlappedTuples = tree.search(pdrBox)

    val nodeIds = overlappedTuples.map(o => o.value.n).toSet[Int]
    val objects = nodeIds.map(id => node.objects.find(_.obj.id == id).get)

    objects
  }

  def insertToNode(obj: UncertainObject, node: NodeGrid): NodeGrid = {
    val incomingEntries = obj.tuples.map(t =>
      Entry(
        Point(t.x.toFloat, t.y.toFloat),
        EntryTuple(obj.id, t.p)
      )
    )

    // FIND OBJECT OVERLAPPED PDR
    val pdrOverlappedObjects = findPdrOverlappedObjects(node, obj)

    val updatedOverlappedObjects = pdrOverlappedObjects.map(q => {
      if (expandDdr(obj.bbox).contains(q.obj.bbox)) {
        NodeObject(q.obj, q.skyProb, isImpossible = true)
      } else {
        val objProb = node.tree
          .search(expandDdr(obj.bbox))
          .filter(_.value.n == q.obj.id)
          .foldLeft(0.0)((acc, e) => acc + e.value.prob)

        if (objProb > (1 - P_THRESHOLD)) {
          NodeObject(q.obj, q.skyProb, isImpossible = true)
        } else {
          // TODO UPDATE SKYPROB q
          val skyProb = SkyPrX(node.tree.insertAll(incomingEntries), q.obj.id)
          NodeObject(q.obj, skyProb, isImpossible = true)
        }
      }
    })

    // INSERT OBJECT TO TREE
    val tree = node.tree.insertAll(incomingEntries)

    val updatedObjects = node.objects.map(o => {
      val updated = updatedOverlappedObjects.find(_.obj.id == o.obj.id)

      if (updated.nonEmpty)
        updated

      o
    })

    val skyProbU = SkyPrX(tree, obj.id)
    val finalObjects = updatedObjects + NodeObject(obj, skyProbU, isImpossible = false)
    NodeGrid(node.id, node.x, node.y, tree, finalObjects)
  }


  def theAlgorithm(grid: GridIndex, uncertainData: UncertainStream): GridIndex = {
    var queue: scala.collection.mutable.Queue[NodeGrid] = scala.collection.mutable.Queue[NodeGrid]()
    var graph: Graph[NodeGrid, WLkUnDiEdge] = Graph()

    var updatedNodes: Set[NodeGrid] = Set()
    var visitedNodes: Set[NodeGrid] = Set()

    val edgeId: Int = uncertainData match {
      case UncertainObject(_, edgeId_, _, _, _, _) => edgeId_
    }

    var edge: Edge = grid.findEdgeById(edgeId).get

    // enqueue
    val nodei = grid.findNodeById(edge.nodei).get
    val nodej = grid.findNodeById(edge.nodej).get

    queue.enqueue(nodei)
    queue.enqueue(nodej)

    while (queue.nonEmpty) {
      var node = queue.dequeue()

      graph = addGraphsFromNodeGrid(graph, grid, node)
      val len = calculateDistance(graph, uncertainData.asInstanceOf[UncertainObject], node)

      if (len < D_EPSILON) {
        node = insertToNode(uncertainData.asInstanceOf[UncertainObject], node)
      }
    }

    grid
  }

  def isPryNotDominatex(y: Entry[EntryTuple], x: Entry[EntryTuple]): Boolean = {
    val expandedY = Box(y.geom.x, y.geom.y, y.geom.x2, y.geom.y2)

    if (expandDdr(expandedY).contains(x.geom))
      true

    false
  }

  def PrYnotdominatex(Y: List[Entry[EntryTuple]], x: Entry[EntryTuple]): Double = {
    Y.filter(y => isPryNotDominatex(y, x))
      .foldLeft(0.0)((acc, e) => acc + e.value.prob)
  }

  def SkyPrx(tree: RTree[EntryTuple], X: Iterator[Entry[EntryTuple]], x: Entry[EntryTuple]): Double = {
    val a = tree.entries
      .toList
      .filterNot(e => X.contains(e))
      .groupBy(_.value.n) // filtered Y Map
      .values
      .map(Y => PrYnotdominatex(Y, x))
      .product

    a
  }

  def SkyPrX(tree: RTree[EntryTuple], objectId: Int): Double = {
    val X = tree.entries.filter(_.value.n == objectId)

    tree.entries
      .map(e => e.value.prob * SkyPrx(tree, X, e))
      .sum
  }

  def main(args: Array[String]): Unit = {
    val table_nodes: Set[NodeGrid] = Set(
      NodeGrid(1, 2, 1, RTree(), Set()),
      NodeGrid(2, 19, 1, RTree(), Set()),
      NodeGrid(3, 3, 3, RTree(), Set()),
      NodeGrid(4, 9, 5, RTree(), Set()),
      NodeGrid(5, 16, 5, RTree(), Set()),
      NodeGrid(6, 3, 8, RTree(), Set()),
      NodeGrid(7, 8, 12, RTree(), Set()),
      NodeGrid(8, 16, 12, RTree(), Set())
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
      Edge(9, 7, 8, None, None)
    )

    val streams: Set[UncertainStream] = Set(
      UncertainObject(1, 1, 0.5, Set(UTuple(5, 7, .6), UTuple(4, 5, .1), UTuple(7, 6, .3)), Box(4, 5, 7, 7), isPossible = true),
      UncertainObject(2, 2, 0.5, Set(UTuple(5, 7, .6), UTuple(4, 5, .1), UTuple(7, 6, .3)), Box(4, 5, 7, 7), isPossible = true),
      UncertainObject(3, 2, 0.6, Set(UTuple(5, 6, .4), UTuple(5, 6, .2), UTuple(6, 6, .4)), Box(5, 6, 6, 6), isPossible = true),
      UncertainObject(4, 3, 0.5, Set(UTuple(1, 3, .2), UTuple(3, 2, .3), UTuple(1, 4, .5)), Box(1, 2, 3, 4), isPossible = true)
      //    StreamDelete(1)
    )

    var grid = new GridIndex()

    grid.addNodes(table_nodes)
    grid.addEdges(table_edges)

    grid.calculateEdgesLengthAndGrid()

    streams.foldLeft(streams) {(acc, stream) => {
      theAlgorithm(grid, stream)
      acc
    }}
  }
}
