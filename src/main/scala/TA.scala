import TA.updateNode

import scala.collection.immutable.Set
import archery._
import scalax.collection.edge.{WLkUnDiEdge, WUnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.collection.mutable

import TurningPoint._

object Constants {
  // $COVERAGE-OFF$
  @inline final val D_EPSILON = 17
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
case class NodeObject(obj: UncertainObject, skyProb: Double, isImpossible: Boolean, distance: Double)
case class NodeGrid(id: Int, x: Double, y: Double, tree: RTree[EntryTuple], objects: Set[NodeObject])

case class GridLocation(x: Int, y: Int) {}
case class EdgeGrid(id: Int, nodei: Int, nodej: Int, length: Option[Double], g: Option[GridLocation], objects: Set[UncertainObject])
case class EdgesNodes(edges: Set[EdgeGrid], nodes: Set[NodeGrid])

case class SkylinePoint(objectId: Int, edgeId: Int, distance: Double, nodeId: Int)

object HelloBox {
  def expandPdr(bbox: Box): Box = {
    val xMin = bbox.lowerLeft.x
    val yMin = bbox.lowerLeft.y
    val xMax = Float.MaxValue
    val yMax = Float.MaxValue

    Box(xMin, yMin, xMax, yMax)
  }

  def expandDdr(bbox: Box): Box = {
    val xMin = bbox.upperRight.x
    val yMin = bbox.upperRight.y
    val xMax = Float.MaxValue
    val yMax = Float.MaxValue

    Box(xMin, yMin, xMax, yMax)
  }
}

object TA {
  def addEdge(graph: Graph[NodeGrid, WLkUnDiEdge], nodei: NodeGrid, nodej: NodeGrid, edge: EdgeGrid): Graph[NodeGrid, WLkUnDiEdge] = {
    graph ++ Graph(WLkUnDiEdge(nodei, nodej)(edge.length.get, edge))
  }

  def addEdges(graph: Graph[NodeGrid, WLkUnDiEdge], nodes: Set[NodeGrid], edges: Set[EdgeGrid]): Graph[NodeGrid, WLkUnDiEdge] = {
    edges.foldLeft(graph)((acc, e) => {
      val nodei = nodes.find((n: NodeGrid) => n.id == e.nodei).get
      val nodej = nodes.find((n: NodeGrid) => n.id == e.nodej).get

      val existingNodei = graph.nodes.find(_.value.id == nodei.id)
      val existingNodej = graph.nodes.find(_.value.id == nodej.id)

      val updatedGraphWithNodei = existingNodei match {
        case None =>
          acc
        case _ =>
          val a = updateNode(acc, existingNodei.get.value)
          a
      }

      val updatedGraphWithNodej = existingNodej match {
        case None =>
          updatedGraphWithNodei
        case _ =>
          updateNode(updatedGraphWithNodei, existingNodej.get.value)
      }

      addEdge(updatedGraphWithNodej, nodei, nodej, e)
    })
  }

  def updateNode(graph: Graph[NodeGrid, WLkUnDiEdge], node: NodeGrid): Graph[NodeGrid, WLkUnDiEdge] = {
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

  def n(g: Graph[NodeGrid, WLkUnDiEdge], outer: NodeGrid): g.NodeT = g get outer

  def calculateDistance(graph: Graph[NodeGrid, WLkUnDiEdge], obj: UncertainObject, nodeId: Int): Double = {
    val node = graph.nodes.find(_.value.id == nodeId).get

    val fakeNode = NodeGrid(0, 0, 0, RTree(), Set())
    val edgeFakeNode = graph.edges.find(e => {
      e.label.asInstanceOf[EdgeGrid].id == obj.edgeId
    }).get

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

  def getDominationProbability(tree: RTree[EntryTuple], ddrOBox: Box, objectId: Int): Double = {
    tree.search(ddrOBox)
      .map(a => {
        a
      })
      .filter(_.value.n == objectId)
      .map(a => {
        a
      })
      .foldLeft(0.0)((acc, e) => acc + e.value.prob)
  }

  def createEntryTuples(obj: UncertainObject): Set[Entry[EntryTuple]] = {
    obj.tuples.map(t =>
      Entry(
        Point(t.x.toFloat, t.y.toFloat),
        EntryTuple(obj.id, t.p)
      )
    )
  }

  def insertToNode(node: NodeGrid, obj: UncertainObject, distance: Double): NodeGrid = {
    val incomingEntries = createEntryTuples(obj)

    val pdrOverlappedObjects = findPdrOverlappedObjects(node, obj)

    val updatedOverlappedObjects = pdrOverlappedObjects.map(q => {
      val ddrObj = expandDdr(obj.bbox)
      val bboxQ = q.obj.bbox
      if (ddrObj.contains(bboxQ)) {
        NodeObject(q.obj, q.skyProb, isImpossible = true, q.distance)
      } else {
        val bbox = expandDdr(obj.bbox)

        val objProb = getDominationProbability(node.tree, bbox, q.obj.id)

        if (objProb > (1 - P_THRESHOLD)) {
          NodeObject(q.obj, q.skyProb, isImpossible = true, q.distance)
        } else {
          val skyProb = SkyPrX(node.tree.insertAll(incomingEntries), q.obj.id)
          NodeObject(q.obj, skyProb, q.isImpossible, q.distance)
        }
      }
    })

    // INSERT OBJECT TO TREE
    val tree = node.tree.insertAll(incomingEntries)

    val updatedObjects = node.objects.map(o => {
      val updated = updatedOverlappedObjects.find(_.obj.id == o.obj.id)

      if (updated.nonEmpty)
        updated.get
      else
        o
    })

    val skyProbU = SkyPrX(tree, obj.id)
    val finalObjects = updatedObjects + NodeObject(obj, skyProbU, isImpossible = false, distance)
    NodeGrid(node.id, node.x, node.y, tree, finalObjects)
  }

  def deleteFromNode(node: NodeGrid, id: Int): NodeGrid = {
    val objMaybe = node.objects
      .find(_.obj.id == id)

    if (objMaybe.isEmpty) {
      return node
    }

    val obj = objMaybe
      .get
      .obj

    val deletingEntries = obj.tuples.map(t =>
      Entry(
        Point(t.x.toFloat, t.y.toFloat),
        EntryTuple(obj.id, t.p)
      )
    )

    val pdrOverlappedObjects = findPdrOverlappedObjects(node, obj)

    val treeORemoved = node.tree.removeAll(deletingEntries)

    val updatedOverlappedObjects = pdrOverlappedObjects.map {
      case q@NodeObject(_, _, false, _) => q
      case NodeObject(obj_, _, isImpossible, distance) =>
        val skyPr = SkyPrX(treeORemoved, obj.id)
        NodeObject(obj_, skyPr, isImpossible, distance)
    }

    val objectsORemoved = updatedOverlappedObjects.filterNot(_.obj.id == id)

    NodeGrid(node.id, node.x, node.y, treeORemoved, objectsORemoved)
  }

  def addTempGraph(g: Graph[NodeGrid, WLkUnDiEdge], grid: GridIndex, nodeId: Int): Graph[NodeGrid, WLkUnDiEdge] = {
    val node = grid.nodes.find(_.id == nodeId).get
    val gridLoc: GridLocation = grid.getGridLocation(node)
    val EdgesNodes(edges, nodes) = grid.getGridEdges(grid, gridLoc)

    val a = addEdges(g, nodes, edges)
    a
  }

  def computeTurningPoint(graph: Graph[NodeGrid, WLkUnDiEdge]): Unit = {
    graph.edges.foreach(e => {
      val nodeSId = e.label.asInstanceOf[EdgeGrid].nodei

      val List(nodeS, nodeE) = if (e._1.toOuter.id == nodeSId) {
        List(e._1.toOuter, e._2.toOuter)
      } else {
        List(e._2.toOuter, e._1.toOuter)
      }

      val edge = e.label.asInstanceOf[EdgeGrid]

      println("\n")
      println("==========")
      println("Process Turning Point edge " + edge.id + " " + nodeS.id + "~" + nodeE.id + " " + edge.length.get)
      processLandmark(nodeS, edge, nodeE)
    })
  }

  case class NodeQueue(nodeId: Int, distance: Double)

  def myAlgo(grid: GridIndex, uncertainData: UncertainStream): GridIndex = {
    var Q: scala.collection.mutable.Queue[NodeQueue] = scala.collection.mutable.Queue[NodeQueue]()
    var tempGrid: Graph[NodeGrid, WLkUnDiEdge] = Graph()

    var updatedNodes: Set[Int] = Set()
    var visitedNodes: Set[Int] = Set()

    println("==============================MOM===============================")
    println("==============================WOW===============================")


    val obj = uncertainData match {
      case uncertainData: UncertainObject =>
        val objInsert = uncertainData.asInstanceOf[UncertainObject]

        grid.addObjectToEdge(objInsert)
        grid.addObject(objInsert)

        println("INSERT object " + objInsert.id + " edge " + objInsert.edgeId + " pos " + objInsert.pos)

        uncertainData.asInstanceOf[UncertainObject]
      case StreamDelete(objectId) =>
        val obj = grid.getObject(objectId).get

        println("DELETE objectId " + objectId)
        grid.removeObjectFromEdge(objectId)
        grid.removeObject(objectId)

        obj
    }

    val edge: EdgeGrid = grid.findEdgeById(obj.edgeId).get

    // enqueue
    val nodei = grid.findNodeById(edge.nodei).get
    val nodej = grid.findNodeById(edge.nodej).get

    tempGrid = addTempGraph(tempGrid, grid, nodei.id)
    tempGrid = addTempGraph(tempGrid, grid, nodej.id)

    val distanceNodei = calculateDistance(tempGrid, obj, nodei.id)
    val distanceNodej = calculateDistance(tempGrid, obj, nodej.id)

    if (distanceNodei < distanceNodej) {
      println("enqueue " + nodei.id)
      Q.enqueue(NodeQueue(nodei.id, distanceNodei))

      visitedNodes = visitedNodes + nodei.id
      println("visit " + nodei.id)
    } else {
      println("enqueue " + nodej.id)
      Q.enqueue(NodeQueue(nodej.id, distanceNodej))

      visitedNodes = visitedNodes + nodej.id
      println("visit " + nodej.id)
    }

    while (Q.nonEmpty) {
      Q = Q.sortBy(_.distance)
      val NodeQueue(currentNodeId, distance) = Q.dequeue()
      println("dequeue " + currentNodeId)

      println("  distance to obj " + distance)
      if (distance < D_EPSILON) {
        val updatedNode = uncertainData match {
          case UncertainObject(_, _, _, _, _, _) =>
            val currentNode = tempGrid.nodes.toOuter.find(_.id == currentNodeId).get
            val distance = calculateDistance(tempGrid, obj, currentNodeId)
            println("  insert to node " + currentNode.id + " objId " + obj.id + " distance " + distance)
            insertToNode(currentNode, obj, distance)
          case StreamDelete(objectId) =>
            val currentNode = tempGrid.nodes.toOuter.find(_.id == currentNodeId).get
            println("  delete from node " + currentNode.id + " objId " + objectId)
            deleteFromNode(currentNode, objectId)
        }

        tempGrid = updateNode(tempGrid, updatedNode)
        grid.updateNode(updatedNode)
        updatedNodes = updatedNodes + updatedNode.id

        val node = tempGrid.nodes.toOuter.find(_.id == currentNodeId).get
        val neighborNodes = tempGrid.find(node)
          .get
          .neighbors
          .map(_.toOuter)

        println("  neighbors " + neighborNodes.map(_.id).toString())

        neighborNodes.foreach(n => {
          if (!visitedNodes.contains(n.id)) {
            tempGrid = addTempGraph(tempGrid, grid, n.id)

            val distanceUnvisitedNode = calculateDistance(tempGrid, obj, n.id)

            Q.enqueue(NodeQueue(n.id, distanceUnvisitedNode))

            println("    enqueue " + n.id)

            visitedNodes = visitedNodes + n.id
            println("    visit " + n.id)
          }
        })
      }
    }

    computeTurningPoint(tempGrid)
    updateGrid(grid, tempGrid)
  }

  def updateGrid(grid: GridIndex, graph: Graph[NodeGrid, WLkUnDiEdge]): GridIndex = {
    grid.updateNodes(graph.nodes.toOuter)

    grid
  }

  def isyNotDominatex(y: Entry[EntryTuple], x: Entry[EntryTuple]): Boolean = {
    val expandedY = Box(y.geom.x, y.geom.y, y.geom.x2, y.geom.y2)

    if (expandDdr(expandedY).contains(x.geom))
      false
    else
      true
  }

  def PrYnotdominatex(Y: List[Entry[EntryTuple]], x: Entry[EntryTuple]): Double = {
    Y.filter(y => isyNotDominatex(y, x))
      .map(a => {
        a
      })
      .foldLeft(0.0)((acc, e) => acc + e.value.prob)
  }

  def SkyPrx(tree: RTree[EntryTuple], X: Set[Entry[EntryTuple]], x: Entry[EntryTuple]): Double = {
    val a = tree.entries
      .toList
      .filterNot(e => X.contains(e))
      .groupBy(_.value.n) // filtered Y Map
      .values
      .map(a => {
        a
      })
      .map(Y => PrYnotdominatex(Y, x))
      .product

    a
  }

  def SkyPrX(tree: RTree[EntryTuple], objectId: Int): Double = {
    val X = tree.entries.filter(_.value.n == objectId).toSet

    X.map(x => {
        val skyPrx = SkyPrx(tree, X, x)
        x.value.prob * skyPrx
      })
      .map(a => {
        a
      })
      .sum
  }

  def readNode(): Set[NodeGrid] ={
    val lines = io.Source.fromFile("/home/syukronrm/Dropbox/TA/dataset/California/cal.cnode.txt").getLines()
    lines.map { l =>
      val lineArray = l.split(' ')
      val nodeId = lineArray(0).toInt
      val lon = lineArray(1).toDouble
      val lat = lineArray(2).toDouble

      NodeGrid(nodeId, lon, lat, RTree(), Set())
    }.toSet
  }

  def readEdge(): Set[EdgeGrid] = {
    val lines = io.Source.fromFile("/home/syukronrm/Dropbox/TA/dataset/California/cal.cedge.txt").getLines()
    lines.map { l =>
      val lineArray = l.split(' ')
      val edgeId = lineArray(0).toInt
      val node1 = lineArray(1).toInt
      val node2 = lineArray(2).toInt
      val distance = lineArray(3).toDouble

      EdgeGrid(edgeId, node1, node2, Some(distance), None, Set())
    }.toSet
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

    val table_edges: Set[EdgeGrid] = Set(
      EdgeGrid(1, 1, 2, None, None, Set()),
      EdgeGrid(2, 1, 3, None, None, Set()),
      EdgeGrid(3, 2, 5, None, None, Set()),
      EdgeGrid(4, 3, 4, None, None, Set()),
      EdgeGrid(5, 3, 6, None, None, Set()),
      EdgeGrid(6, 4, 5, None, None, Set()),
      EdgeGrid(7, 4, 7, None, None, Set()),
      EdgeGrid(8, 5, 8, None, None, Set()),
      EdgeGrid(9, 6, 7, None, None, Set()),
      EdgeGrid(10, 7, 8, None, None, Set()),
      EdgeGrid(11, 4, 6, None, None, Set())
    )

    val streams: List[UncertainStream] = List(
      UncertainObject(1, 1, 0.5, Set(UTuple(5, 7, .6), UTuple(4, 5, .1), UTuple(7, 6, .3)), Box(4, 5, 7, 7), isPossible = true),
      UncertainObject(2, 2, 0.5, Set(UTuple(6, 8, .6), UTuple(4, 4, .1), UTuple(7, 6, .3)), Box(4, 4, 7, 8), isPossible = true),
      StreamDelete(1),
      UncertainObject(3, 2, 0.6, Set(UTuple(5, 6, .4), UTuple(5, 6, .2), UTuple(6, 6, .4)), Box(5, 6, 6, 6), isPossible = true),
      UncertainObject(4, 3, 0.5, Set(UTuple(1, 3, .2), UTuple(3, 2, .3), UTuple(1, 4, .5)), Box(1, 2, 3, 4), isPossible = true)
    )


    import collection.spatial.RTree
    import ta.geometry.Point2d
    import ta.geometry.Rect2d
    val pTree = new RTree[Point2d](new Point2d.Builder, 2, 8, RTree.Split.AXIAL)



    var i = 0
    while ( {
      i < 10
    }) {
      pTree.add(new Point2d(i, i))

      {
        i += 1; i - 1
      }
    }

    val result: Array[Point2d] = new Array[Point2d](10)
    val rect: Rect2d = new Rect2d(new Point2d(2, 2), new Point2d(8, 8))
    val n = pTree.search(rect, result)

    var grid = new GridIndex()

    grid.addNodes(table_nodes)
    grid.addEdges(table_edges)

    grid.calculateEdgesLengthAndGrid()

    val nodeSize = table_nodes.size
    val edgeSize = table_edges.size

    val t0 = System.nanoTime()

    streams.foldLeft(streams) {(acc, stream) => {
      grid = myAlgo(grid, stream)
      acc
    }}

    val t1 = System.nanoTime()
    println("Node size: " + nodeSize)
    println("Edge size: " + edgeSize)
    println("Read file: " + ((t1 - t0) / 1000000) + " ms")
  }
}
