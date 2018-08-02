package ta.algorithm

import collection.spatial.RTree
import scalax.collection.Graph
import scalax.collection.edge.WLkUnDiEdge
import ta.graph.TempGraph
import ta.grid._
import ta.stream.{ExpiredObject, RawObject, Stream}
import ta.Constants._
import ta.geometry.{Point2d, Rect2d}
import ta.algorithm.TurningPoint._

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.collection.mutable

case class NodeQueue(nodeId: Int, distance: Double)

object TheAlgorithm {
  def TheAlgorithm(grid: Grid, stream: Stream): Grid = {
    var Q: scala.collection.mutable.Queue[NodeQueue] = scala.collection.mutable.Queue[NodeQueue]()
    val tempGraph = new TempGraph

    var visitedNodes: Set[Int] = Set()
    var updatedNodes: Set[Int] = Set()

    val rawObject = stream match {
      case _rawObject: RawObject =>
        grid.addObjectToEdge(_rawObject)
        grid.addRawObject(_rawObject)

        STATE = "INSERTION"

        _rawObject
      case ExpiredObject(id) =>
        val _rawObject = grid.getRawObject(id).get

        STATE = "DELETION"

        _rawObject
    }

    val objectList: java.util.List[Point2d] = rawObject.points.toList.asJava
    val rect = new Rect2d(objectList)

    // grid yang sudah dimasukkan pada temporary tempGraph
    var addedGrid: Set[GridLocation] = Set()

    val edge = grid.getEdge(rawObject.edgeId).get
    val nodei = grid.getNode(edge.i).get
    val nodej = grid.getNode(edge.j).get

    val gridNodeI = grid.getGridLocation(nodei)
    val gridNodeJ = grid.getGridLocation(nodej)

    val EdgesNodes(edgesNodeI, nodesNodeI) = grid.getDataGrid(gridNodeI)
    tempGraph.addEdges(nodesNodeI, edgesNodeI)
    addedGrid += gridNodeI

    if (!addedGrid.contains(gridNodeJ)) {
      val EdgesNodes(edgesNodeJ, nodesNodeJ) = grid.getDataGrid(gridNodeJ)
      tempGraph.addEdges(nodesNodeJ, edgesNodeJ)
      addedGrid += gridNodeJ
    }

    val e = tempGraph.getEdge(rawObject.edgeId)
    val distanceNodeI = e.length * rawObject.position
    val distanceNodeJ = e.length * (1 - rawObject.position)

    Q.enqueue(NodeQueue(nodei.id, distanceNodeI))
    visitedNodes = visitedNodes + nodei.id
    Q.enqueue(NodeQueue(nodej.id, distanceNodeJ))
    visitedNodes = visitedNodes + nodej.id

    while (Q.nonEmpty) {
      Q = Q.sortBy(_.distance)
      val NodeQueue(currentNodeId, distance) = Q.dequeue()

      if (distance < D_EPSILON) {
        val currentNode = grid.getNode(currentNodeId).get

        val gridLocation = grid.getGridLocation(currentNode)

        if (!addedGrid.contains(gridLocation)) {
          val EdgesNodes(edgesN, nodesN) = grid.getDataGrid(gridLocation)
          tempGraph.addEdges(nodesN, edgesN)
          addedGrid += gridLocation
        }

        val updatedNode = stream match {
          case _: RawObject =>
            insertToNode(grid, currentNode, rawObject, distance, rect)
          case ExpiredObject(objectId) =>
            deleteFromNode(grid, currentNode, objectId, rect)
        }

        tempGraph.updateNode(updatedNode)
        grid.updateNode(updatedNode)
        updatedNodes += updatedNode.id

        val neighborNodesEdges = tempGraph.getNeighborNodesEdges(currentNodeId)

        neighborNodesEdges.keys.foreach { nodeId =>
          if (!visitedNodes.contains(nodeId)) {
            val distanceUnvisitedNode = distance + neighborNodesEdges(nodeId)
            Q.enqueue(NodeQueue(nodeId, distanceUnvisitedNode))
            visitedNodes += nodeId
          }
        }
      }
    }

    if (stream.isInstanceOf[RawObject])
      tempGraph.addObjectToEdge(rawObject)
    else
      tempGraph.removeObjectFromEdge(rawObject)

    computeTurningPoint(grid, tempGraph.edgesGraph, tempGraph.nodesGraph, updatedNodes, edge)

    if (stream.isInstanceOf[ExpiredObject]) {
      grid.removeObjectFromEdge(stream.getId)
      grid.removeRawObject(stream.getId)
    }

    grid
  }

  def updateGrid(grid: Grid, graph: Graph[Node, WLkUnDiEdge]): Grid = {
    grid.updateNodes(graph.nodes.toOuter)

    grid
  }

  def computeTurningPoint(grid: Grid, edges: mutable.Map[Int, Edge], nodes: mutable.Map[Int, Node], updatedNodes: Set[Int], edge: Edge): Unit = {
    edges.values
      // pilih edge yang berhubungan langsung dengan node yang terupdate
      .filter(e => updatedNodes.contains(e.i) | updatedNodes.contains(e.j) | e.id == edge.id)
      .foreach { e =>
        print(" e" + e.id)
        val nodeS = nodes(e.i)
        val nodeE = nodes(e.j)

        processLandmark(grid, nodeS, e, nodeE)
      }
  }

  def deleteFromNode(grid: Grid, currentNode: Node, objectId: Int, rect: Rect2d): Node = {
    val objectMaybe = currentNode.objects.find(_.id == objectId)

    if (objectMaybe.isEmpty) {
      return currentNode
    }

    val toBeDeletedPoints = objectMaybe.get.points(grid)

    for (point <- toBeDeletedPoints) {
      currentNode.tree.remove(point)
    }

    var overlappedObjects = findPDROverlappedObjects(currentNode, rect)

    overlappedObjects = overlappedObjects.map {
      case q@Object(_, _, _, true, _, _, _, _) => q
      case q@Object(_, _, _, _, _, _, _, _) =>
        val skyProb = SkyPrX(currentNode.tree, q.id)
        q.updateSkyProb(skyProb)
    }

    var newObjects = currentNode.objects.map { o =>
      val updatedObjectMaybe = overlappedObjects.find(_.id == o.id)

      updatedObjectMaybe match {
        case None => o
        case Some(obj) => obj
      }
    }

    newObjects = newObjects.filterNot(_.id == objectId)

    Node(currentNode.id, currentNode.x, currentNode.y, currentNode.tree, newObjects)
  }

  def insertToNode(grid: Grid, node: Node,
                   rawObject: RawObject,
                   distance: Double,
                   rect: Rect2d): Node = {

    var overlappedObjects = findPDROverlappedObjects(node, rect)

    rawObject.points.foreach(p => node.tree.add(p))

    val updatedOverlappedObjects = overlappedObjects.map(q => {
      val ddrRect = rect.getDDR.asInstanceOf[Rect2d]
      val qRect = q.rect

      if (ddrRect.contains(qRect) & distance < q.distance) {
        q.asImpossible()
      } else {
        val objProb = getDominationProbability(node.tree, ddrRect, q.id)
        if (objProb > (1 - P_THRESHOLD) & distance < q.distance) {
          q.asImpossible()
        } else {
          val skyProb = SkyPrX(node.tree, q.id)
          q.updateSkyProb(skyProb)
        }
      }
    })

    val updatedObjects = node.objects.map(o => {
      val updated = updatedOverlappedObjects.find(_.id == o.id)

      if (updated.nonEmpty)
        updated.get
      else
        o
    })

    val PDDoverlappedObjects = findPDDOverlappedObjects(node, rect, rawObject.id)
    val pddTree = new RTree(new Point2d.Builder, 2, 8, RTree.Split.AXIAL)
    PDDoverlappedObjects.foreach { o =>
      val points = grid.getRawObject(o.id).get.points
      points.foreach { p =>
        pddTree.add(p)
      }
    }

    rawObject.points.foreach { p => pddTree.add(p) }

    val skyProbU = SkyPrX(pddTree, rawObject.id)

    val finalObjects = updatedObjects +
        Object(rawObject.id, rawObject.edgeId, skyProbU, isImpossible = false, node.id, rect, distance, rawObject.position)
    Node(node.id, node.x, node.y, node.tree, finalObjects)
  }

  def SkyPrX(tree: RTree[Point2d], objectId: Int): Double = {
    val X = scala.collection.mutable.Set[Point2d]()
    tree.forEach { p =>
      if (p.o == objectId) X.add(p)
    }

    X.map(x => x.p * SkyPrx(tree, X, x))
      .sum
  }

  def SkyPrx(tree: RTree[Point2d], X: mutable.Set[Point2d], x: Point2d): Double = {
    val entries = mutable.Set[Point2d]()
    tree.forEach(p => entries.add(p))

    entries
      .toList
      .filterNot(e => X.contains(e))
      .groupBy(_.o)
      .values
      .map(Y => PrYnotdominatex(Y, x))
      .product
  }

  def PrYnotdominatex(Y: List[Point2d], x: Point2d): Double = {
    Y.filter(y => isyNotDominatex(y, x))
      .foldLeft(0.0)((acc, e) => acc + e.p)
  }

  def isyNotDominatex(y: Point2d, x: Point2d): Boolean = {
    if (y.x <= x.x & y.y <= x.y) {
      false
    } else {
      true
    }
  }

  def getDominationProbability(tree: RTree[Point2d], ddrRect: Rect2d, objectId: Int): Double = {
    val result = new Array[Point2d](N_POINTS)

    tree.search(ddrRect, result)

    result
        .filter(_.isInstanceOf[Point2d])
        .filter(_.o == objectId)
        .foldLeft(0.0)((acc, e) => acc + e.p)
  }

  def findPDROverlappedObjects(node: Node, rect: Rect2d): Set[Object] = {
    val tree = node.tree
    val PDRBox: Rect2d = rect.getPDR.asInstanceOf[Rect2d]
    val overlappedPoints: Array[Point2d] = new Array[Point2d](N_POINTS)
    tree.search(PDRBox, overlappedPoints)

    val objectIds = overlappedPoints.toList.filter(_.isInstanceOf[Point2d]).map(_.o).toSet
    val objects = objectIds.map(id => {
      val a = node.objects.find(_.id == id)
      a.get
    })

    objects
  }

  def findPDDOverlappedObjects(node: Node, rect: Rect2d, currentId: Int): Set[Object] = {
    val tree = node.tree
    val pddBox: Rect2d = rect.getPDD.asInstanceOf[Rect2d]
    val overlappedPoints: Array[Point2d] = new Array[Point2d](N_POINTS)
    tree.search(pddBox, overlappedPoints)

    val objectIds = overlappedPoints.toList.filter(_.isInstanceOf[Point2d]).map(_.o).toSet - currentId
    val objects = objectIds.map(id => {
      val a = node.objects.find(_.id == id)
      a.get
    })

    objects
  }
}
