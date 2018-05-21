package ta.algorithm

import collection.spatial.{HyperPoint, HyperRect, RTree}
import scalax.collection.Graph
import scalax.collection.edge.WLkUnDiEdge
import ta.graph.TempGraph
import ta.grid._
import ta.stream.{ExpiredObject, RawObject, Stream}
import ta.Constants._
import ta.geometry.{Point2d, Rect2d, Rect3d}
import ta.grid.Rect._

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.collection.mutable

case class NodeQueue(nodeId: Int, distance: Double)

object TheAlgorithm {
  def TheAlgorithm(grid: Grid, stream: Stream): Grid = {
    println("====================================WOW===============================")
    var Q: scala.collection.mutable.Queue[NodeQueue] = scala.collection.mutable.Queue[NodeQueue]()
    val tempGraph = new TempGraph

    var updatedNodes: Set[Int] = Set()
    var visitedNodes: Set[Int] = Set()

    val rawObject = stream match {
      case _rawObject: RawObject =>
        println("Insertion Object ID " + _rawObject.id)
        grid.addObjectToEdge(_rawObject)
        grid.addRawObject(_rawObject)
        _rawObject
      case ExpiredObject(id) =>
        println("Deletion Object ID " + id)
        val _rawObject = grid.getRawObject(id).get
        grid.removeObjectFromEdge(id)
        _rawObject
    }

    val objectList: java.util.List[Point2d] = rawObject.points.toList.asJava
    val rect = new Rect2d(objectList)

    val edge = grid.getEdge(rawObject.edgeId).get
    val nodei = grid.getNode(edge.i).get
    val nodej = grid.getNode(edge.j).get

    val gridNodeI = grid.getGridLocation(nodei)
    val gridNodeJ = grid.getGridLocation(nodej)
    val EdgesNodes(edgesNodeI, nodesNodeI) = grid.getDataGrid(gridNodeI)
    val EdgesNodes(edgesNodeJ, nodesNodeJ) = grid.getDataGrid(gridNodeJ)

    tempGraph.addEdges(nodesNodeI, edgesNodeI)
    println("  add grid (" + gridNodeI.x + ", " + gridNodeI.y +")")
    tempGraph.addEdges(nodesNodeI, edgesNodeI)
    println("  add grid (" + gridNodeJ.x + ", " + gridNodeJ.y +")")

    val distanceNodeI = tempGraph.calculateDistance(rawObject, nodei.id)
    println("  distance node " + nodei.id + ": " + distanceNodeI)
    val distanceNodeJ = tempGraph.calculateDistance(rawObject, nodej.id)
    println("  distance node " + nodej.id + ": " + distanceNodeJ)

    if (distanceNodeI < distanceNodeJ) {
      println("  enqueue node " + nodei.id)
      Q.enqueue(NodeQueue(nodei.id, distanceNodeI))
      visitedNodes = visitedNodes + nodei.id
    } else {
      println("  enqueue node " + nodej.id)
      Q.enqueue(NodeQueue(nodej.id, distanceNodeJ))
      visitedNodes = visitedNodes + nodej.id
    }

    while (Q.nonEmpty) {
      Q = Q.sortBy(_.distance)
      val NodeQueue(currentNodeId, distance) = Q.dequeue()
      println("  dequeue node " + currentNodeId + " with distance " + distance)

      if (distance < D_EPSILON) {
        val currentNode = tempGraph.getNode(currentNodeId).get
        val updatedNode = stream match {
          case _: RawObject =>
            val distance = tempGraph.calculateDistance(rawObject, currentNodeId)
            println("    distance node " + currentNodeId + ": " + distance)
            println("    insert object " + rawObject.id + " to node " + currentNodeId)
            insertToNode(currentNode, rawObject, distance, rect)
          case ExpiredObject(objectId) =>
            println("    delete object " + rawObject + " from node " + currentNodeId)
            deleteFromNode(currentNode, objectId, rect)
        }

        tempGraph.updateNode(updatedNode)
        grid.updateNode(updatedNode)

        updatedNodes += updatedNode.id

        println("    node neighbor ")
        val neighborNodes = tempGraph.getNeighborNodes(currentNodeId)

        neighborNodes.foreach(n => {
          println("      node " + n.id)
          if (!visitedNodes.contains(n.id)) {
            val gridLocation = grid.getGridLocation(n)
            val EdgesNodes(edgesN, nodesN) = grid.getDataGrid(gridLocation)
            tempGraph.addEdges(nodesN, edgesN)
            println("        add grid (" + gridLocation.x + ", " + gridLocation.y +")")

            val distanceUnvisitedNode = tempGraph.calculateDistance(rawObject, n.id)
            println("        distance node " + n.id + ": " + distanceUnvisitedNode)
            Q.enqueue(NodeQueue(n.id, distanceUnvisitedNode))

            visitedNodes += n.id
            println("        visit node " + n.id)
          }
        })
      }
    }

    grid
  }

  def removePoints(tree: RTree[Point2d], points: List[Point2d]): RTree[Point2d] = {
    val newTree = new RTree(new Point2d.Builder(), 2, 8, RTree.Split.AXIAL)
    tree.forEach(p => {
      if (!points.contains(p))
        newTree.add(p)
    })

    newTree
  }

  def deleteFromNode(currentNode: Node, objectId: Int, rect: Rect2d): Node = {
    val objectMaybe = currentNode.objects.find(_.id == objectId)

    if (objectMaybe.isEmpty) {
      return currentNode
    }

    val toBeDeletedPoints = objectMaybe.get.points

//    currentNode.tree = removePoints(currentNode.tree, toBeDeletedPoints)
    for (point <- toBeDeletedPoints) {
      currentNode.tree.remove(point)
    }

    var overlappedObjects = findPDROverlappedObjects(currentNode, rect)

    overlappedObjects = overlappedObjects.map {
      case q@Object(_, _, _, true, _, _, _) => q
      case q@Object(a, b, _, d, e, f, g) =>
        val skyProb = SkyPrX(currentNode.tree, q.id)
        println("      SkyProb object " + q.id + " = " + skyProb)
        Object(a, b, skyProb, d, e, f, g)
    }

    overlappedObjects = overlappedObjects.filterNot(_.id == objectId)

    Node(currentNode.id, currentNode.x, currentNode.y, currentNode.tree, overlappedObjects)
  }

  def insertToNode(node: Node,
                   rawObject: RawObject,
                   distance: Double,
                   rect: Rect2d): Node = {

//    val clonedTree = node.tree.clone().asInstanceOf[RTree[Point2d]]

    val overlappedObjects = findPDROverlappedObjects(node, rect)
    println("    overlapped objects:")
    val updatedOverlappedObjects = overlappedObjects.map(q => {
      val ddrRect = rect.getDDR.asInstanceOf[Rect2d]
      val qRect = createRect(q.points)

      if (ddrRect.contains(qRect)) {
        println("      mark " + q.id + " as impossible")
        // Object is impossible
        Object(q.id, q.edgeId, q.skyProb, isImpossible = true, node.id, q.points, q.distance)
      } else {
        rawObject.points.foreach(p => node.tree.add(p))
        val objProb = getDominationProbability(node.tree, ddrRect, q.id)
        rawObject.points.foreach(p => node.tree.remove(p))
        println("      probability object " + q.id + " dominate object "+ rawObject.id +" is " + objProb)
        if (objProb > (1 - P_THRESHOLD)) {
          println("        mark " + q.id + " as impossible")
          // Object is impossible
          Object(q.id, q.edgeId, q.skyProb, isImpossible = true, node.id, q.points, q.distance)
        } else {
          rawObject.points.foreach(p => node.tree.add(p))
          val skyProb = SkyPrX(node.tree, q.id)
          rawObject.points.foreach(p => node.tree.remove(p))
          println("      SkyProb object " + q.id + " = " + skyProb)
          Object(q.id, q.edgeId, skyProb, q.isImpossible, node.id, q.points, q.distance)
        }
      }
    })

    rawObject.points.foreach(p => node.tree.add(p))

    val updatedObjects = node.objects.map(o => {
      val updated = updatedOverlappedObjects.find(_.id == o.id)

      if (updated.nonEmpty)
        updated.get
      else
        o
    })

    val skyProbU = SkyPrX(node.tree, rawObject.id)
    println("    SkyProb incoming object " + rawObject.id + " = " + skyProbU)
    val finalObjects = updatedObjects +
        Object(rawObject.id, rawObject.edgeId, skyProbU, isImpossible = false, node.id, rawObject.points, distance)
    Node(node.id, node.x, node.y, node.tree, finalObjects)
  }

  def SkyPrX(tree: RTree[Point2d], objectId: Int): Double = {
    val X = scala.collection.mutable.Set[Point2d]()
    tree.forEach(p => {
      if (p.o == objectId) {
        X.add(p)
      }
    })

    X.map(x => {
      val skyPrx = SkyPrx(tree, X, x)
      x.p * skyPrx
    })
      .map(a => {
        a
      })
      .sum
  }

  def SkyPrx(tree: RTree[Point2d], X: mutable.Set[Point2d], x: Point2d): Double = {
    val entries = mutable.Set[Point2d]()
    tree.forEach(p => entries.add(p))

    entries
      .toList
      .filterNot(e => X.contains(e))
      .groupBy(_.o) // filtered Y Map
      .values
      .map(Y => PrYnotdominatex(Y, x))
      .product
  }

  def PrYnotdominatex(Y: List[Point2d], x: Point2d): Double = {
    Y.filter(y => isyNotDominatex(y, x))
      .foldLeft(0.0)((acc, e) => acc + e.p)
  }

  def isyNotDominatex(y: Point2d, x: Point2d): Boolean = {
    val Y = new Rect2d(y)
    val X = new Rect2d(x)

    if (Y.getDDR.contains(X))
      false
    else
      true
  }

  def getDominationProbability(tree: RTree[Point2d], ddrRect: Rect2d, objectId: Int): Double = {
    val result = new Array[Point2d](MAX_POINT)

    tree.search(ddrRect, result)

    result
        .filter(_.isInstanceOf[Point2d])
        .filter(_.o == objectId)
        .foldLeft(0.0)((acc, e) => acc + e.p)
  }

  def findPDROverlappedObjects(node: Node, rect: Rect2d): Set[Object] = {
    val tree = node.tree
    val PDRBox: Rect2d = rect.getPDR.asInstanceOf[Rect2d]
    val overlappedPoints: Array[Point2d] = new Array[Point2d](MAX_POINT)
    tree.search(PDRBox, overlappedPoints)

    val objectIds = overlappedPoints.toList.filter(_.isInstanceOf[Point2d]).map(_.o).toSet
    println("    rect " + rect.toString)
    println("    PDRBox " + PDRBox.toString + " objects IDs = " + objectIds.toString)
    val objects = objectIds.map(id => {
      val a = node.objects.find(_.id == id)
      if (a.isEmpty) {
        a
      }
      a.get
    })

    objects.toSet
  }

//  def insertToNode(node: NodeGrid, obj: UncertainObject, distance: Double): NodeGrid = {
//    val incomingEntries = createEntryTuples(obj)
//
//    val pdrOverlappedObjects = findPdrOverlappedObjects(node, obj)
//
//    val updatedOverlappedObjects = pdrOverlappedObjects.map(q => {
//      val ddrObj = expandDdr(obj.bbox)
//      val bboxQ = q.obj.bbox
//      if (ddrObj.contains(bboxQ)) {
//        NodeObject(q.obj, q.skyProb, isImpossible = true, q.distance)
//      } else {
//        val bbox = expandDdr(obj.bbox)
//
//        val objProb = getDominationProbability(node.tree, bbox, q.obj.id)
//
//        if (objProb > (1 - P_THRESHOLD)) {
//          NodeObject(q.obj, q.skyProb, isImpossible = true, q.distance)
//        } else {
//          val skyProb = SkyPrX(node.tree.insertAll(incomingEntries), q.obj.id)
//          NodeObject(q.obj, skyProb, q.isImpossible, q.distance)
//        }
//      }
//    })
//
//    // INSERT OBJECT TO TREE
//    val tree = node.tree.insertAll(incomingEntries)
//
//    val updatedObjects = node.objects.map(o => {
//      val updated = updatedOverlappedObjects.find(_.obj.id == o.obj.id)
//
//      if (updated.nonEmpty)
//        updated.get
//      else
//        o
//    })
//
//    val skyProbU = SkyPrX(tree, obj.id)
//    val finalObjects = updatedObjects + NodeObject(obj, skyProbU, isImpossible = false, distance)
//    NodeGrid(node.id, node.x, node.y, tree, finalObjects)
//  }
}
