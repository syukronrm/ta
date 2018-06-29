package ta.algorithm

import collection.spatial.{HyperPoint, HyperRect, RTree}
import scalax.collection.Graph
import scalax.collection.edge.WLkUnDiEdge
import ta.graph.TempGraph
import ta.grid._
import ta.stream.{ExpiredObject, RawObject, Stream}
import ta.Constants._
import ta.geometry.{Point4d, Rect4d}
import ta.grid.Rect._
import ta.algorithm.TurningPoint._

import scala.collection.JavaConverters._
import scala.collection.immutable.Set
import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

case class NodeQueue(nodeId: Int, distance: Double)

object TheAlgorithm {
  var jumlahPDROverlapped: Set[Int] = Set()
  var skyComputeSum: Int = 0

  def TheAlgorithm(grid: Grid, stream: Stream): Grid = {
    jumlahPDROverlapped = Set()
    skyComputeSum = 0
    //println("====================================WOW===============================")
    var Q: scala.collection.mutable.Queue[NodeQueue] = scala.collection.mutable.Queue[NodeQueue]()
    val tempGraph = new TempGraph

    //println(GRID_WIDTH)
    //println(GRID_HEIGHT)

    //var updatedNodes: Set[Int] = Set()
    var visitedNodes: Set[Int] = Set()
    var updatedNodes: Set[Int] = Set()

    val rawObject = stream match {
      case _rawObject: RawObject =>
       // println("Insertion Object ID " + _rawObject.id)
        grid.addObjectToEdge(_rawObject)
        grid.addRawObject(_rawObject)
        _rawObject
      case ExpiredObject(id) =>
       // println("Deletion Object ID " + id)
        val _rawObject = grid.getRawObject(id).get
        _rawObject
    }

    val objectList: java.util.List[Point4d] = rawObject.points.toList.asJava
    val rect = new Rect4d(objectList)

    var addedGrid: Set[GridLocation] = Set()

    val edge = grid.getEdge(rawObject.edgeId).get
    val nodei = grid.getNode(edge.i).get
    val nodej = grid.getNode(edge.j).get


    val gridNodeI = grid.getGridLocation(nodei)
    val gridNodeJ = grid.getGridLocation(nodej)

    addedGrid += gridNodeI
    //println("  importing data grid node " + nodei.id)
    val EdgesNodes(edgesNodeI, nodesNodeI) = grid.getDataGrid(gridNodeI)
    //println("add Edges " + gridNodeI.toString)
    tempGraph.addEdges(nodesNodeI, edgesNodeI)
    addedGrid += gridNodeI
    //println("    nodes " + nodesNodeI.size)
    //println("    edges " + edgesNodeI.size)

    //println("  importing data grid node " + nodej.id)
    if (!addedGrid.contains(gridNodeJ)) {
      val EdgesNodes(edgesNodeJ, nodesNodeJ) = grid.getDataGrid(gridNodeJ)
      //println("add Edges " + gridNodeJ.toString)
      tempGraph.addEdges(nodesNodeJ, edgesNodeJ)
      addedGrid += gridNodeJ
    }

    //println("    nodes " + nodesNodeJ.size)
    //println("    edges " + edgesNodeJ.size)

    //println("  add grid (" + gridNodeI.x + ", " + gridNodeI.y +")")
    //println("  add grid (" + gridNodeJ.x + ", " + gridNodeJ.y +")")

    val e = tempGraph.getEdge(rawObject.edgeId)
    var distanceNodeI = e.length * rawObject.position
    var distanceNodeJ = e.length * (1 - rawObject.position)

    Q.enqueue(NodeQueue(nodei.id, distanceNodeI))
    visitedNodes = visitedNodes + nodei.id
    Q.enqueue(NodeQueue(nodej.id, distanceNodeJ))
    visitedNodes = visitedNodes + nodej.id

    while (Q.nonEmpty) {
      Q = Q.sortBy(_.distance)
      val NodeQueue(currentNodeId, distance) = Q.dequeue()
      //println("  dequeue node " + currentNodeId + " with distance " + distance)

      if (distance < D_EPSILON) {
        val currentNode = grid.getNode(currentNodeId).get
//        val currentNode = tempGraph.getNode(currentNodeId).get

        val gridLocation = grid.getGridLocation(currentNode)

        if (!addedGrid.contains(gridLocation)) {
          //println("add Edges " + gridLocation.toString)
          val EdgesNodes(edgesN, nodesN) = grid.getDataGrid(gridLocation)
          tempGraph.addEdges(nodesN, edgesN)
          addedGrid += gridLocation
          //println("        add grid (" + gridLocation.x + ", " + gridLocation.y +")")
        }

        val updatedNode = stream match {
          case _: RawObject =>
            //val distance = tempGraph.calculateDistance(rawObject, currentNodeId)
            //println("    distance node " + currentNodeId + ": " + distance)
            //println("    insert object " + rawObject.id + " to node " + currentNodeId)
            insertToNode(grid, currentNode, rawObject, distance, rect)
          case ExpiredObject(objectId) =>
            //println("    delete object " + rawObject + " from node " + currentNodeId)
            deleteFromNode(grid, currentNode, objectId, rect)
        }

        tempGraph.updateNode(updatedNode)
        grid.updateNode(updatedNode)
        updatedNodes += updatedNode.id

        //println("    node neighbor ")
        val neighborNodesEdges = tempGraph.getNeighborNodesEdges(currentNodeId)
        //println("neighbors " + neighborNodesEdges.toString())

        neighborNodesEdges.keys.foreach { nodeId =>
          if (!visitedNodes.contains(nodeId)) {
            val distanceUnvisitedNode = distance + neighborNodesEdges(nodeId)
            Q.enqueue(NodeQueue(nodeId, distanceUnvisitedNode))
            visitedNodes += nodeId
          }
        }
      }
    }

//    if (rawObject.id > 5000 & rawObject.id < 5100) {
    if (false) {
      println(grid.nodes.size)
      val isImpossibles = grid.nodes.map(n => n.objects.filter(_.isImpossible == true).size)
      println(isImpossibles.size)

      stream match {
        case _: RawObject =>
          println("Insertion " + stream.getId + " " + isImpossibles.toString())
        case _: ExpiredObject =>
          println("Deletion " + stream.getId + " " + isImpossibles.toString())
      }

      println("  rata-rata " + isImpossibles.sum.toDouble / isImpossibles.size)
    }

//    computeTurningPoint(tempGraph.graph)
    if (ENV == "TESTING") {
      computeTurningPoint(grid, tempGraph.edgesGraph, tempGraph.nodesGraph, updatedNodes)
    }

    if (stream.isInstanceOf[ExpiredObject]) {
      grid.removeObjectFromEdge(stream.getId)
      grid.removeRawObject(stream.getId)
    }

    val rata2PDROverlap = jumlahPDROverlapped.sum.toDouble / jumlahPDROverlapped.size
//    println("PDR overlap " + rata2PDROverlap + "\t" + jumlahPDROverlapped.toString())

    grid
//    updateGrid(grid, tempGraph.graph)
  }

  def updateGrid(grid: Grid, graph: Graph[Node, WLkUnDiEdge]): Grid = {
    grid.updateNodes(graph.nodes.toOuter)

    grid
  }

  def computeTurningPoint(grid: Grid, edges: mutable.Map[Int, Edge], nodes: mutable.Map[Int, Node], updatedNodes: Set[Int]): Unit = {
    edges.values
      .filter(e => updatedNodes.contains(e.i) | updatedNodes.contains(e.j))
      .foreach { e =>
        val nodeS = nodes(e.i)
        val nodeE = nodes(e.j)

        processLandmark(grid, nodeS, e, nodeE)
      }
  }

//  def computeTurningPoint(graph: Graph[Node, WLkUnDiEdge]): Unit = {
//    graph.edges.foreach(e => {
//      val nodeSId = e.label.asInstanceOf[Edge].i
//
//      val List(nodeS, nodeE) = if (e._1.toOuter.id == nodeSId) {
//        List(e._1.toOuter, e._2.toOuter)
//      } else {
//        List(e._2.toOuter, e._1.toOuter)
//      }
//
//      val edge = e.label.asInstanceOf[Edge]
//
//      //println("\n")
//      //println("==========")
//      //println("Process Turning Point edge " + edge.id + " " + nodeS.id + "~" + nodeE.id + " " + edge.length)
//      processLandmark(nodeS, edge, nodeE)
//    })
//  }

  def removePoints(tree: RTree[Point4d], points: List[Point4d]): RTree[Point4d] = {
    val newTree = new RTree(new Point4d.Builder(), 2, 8, RTree.Split.AXIAL)
    tree.forEach(p => {
      if (!points.contains(p))
        newTree.add(p)
    })

    newTree
  }

  def deleteFromNode(grid: Grid, currentNode: Node, objectId: Int, rect: Rect4d): Node = {
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
      case q@Object(a, b, _, d, e, f, g, h) =>
        val skyProb = SkyPrX(currentNode.tree, q.id)
        //println("      SkyProb object " + q.id + " = " + skyProb)
        Object(a, b, skyProb, d, e, f, g, h)
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
                   rect: Rect4d): Node = {

    var overlappedObjects = findPDROverlappedObjects(node, rect)
    //println("    overlapped objects:")
    jumlahPDROverlapped += overlappedObjects.size

    rawObject.points.foreach(p => node.tree.add(p))

    val updatedOverlappedObjects = overlappedObjects.map(q => {
      val ddrRect = rect.getDDR.asInstanceOf[Rect4d]
      val qRect = q.rect

      if (ddrRect.contains(qRect) & distance < q.distance) {
        //println("      mark " + q.id + " as impossible")
        // Object is impossible
//        println("1: isImpossible " + q.id)
        Object(q.id, q.edgeId, q.skyProb, isImpossible = true, node.id, q.rect, q.distance, q.position)
      } else {
//        rawObject.points.foreach(p => node.tree.add(p))
        val objProb = getDominationProbability(node.tree, ddrRect, q.id)
//        rawObject.points.foreach(p => node.tree.remove(p))
        //println("      probability object " + q.id + " dominate object "+ rawObject.id +" is " + objProb)
        if (objProb > (1 - P_THRESHOLD) & distance < q.distance) {
          //println("        mark " + q.id + " as impossible")
          // Object is impossible
//          println("2: isImpossible " + q.id)
          Object(q.id, q.edgeId, q.skyProb, isImpossible = true, node.id, q.rect, q.distance, q.position)
        } else {
//          rawObject.points.foreach(p => node.tree.add(p))
          val skyProb = if (ENV == "GENERATE") {
            0.0
          } else {
            skyComputeSum += 1
            SkyPrX(node.tree, q.id)
          }
//          rawObject.points.foreach(p => node.tree.remove(p))
          //println("      SkyProb object " + q.id + " = " + skyProb)
          Object(q.id, q.edgeId, skyProb, q.isImpossible, node.id, q.rect, q.distance, q.position)
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

    val skyProbU = if (ENV == "GENERATE") {
      0.0
    } else {
      val PDDoverlappedObjects = findPDDOverlappedObjects(node, rect, rawObject.id)
      val pddTree = new RTree(new Point4d.Builder, 2, 8, RTree.Split.AXIAL)
      PDDoverlappedObjects.foreach { o =>
        val points = grid.getRawObject(o.id).get.points
        points.foreach { p =>
          pddTree.add(p)
        }
      }
      skyComputeSum += 1
      SkyPrX(pddTree, rawObject.id)
    }

    //println("    SkyProb incoming object " + rawObject.id + " = " + skyProbU)
    val finalObjects = updatedObjects +
        Object(rawObject.id, rawObject.edgeId, skyProbU, isImpossible = false, node.id, rect, distance, rawObject.position)
    Node(node.id, node.x, node.y, node.tree, finalObjects)
  }

  def SkyPrX(tree: RTree[Point4d], objectId: Int): Double = {
    val X = scala.collection.mutable.Set[Point4d]()
    tree.forEach(p => {
      if (p.o == objectId) {
        X.add(p)
      }
    })

    X.map(x => {
      x.p * SkyPrx(tree, X, x)
    })
      .sum
  }

  def SkyPrx(tree: RTree[Point4d], X: mutable.Set[Point4d], x: Point4d): Double = {
    val entries = mutable.Set[Point4d]()
    tree.forEach(p => entries.add(p))

    entries
      .toList
      .filterNot(e => X.contains(e))
      .groupBy(_.o) // filtered Y Map
      .values
      .map(Y => PrYnotdominatex(Y, x))
      .product
  }

  def PrYnotdominatex(Y: List[Point4d], x: Point4d): Double = {
    Y.filter(y => isyNotDominatex(y, x))
      .foldLeft(0.0)((acc, e) => acc + e.p)
  }

  def isyNotDominatex(y: Point4d, x: Point4d): Boolean = {
    if (y.x <= x.x & y.y <= x.y & y.z <= x.z & y.a <= x.a) {
      false
    } else {
      true
    }

//    val Y = new Rect4d(y)
//    val X = new Rect4d(x)
//    if (Y.getDDR.contains(X))
//      false
//    else
//      true
  }

  def getDominationProbability(tree: RTree[Point4d], ddrRect: Rect4d, objectId: Int): Double = {
    val result = new Array[Point4d](N_POINTS)

    tree.search(ddrRect, result)

    result
        .filter(_.isInstanceOf[Point4d])
        .filter(_.o == objectId)
        .foldLeft(0.0)((acc, e) => acc + e.p)
  }

  def findPDROverlappedObjects(node: Node, rect: Rect4d): Set[Object] = {
    val tree = node.tree
    val PDRBox: Rect4d = rect.getPDR.asInstanceOf[Rect4d]
    val overlappedPoints: Array[Point4d] = new Array[Point4d](N_POINTS)
    tree.search(PDRBox, overlappedPoints)

    val objectIds = overlappedPoints.toList.filter(_.isInstanceOf[Point4d]).map(_.o).toSet
    //println("    rect " + rect.toString)
    //println("    PDRBox " + PDRBox.toString + " objects IDs = " + objectIds.toString)
//    println("PDR objects " + objectIds.toString())
    val objects = objectIds.map(id => {
      val a = node.objects.find(_.id == id)
//      if (a.isEmpty) {
//        println(id)
//      }
      a.get
    })

    objects.toSet
  }

  def findPDDOverlappedObjects(node: Node, rect: Rect4d, currentId: Int): Set[Object] = {
    val tree = node.tree
    val pddBox: Rect4d = rect.getPDD.asInstanceOf[Rect4d]
    val overlappedPoints: Array[Point4d] = new Array[Point4d](N_POINTS)
    tree.search(pddBox, overlappedPoints)

    val objectIds = overlappedPoints.toList.filter(_.isInstanceOf[Point4d]).map(_.o).toSet - currentId
    //println("    rect " + rect.toString)
    //println("    PDRBox " + PDRBox.toString + " objects IDs = " + objectIds.toString)
//    println("PDD objects " + objectIds.toString())
    val objects = objectIds.map(id => {
      val a = node.objects.find(_.id == id)
//            if (a.isEmpty) {
//      println(id)
//            }
      a.get
    })

    objects.toSet
  }
}
