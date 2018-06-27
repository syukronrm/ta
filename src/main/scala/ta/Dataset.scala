package ta

import java.util.Scanner

import collection.spatial.RTree
import ta.geometry.{Point2d, Point3d, Point5d}
import ta.grid.{Edge, GridLocation, Node}
import ta.Constants._
import ta.grid.Object
import ta.stream.{ExpiredObject, RawObject, Stream}

import scala.collection.parallel.immutable.ParSeq
import scala.util.Random

object Dataset {
  var nodes: List[RawNode] = List()
  var edges: List[RawEdge] = List()

  def addNode(rawNode: RawNode): Unit = {
    synchronized {
      this.nodes = this.nodes :+ rawNode
    }
  }

  def addEdge(rawEdge: RawEdge): Unit = {
    synchronized {
      this.edges = this.edges :+ rawEdge
    }
  }

  def findNode(nodeId: Int): Option[RawNode] = {
    this.nodes.find(_.id == nodeId)
  }

  def readNode(): List[RawNode] ={
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cnode.txt").getLines()

    var minX = 20000.0
    var minY = 20000.0
    var maxX = -20000.0
    var maxY = -20000.0

    val nodes = lines.map { l =>
      val lineArray = l.split(' ')
      val nodeId = lineArray(0).toInt
      val lon = lineArray(1).toDouble
      val lat = lineArray(2).toDouble

      if (minX > lon) {
        minX = lon
      } else if (maxX < lon) {
        maxX = lon
      }

      if (minY > lat) {
        minY = lat
      } else if (maxY < lat) {
        maxY = lat
      }

      this.addNode(RawNode(nodeId, lon, lat))
      RawNode(nodeId, lon, lat)
    }

    val a = nodes.toList

    var rangeX = (maxX - minX) / N_GRID_CELL
    var rangeY = (maxY - minY) / N_GRID_CELL

    var gridHeight = rangeY / N_GRID_CELL
    var gridWidth = rangeX / N_GRID_CELL

    Constants.GRID_HEIGHT = gridHeight
    Constants.GRID_WIDTH = gridWidth

    Constants.D_EPSILON = (maxX - minX) * (PERCENT_DISTANCE / 100.0)

    a
  }

  def readNodePartial(): List[RawNode] = {
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cnode.txt").getLines()

    lines.foreach { l =>
      val lineArray = l.split(' ')
      val nodeId = lineArray(0).toInt
      val lon = lineArray(1).toDouble
      val lat = lineArray(2).toDouble

      if (lon > -120 & lat < 37) {
        this.addNode(RawNode(nodeId, lon, lat))
      }
    }

    //println(this.nodes)
    val a = nodes

    var rangeX = (-114 - (-120)).toDouble / N_GRID_CELL
    var rangeY = (37 - 32).toDouble / N_GRID_CELL

    println("rangeX " + rangeX + "rangeY " + rangeY)

    var gridHeight = rangeY / N_GRID_CELL
    var gridWidth = rangeX / N_GRID_CELL

    Constants.GRID_HEIGHT = gridHeight
    Constants.GRID_WIDTH = gridWidth

    Constants.D_EPSILON = 6 * (PERCENT_DISTANCE / 100.0)

    a
  }

  def getGridLocation(x: Double, y: Double): GridLocation = {
    val gx = math.floor(x / GRID_WIDTH).toInt
    val gy = math.floor(y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def readEdge(): List[RawEdge] = {
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cedge.txt").getLines()

    lines.foreach { l =>
      val lineArray = l.split(' ')
      val edgeId = lineArray(0).toInt
      val node1 = lineArray(1).toInt
      val node2 = lineArray(2).toInt
      val distance = lineArray(3).toDouble

      addEdge(RawEdge(edgeId, node1, node2, Some(distance)))
    }

    this.edges
  }

  def readEdgePartial(): List[RawEdge] = {
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cedge.txt").getLines().toSet

    lines.par.foreach { l =>
      val lineArray = l.split(' ')
      val edgeId = lineArray(0).toInt
      val node1 = lineArray(1).toInt
      val node2 = lineArray(2).toInt
      val distance = lineArray(3).toDouble

      val node1Maybe = findNode(node1)
      val node2Maybe = findNode(node2)

      if (node1Maybe.isDefined & node2Maybe.isDefined) {
        addEdge(RawEdge(edgeId, node1, node2, Some(distance)))
      }
    }

    this.edges
  }

  def generateObjects(): List[Stream] = {
    var objectId = 0
    var expiredObjectId = 0
    val edgesSize = this.edges.size

    val a = (1 to (N_OBJECTS + (N_STREAM * 2))).map(_ => {
      val edgeIndex = Math.floor(Math.random() * (edgesSize - 1)).toInt
      val edgeMaybe = edges.lift(edgeIndex)

      if (edgeMaybe.isEmpty) {
        //println(this.edges)
        println("ERROR " + edgeIndex)
      }

      val edgeId = edgeMaybe.get.id
      val position = Math.random

      if (objectId - expiredObjectId >= TIME_EXPIRATION) {
        expiredObjectId += 1
        //println("generate expire object Id " + expiredObjectId)
        ExpiredObject(expiredObjectId)
      } else {
        objectId += 1
        //println("generate stream object Id " + objectId)
        RawObject(objectId, edgeId, position, generateUncertainData(objectId))
      }
    }).toList

    a
  }

  def generateUncertainData(objectId: Int) = {
    val List(baseX, baseY, baseZ, baseA, baseB) = independenceUncertainData()

    val prob: Double = 1.0/N_POINTS

    var startX: Int = baseX - RANGE
    var endX: Int = baseX + RANGE
    if (startX < MIN_DATASPACE) startX = MIN_DATASPACE
    if (endX > MAX_DATASPACE) endX = MAX_DATASPACE

    var startY: Int = baseY - RANGE
    var endY: Int = baseY + RANGE
    if (startY < MIN_DATASPACE) startY = MIN_DATASPACE
    if (endY > MAX_DATASPACE) endY = MAX_DATASPACE

    var startZ: Int = baseZ - RANGE
    var endZ: Int = baseZ + RANGE
    if (startZ < MIN_DATASPACE) startZ = MIN_DATASPACE
    if (endZ > MAX_DATASPACE) endZ = MAX_DATASPACE

    var startA: Int = baseA - RANGE
    var endA: Int = baseA + RANGE
    if (startA < MIN_DATASPACE) startA = MIN_DATASPACE
    if (endA > MAX_DATASPACE) endA = MAX_DATASPACE

    var startB: Int = baseB - RANGE
    var endB: Int = baseB + RANGE
    if (startB < MIN_DATASPACE) startB = MIN_DATASPACE
    if (endB > MAX_DATASPACE) endB = MAX_DATASPACE

    (1 to N_POINTS).par.map(_ => {
      val x = startX + Random.nextInt(endX - startX + 1)
      val y = startY + Random.nextInt(endY - startY + 1)
      val z = startZ + Random.nextInt(endZ - startZ + 1)
      val a = startA + Random.nextInt(endA - startA + 1)
      val b = startB + Random.nextInt(endB - startB + 1)

      new Point5d(x, y, z, a, b, prob, objectId)
    }).toList
  }

  def anticorrelatedUncertainData() = {
    val baseX = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseY = (MAX_DATASPACE - MIN_DATASPACE) - baseX

    List(baseX, baseY)
  }

  def correlatedUncertainData() = {
    val baseX = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseY = baseX

    List(baseX, baseY)
  }

  def independenceUncertainData() = {
    val baseX = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseY = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseZ = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseA = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseB = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt

    List(baseX, baseY, baseZ, baseA, baseB)
  }
}

