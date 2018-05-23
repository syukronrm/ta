package ta

import collection.spatial.RTree
import ta.geometry.Point2d
import ta.grid.{Edge, GridLocation, Node}
import ta.Constants._
import ta.grid.Object
import ta.stream.{ExpiredObject, RawObject}

import scala.collection.parallel.immutable.ParSeq
import scala.util.Random

object Dataset {
  def readNode(): Set[RawNode] ={
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

      RawNode(nodeId, lon, lat)
    }

    val a = nodes.toSet

    var rangeX = (maxX - minX) / N_GRID_CELL
    var rangeY = (maxY - minY) / N_GRID_CELL

    var gridHeight = rangeY / N_GRID_CELL
    var gridWidth = rangeX / N_GRID_CELL

    Constants.GRID_HEIGHT = gridHeight
    Constants.GRID_WIDTH = gridWidth

    Constants.D_EPSILON = ((rangeX + rangeY) / 2) * DISTANCE

    a
  }

  def getGridLocation(x: Double, y: Double): GridLocation = {
    val gx = math.floor(x / GRID_WIDTH).toInt
    val gy = math.floor(y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def readEdge(): Set[RawEdge] = {
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cedge.txt").getLines()
    lines.map { l =>
      val lineArray = l.split(' ')
      val edgeId = lineArray(0).toInt
      val node1 = lineArray(1).toInt
      val node2 = lineArray(2).toInt
      val distance = lineArray(3).toDouble

      RawEdge(edgeId, node1, node2, Some(distance))
    }.toSet
  }

  def generateRandomUncertainData(objectId: Int): List[Point2d] = {
    val baseX = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseY = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val prob: Double = 1.0/N_UNCERTAIN_DATA

    var startX: Int = baseX - RANGE
    var endX: Int = baseX + RANGE
    if (startX < MIN_DATASPACE) startX = MIN_DATASPACE
    if (endX > MAX_DATASPACE) endX = MAX_DATASPACE

    var startY: Int = baseY - RANGE
    var endY: Int = baseY + RANGE
    if (startY < MIN_DATASPACE) startY = MIN_DATASPACE
    if (endY > MAX_DATASPACE) endY = MAX_DATASPACE

    (1 to N_UNCERTAIN_DATA).par.map(_ => {
      val x = startX + Random.nextInt(endX - startX + 1)
      val y = startY + Random.nextInt(endY - startY + 1)

      new Point2d(x, y, prob, objectId)
    }).toList
  }

  val MAX_OBJECT = 1000
  val MIN_DATASPACE = 0
  val MAX_DATASPACE = 10000
  val N_UNCERTAIN_DATA = 10
  val RANGE = 250
  val DISTANCE = 0.5
  val TIME_EXPIRATION = 500
  val MIN_EDGE_ID = 0
  val MAX_EDGE_ID = 21692

  def generateObjects() = {
    var objectId = 0
    var expiredObjectId = 0

    val a = (1 to MAX_OBJECT*2).map(_ => {
      val edgeId = Math.floor(Math.random() * (MAX_EDGE_ID - MIN_EDGE_ID) + MIN_EDGE_ID).toInt
      val position = Math.random

      if (objectId - expiredObjectId >= TIME_EXPIRATION || objectId >= MAX_OBJECT) {
        expiredObjectId += 1
        ExpiredObject(expiredObjectId)
      } else {
        objectId += 1
        RawObject(objectId, edgeId, position, generateRandomUncertainData(objectId))
      }
    })

    a
  }
}

