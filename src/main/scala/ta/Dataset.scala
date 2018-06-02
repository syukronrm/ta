package ta

import collection.spatial.RTree
import ta.geometry.Point2d
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

      RawNode(nodeId, lon, lat)
    }

    val a = nodes.toList

    var rangeX = (maxX - minX) / N_GRID_CELL
    var rangeY = (maxY - minY) / N_GRID_CELL

    var gridHeight = rangeY / N_GRID_CELL
    var gridWidth = rangeX / N_GRID_CELL

    Constants.GRID_HEIGHT = gridHeight
    Constants.GRID_WIDTH = gridWidth

    Constants.D_EPSILON = ((rangeX + rangeY) / 2) * PERCENT_DISTANCE

    a
  }

  def readPartialNode(): List[RawNode] = {
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cnode.txt").getLines()

    lines.foreach { l =>
      val lineArray = l.split(' ')
      val nodeId = lineArray(0).toInt
      val lon = lineArray(1).toDouble
      val lat = lineArray(2).toDouble

      if (lon > -120 & lat > 37) {
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

    Constants.D_EPSILON = ((rangeX + rangeY) / 2) * PERCENT_DISTANCE

    a
  }

  def getGridLocation(x: Double, y: Double): GridLocation = {
    val gx = math.floor(x / GRID_WIDTH).toInt
    val gy = math.floor(y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def readEdge(): List[RawEdge] = {
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cedge.txt").getLines()
    lines.map { l =>
      val lineArray = l.split(' ')
      val edgeId = lineArray(0).toInt
      val node1 = lineArray(1).toInt
      val node2 = lineArray(2).toInt
      val distance = lineArray(3).toDouble

      RawEdge(edgeId, node1, node2, Some(distance))
    }.toList
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

  def generateRandomUncertainData(objectId: Int): List[Point2d] = {
    val baseX = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val baseY = Math.floor(Math.random() * (MAX_DATASPACE - MIN_DATASPACE) + MIN_DATASPACE).toInt
    val prob: Double = 1.0/N_POINTS

    var startX: Int = baseX - RANGE
    var endX: Int = baseX + RANGE
    if (startX < MIN_DATASPACE) startX = MIN_DATASPACE
    if (endX > MAX_DATASPACE) endX = MAX_DATASPACE

    var startY: Int = baseY - RANGE
    var endY: Int = baseY + RANGE
    if (startY < MIN_DATASPACE) startY = MIN_DATASPACE
    if (endY > MAX_DATASPACE) endY = MAX_DATASPACE

    (1 to N_POINTS).par.map(_ => {
      val x = startX + Random.nextInt(endX - startX + 1)
      val y = startY + Random.nextInt(endY - startY + 1)

      new Point2d(x, y, prob, objectId)
    }).toList
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
        ExpiredObject(expiredObjectId)
      } else {
        objectId += 1
        RawObject(objectId, edgeId, position, generateRandomUncertainData(objectId))
      }
    }).toList

    a
  }
}

