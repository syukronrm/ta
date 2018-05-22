package ta

import collection.spatial.RTree
import ta.geometry.Point2d
import ta.grid.{Edge, GridLocation, Node}
import ta.Constants._

object Import {
  private var minX = Double.MaxValue
  private var minY = Double.MaxValue
  private var maxX = Double.MinValue
  private var maxY = Double.MinValue

  def readNode() ={
    val lines = io.Source.fromFile("src/main/scala/dataset/california/cal.cnode.txt").getLines()

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

    val rangeX = (maxX - minX) / N_GRID_CELL
    val rangeY = (maxY - minY) / N_GRID_CELL

    Constants.GRID_HEIGHT = rangeY
    Constants.GRID_WIDTH = rangeX

    nodes.toSet
  }

  def getGridLocation(x: Double, y: Double): GridLocation = {
    val gx = math.floor(x / GRID_WIDTH).toInt
    val gy = math.floor(y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def readEdge() = {
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
}
