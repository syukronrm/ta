package ta

import collection.geometry._
import collection.spatial.{HyperPoint, RTree, RectBuilder}
import ta.grid.Node
import ta.grid.Edge
import ta.grid.Grid

import scala.collection.immutable.Set

case class RawNode(id: Int, x: Double, y: Double)
case class RawEdge(id: Int, i: Int, j: Int, lengthMaybe: Option[Double])

object TAQ {
  def main(args: Array[String]): Unit = {
    val table_nodes = Set(
      RawNode(1, 2, 1),
      RawNode(2, 19, 1),
      RawNode(3, 3, 3),
      RawNode(4, 9, 5),
      RawNode(5, 16, 5),
      RawNode(6, 3, 8),
      RawNode(7, 8, 12),
      RawNode(8, 16, 12)
    )

    val table_edges = Set(
      RawEdge(1, 1, 2, None),
      RawEdge(2, 1, 3, None),
      RawEdge(3, 2, 5, None),
      RawEdge(4, 3, 4, None),
      RawEdge(5, 3, 6, None),
      RawEdge(6, 4, 5, None),
      RawEdge(7, 4, 7, None),
      RawEdge(8, 5, 8, None),
      RawEdge(9, 6, 7, None),
      RawEdge(10, 7, 8, None),
      RawEdge(11, 4, 6, None)
    )

    val grid = new Grid
    grid.addRawNodes(table_nodes)
    grid.addRawEdges(table_edges)

    println("Main!")
  }
}
