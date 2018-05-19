package ta

import collection.geometry._
import collection.spatial.{HyperPoint, RTree, RectBuilder}
import ta.grid.Node
import ta.grid.Edge
import ta.grid.Grid

import scala.collection.immutable.Set

case class RawEdge(id: Int, i: Int, j: Int)
case class RawNode(id: Int, x: Double, y: Double)

object TAQ {
  def createNewTree2d[T: RectBuilder[T]](): RTree[T] = {
    val a = implicitly[RectBuilder[T]]
    new RTree[T](new a.Builder, 2, 8, RTree.Split.AXIAL)
  }

  def createNewTree3d(): RTree[Point3d] = {
    new RTree[Point3d](new Point3d.Builder, 2, 8, RTree.Split.AXIAL)
  }

  def main(args: Array[String]): Unit = {
    val table_nodes_2d = Set(
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
      RawEdge(1, 1, 2),
      RawEdge(2, 1, 3),
      RawEdge(3, 2, 5),
      RawEdge(4, 3, 4),
      RawEdge(5, 3, 6),
      RawEdge(6, 4, 5),
      RawEdge(7, 4, 7),
      RawEdge(8, 5, 8),
      RawEdge(9, 6, 7),
      RawEdge(10, 7, 8),
      RawEdge(11, 4, 6)
    )

    val grid = new Grid[Point2d]

    println("Main!")
  }
}
