package ta

import collection.spatial.{HyperPoint, RTree, RectBuilder}
import ta.grid.Node
import ta.grid.Edge
import ta.grid.Grid
import ta.stream.{ExpiredObject, RawObject}
import ta.geometry.Point2d
import ta.algorithm.TheAlgorithm._

import scala.collection.immutable.Set
import scala.collection.parallel.ParSet

case class RawNode(id: Int, x: Double, y: Double)
case class RawEdge(id: Int, i: Int, j: Int, lengthMaybe: Option[Double])

object TAQ {
  val mb: Int = 1024 * 1024

  def printMemoryUsage(str: String) = {
    val runtime = Runtime.getRuntime
    val used = (runtime.totalMemory - runtime.freeMemory) / mb
    val free = runtime.freeMemory / mb
    val total = runtime.totalMemory / mb
    val max = runtime.maxMemory / mb

//    println("==== " + str + " ====")
    println(used + "\t" + free + "\t" + total + "\t" + max)
  }

  def main(args: Array[String]): Unit = {
    val cal_table_nodes = Dataset.readNode()
    val cal_table_edges = Dataset.readEdge()

//    val table_nodes = ParSet(
//      RawNode(1, 2, 1),
//      RawNode(2, 19, 1),
//      RawNode(3, 3, 3),
//      RawNode(4, 9, 5),
//      RawNode(5, 16, 5),
//      RawNode(6, 3, 8),
//      RawNode(7, 8, 12),
//      RawNode(8, 16, 12)
//    )

//    val table_edges = ParSet(
//      RawEdge(1, 1, 2, None),
//      RawEdge(2, 1, 3, None),
//      RawEdge(3, 2, 5, None),
//      RawEdge(4, 3, 4, None),
//      RawEdge(5, 3, 6, None),
//      RawEdge(6, 4, 5, None),
//      RawEdge(7, 4, 7, None),
//      RawEdge(8, 5, 8, None),
//      RawEdge(9, 6, 7, None),
//      RawEdge(10, 7, 8, None),
//      RawEdge(11, 4, 6, None)
//    )

//    val streams = List(
//      RawObject(1, 1, 0.5, List(new Point2d(5, 7, .6, 1), new Point2d(4, 5, .1, 1), new Point2d(7, 6, .3, 1))),
//      RawObject(2, 2, 0.5, List(new Point2d(6, 8, .6, 2), new Point2d(4, 4, .1, 2), new Point2d(7, 6, .3, 2))),
//      ExpiredObject(1),
//      RawObject(3, 2, 0.6, List(new Point2d(5, 6, .4, 3), new Point2d(5, 6, .2, 3), new Point2d(6, 6, .4, 3))),
//      RawObject(4, 3, 0.5, List(new Point2d(1, 3, .2, 4), new Point2d(3, 2, .3, 4), new Point2d(1, 4, .5, 4)))
//    )
    println("Used\tFree\tTotal\tMax")
    printMemoryUsage("read node and edge")

    var grid = new Grid

    grid.addRawNodes(cal_table_nodes)
    grid.addRawEdges(cal_table_edges)

    printMemoryUsage("inserted to grid")

    val streamsN = Dataset.generateObjects()

    printMemoryUsage("generate objects")

    val t0 = System.nanoTime()

    streamsN.foldLeft(streamsN) {(acc, stream) => {
      grid = TheAlgorithm(grid, stream)
      acc
    }}

    printMemoryUsage("algorithm")

    val t1 = System.nanoTime()

    println("Time: " + ((t1 - t0) / 1000000) + " ms")
  }
}
