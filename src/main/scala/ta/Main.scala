package ta

import java.util
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Setup
import ta.grid.Grid
import ta.stream.{ExpiredObject, RawObject}
import ta.geometry.Point2d
import ta.algorithm.TheAlgorithm._
import ta.Constants._
import ta.naive_approach.Naive
import visualize.Exporter._

import scala.collection.immutable.Set

case class RawNode(id: Int, x: Double, y: Double)
case class RawEdge(id: Int, i: Int, j: Int, lengthMaybe: Option[Double])

// main object dari TA
object Main {
  val mb: Int = 1024 * 1024

  def main(args: Array[String]): Unit = {
    //    val cal_table_nodes = Dataset.readNode()
    //    val cal_table_edges = Dataset.readEdge()

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

    val streams = List(
      RawObject(1, 1, 0.5, List(new Point2d(5, 7, .6, 1), new Point2d(4, 5, .1, 1), new Point2d(7, 6, .3, 1))),
      RawObject(2, 2, 0.5, List(new Point2d(6, 8, .6, 2), new Point2d(4, 4, .1, 2), new Point2d(7, 6, .3, 2))),
      ExpiredObject(1),
      RawObject(3, 2, 0.6, List(new Point2d(5, 6, .4, 3), new Point2d(5, 6, .2, 3), new Point2d(6, 6, .4, 3))),
      RawObject(4, 3, 0.5, List(new Point2d(1, 3, .2, 4), new Point2d(3, 2, .3, 4), new Point2d(1, 4, .5, 4)))
    )



    var grid = new Grid

//    grid.addRawNodes(table_nodes)
//    grid.addRawEdges(table_edges)


    //    val streamsN = Dataset.generateObjects()


    val t0 = System.nanoTime()

    //    val streamSize = streamsN.size

    var tStart = System.nanoTime()

//    val cal_table_nodes: Set[RawNode] = Dataset.readPartialNode().toSet
//    val cal_table_edges: Set[RawEdge] = Dataset.readEdgePartial().toSet
//
//    val table_edges: Set[RawEdge] = cal_table_edges
//    val table_nodes: Set[RawNode] = cal_table_nodes

    grid.addRawNodes(table_nodes)
    grid.addRawEdges(table_edges)

    val naiveApproach = new Naive
    naiveApproach.addNodes(table_nodes)
    naiveApproach.addRawEdges(table_edges)

//    val streams = Dataset.generateObjects()

    streams.foldLeft(streams) {(acc, stream) => {
      if (stream.getId == N_OBJECTS && stream.isInstanceOf[RawObject]) {
        tStart = System.nanoTime()
        println(tStart)
      }

      println("Stream " + stream.getId + " from " + streams.size)
      grid = TheAlgorithm(grid, stream)
//      naiveApproach.naiveAlgorithm(stream)
      acc
    }}
    grid.createDataNaive

    val tEnd = System.nanoTime()

    val t1 = System.nanoTime()

    println("Time Total: " + ((t1 - t0) / 1000000) + " ms")
    println("Time Stream " + N_STREAM + " Objects: " + ((tEnd - tStart) / 1000000) + " ms")
  }
}
