package ta

import java.util
import java.util.concurrent.TimeUnit

import collection.spatial.{HyperPoint, RTree, RectBuilder}
import org.openjdk.jmh.annotations.Setup
import ta.grid.Node
import ta.grid.Edge
import ta.grid.Grid
import ta.stream.{ExpiredObject, RawObject}
import ta.geometry.Point2d
import ta.algorithm.TheAlgorithm._
import ta.Constants._
import ta.naive_approach.Naive
import com.rits.cloning.Cloner
import com.sun.java_cup.internal.runtime.Scanner

import scala.collection.immutable.Set
import scala.collection.parallel.ParSet

case class RawNode(id: Int, x: Double, y: Double)
case class RawEdge(id: Int, i: Int, j: Int, lengthMaybe: Option[Double])

object TestImport {
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

    val streams = List(
      RawObject(1, 1, 0.5, List(new Point2d(5, 7, .6, 1), new Point2d(4, 5, .1, 1), new Point2d(7, 6, .3, 1))),
      RawObject(2, 2, 0.5, List(new Point2d(6, 8, .6, 2), new Point2d(4, 4, .1, 2), new Point2d(7, 6, .3, 2))),
      ExpiredObject(1),
      RawObject(3, 2, 0.6, List(new Point2d(5, 6, .4, 3), new Point2d(5, 6, .2, 3), new Point2d(6, 6, .4, 3))),
      RawObject(4, 3, 0.5, List(new Point2d(1, 3, .2, 4), new Point2d(3, 2, .3, 4), new Point2d(1, 4, .5, 4)))
    )

    val cal_table_nodes = Dataset.readNode()
    val cal_table_edges = Dataset.readEdge()
    val generatedStream = Dataset.generateObjects()

    println(Constants.N_OBJECTS)

    var grid = new Grid
    grid.addRawNodes(cal_table_nodes.toSet)
    grid.addRawEdges(cal_table_edges.toSet)

    println(generatedStream.size)

    (0 to 999).foreach { i =>
      val stream = generatedStream.lift(i).get
      println(stream.getId + " from " + (Constants.N_OBJECTS + Constants.TIME_EXPIRATION))
      grid = TheAlgorithm(grid, stream)
    }
//    generatedStream.foreach { stream =>
//      println(stream.getId + " from " + (Constants.N_OBJECTS + Constants.TIME_EXPIRATION))
//      grid = TheAlgorithm(grid, stream)
//    }

    grid.createDataNaive

    val naiveApproach = new Naive
    naiveApproach.addNodes(table_nodes)
    naiveApproach.addRawEdges(table_edges)

    naiveApproach.readDataGrid
    println(naiveApproach.objects)
  }
}

object TAQ {
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

      println(stream.getId + " from " + (Constants.N_OBJECTS + Constants.TIME_EXPIRATION))
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

import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.State
import org.openjdk.jmh.annotations.Warmup
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.OperationsPerInvocation
import org.openjdk.jmh.annotations.TearDown
import org.openjdk.jmh.annotations.Level
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.Timeout

@State(Scope.Benchmark)
object BenchmarkStreamState {
}

import stat.Hotel

@State(Scope.Thread)
class BenchmarkStream {
  import BenchmarkStreamState._

  var index = 0

  var streamsN: List[stream.Stream] = _

  var gridFixed: Grid = _
  var grid: Grid = _

  def runInitial(): Grid = {
    var _grid = new Grid

    val cal_table_nodes: Set[RawNode] = Dataset.readNodePartial().toSet
    val cal_table_edges: Set[RawEdge] = Dataset.readEdgePartial().toSet

    println("nodes size " + cal_table_nodes.size)
    println("edges size " + cal_table_edges.size)

    val table_edges: Set[RawEdge] = cal_table_edges
    val table_nodes: Set[RawNode] = cal_table_nodes

    streamsN = Dataset.generateObjects()

    _grid.addRawNodes(table_nodes)
    _grid.addRawEdges(table_edges)

    ENV = "TESTING"
    (0 until N_OBJECTS).foreach { i =>
      if (i % 100 == 0)
        println("runInitial " + i)
      val stream = streamsN.lift(i).get
      _grid = TheAlgorithm(_grid, stream)
    }
    ENV = "TESTING"
    println("Start Update All SkyProb")
//    _grid.updateAllSkyProb()
    println("End Update All SkyProb")

    _grid.createDataNaive

    _grid.nodes.foreach(n => {
      Hotel.add(n.objects.size)
    })

    println(Hotel)
    println(Hotel.average())
    Hotel.flush()

    _grid
  }

  // @Param(Array("100", "1000", "5000", "10000", "20000"))
  var nObjects: Int = 5000

  // 0.5km, 2.5km, 5km, 10km, 15km
  // @Param(Array("0.1", "0.5", "2", "3"))
  var distance: Double = 1

  // @Param(Array("32", "64", "128", "256", "512"))
  var gridCell: Int = 256

//  @Param(Array("150", "200"))
  var nPoints: Int = 50

//  @Param(Array("2", "1", "3"))
  var kindOfData: Int = 3

  // @Param(Array("0.1", "0.3", "0.5", "0.7", "0.9"))
  var threshold: Double = 0.5

  @Setup
  def setup(): Unit = {
    Constants.N_OBJECTS = nObjects
    Constants.TIME_EXPIRATION = nObjects
    Constants.PERCENT_DISTANCE = distance
    Constants.N_GRID_CELL = gridCell
    Constants.N_POINTS = nPoints
    Constants.KIND_OF_DATA = kindOfData
    Constants.P_THRESHOLD = threshold

    grid = runInitial()

//    grid = gridFixed.cloneGrid()
    index = N_OBJECTS
  }

  @TearDown(Level.Iteration)
  def tear(): Unit = {
//    grid = gridFixed.cloneGrid()
//    index = N_OBJECTS
  }

  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Timeout(time = 120, timeUnit = TimeUnit.MINUTES)
  @Measurement(iterations = 10, time = 20)
  def doStreaming(): Unit = {
    val streamMaybe = streamsN.lift(index)

    val stream = streamMaybe match {
      case Some(_stream) =>
        index += 1
        _stream
      case None =>
        index = N_OBJECTS
        streamsN.lift(index).get
    }

    grid = TheAlgorithm(grid, stream)
  }
}

@State(Scope.Thread)
class BenchmarkBruteForce {
  var index = 0

  var streamsN: List[stream.Stream] = _

  var naiveFixed: Naive = _
  var naive: Naive = _

  def runInitial() = {
    val cal_table_nodes: Set[RawNode] = Dataset.readNodePartial().toSet
    val cal_table_edges: Set[RawEdge] = Dataset.readEdgePartial().toSet
    val table_nodes: Set[RawNode] = cal_table_nodes
    val table_edges: Set[RawEdge] = cal_table_edges

    println("generate dataset")
    streamsN = Dataset.generateObjects()

    val naiveApproach = new Naive
    println("add nodes")
    naiveApproach.addNodes(table_nodes)
    println("add edges")
    naiveApproach.addRawEdges(table_edges)

    var prevTime = System.nanoTime()
    var time = System.nanoTime()

    naiveApproach.readDataGrid

//    println("initial stream")
//    (0 until N_OBJECTS).foreach { i =>
//      //if (i % 20 == 0)
//      println("runInitial " + i)
//      val stream = streamsN.lift(i).get
//      naiveApproach.naiveAlgorithm(stream)
//      time = System.nanoTime()
//      println("time " + (time - prevTime)/1000000)
//      prevTime = System.nanoTime()
//    }

    naiveApproach
  }

  // @Param(Array("100", "1000", "5000", "10000", "20000"))
  var nObjects: Int = 5000

  // 0.5km, 2.5km, 5km, 10km, 15km
  // @Param(Array("0.1", "0.5", "2", "3"))
  var distance: Double = 1

  // @Param(Array("32", "64", "128", "256", "512"))
  var gridCell: Int = 256

  @Param(Array("10", "50", "100", "150", "200"))
  var nPoints: Int = 50

  // @Param(Array("1", "2", "3"))
  var kindOfData: Int = 3

  @Setup
  def setup(): Unit = {
    Constants.N_OBJECTS = nObjects
    Constants.TIME_EXPIRATION = nObjects
    Constants.PERCENT_DISTANCE = distance
    Constants.N_GRID_CELL = gridCell
    Constants.N_POINTS = nPoints
    Constants.KIND_OF_DATA = kindOfData

    println("runInitial()")
    naive = runInitial()

    //naive = naiveFixed.cloneMe()
    index = N_OBJECTS
  }

  @TearDown(Level.Iteration)
  def tear(): Unit = {
//    naive = naiveFixed.cloneMe()
    //index = N_OBJECTS
  }


  @Benchmark
  @BenchmarkMode(Array(Mode.AverageTime))
  @Timeout(time = 120, timeUnit = TimeUnit.MINUTES)
  @Measurement(iterations = 10, time = 120)
  def doStreaming(): Unit = {
    val streamMaybe = streamsN.lift(index)

    val stream = streamMaybe match {
      case Some(_stream) =>
        index += 1
        _stream
      case None =>
        index = N_OBJECTS
        streamsN.lift(index).get
    }
//
//    if (stream.isInstanceOf[RawObject]) {
//      println("Index "+ index +" Stream RawObject " + stream.getId)
//    }
//
//    if (stream.isInstanceOf[ExpiredObject]) {
//      println("Index "+ index +" Stream Expire " + stream.getId)
//    }

    naive.naiveAlgorithm(stream)
  }
}
