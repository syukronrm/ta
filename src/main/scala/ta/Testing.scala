package ta

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import ta.Constants.{ENV, N_OBJECTS}
import ta.algorithm.TheAlgorithm.TheAlgorithm
import ta.grid.Grid
import ta.stat.Hotel

import scala.collection.immutable.Set

// Proses pengujian/benchmarking menggunakan JMH
// https://github.com/ktoso/sbt-jmh
@State(Scope.Thread)
class Testing {

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

    index = N_OBJECTS
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
