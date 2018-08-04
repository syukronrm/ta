package ta

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import ta.Constants.N_OBJECTS
import ta.naive_approach.Naive

import scala.collection.immutable.Set

@State(Scope.Thread)
class TestNaive {
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

    index = N_OBJECTS
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

    naive.naiveAlgorithm(stream)
  }
}
