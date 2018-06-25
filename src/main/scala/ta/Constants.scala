package ta

object Constants {
  @inline var D_EPSILON: Double = 10
  @inline final val P_THRESHOLD = 0.5
  @inline var N_GRID_CELL = 128
  @inline var GRID_WIDTH: Double = 5
  @inline var GRID_HEIGHT: Double = 5
  @inline final val DIMENSION = 2

  @inline var N_OBJECTS = 1000
  @inline var N_POINTS = 100

  @inline val MIN_DATASPACE = 0
  @inline val MAX_DATASPACE = 10000

  @inline val RANGE = 250

  @inline var PERCENT_DISTANCE = 0.5

  @inline var TIME_EXPIRATION = 1000

  @inline val MIN_EDGE_ID = 0
  @inline val MAX_EDGE_ID = 21692

  @inline val N_STREAM = 1000

  @inline var KIND_OF_DATA = 3
  // 1 : anticorrelated
  // 2 : correlated
  // 3 : independence

  @inline var ENV: String = "GENERATE"
  // "TESTING"
}
