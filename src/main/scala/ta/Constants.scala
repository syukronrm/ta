package ta

object Constants {
  @inline var D_EPSILON: Double = 10
  @inline final val P_THRESHOLD = 0.5
  @inline final val N_GRID_CELL = 64
  @inline var GRID_WIDTH: Double = 5
  @inline var GRID_HEIGHT: Double = 5
  @inline final val DIMENSION = 2

  @inline var N_OBJECTS = 100
  @inline final val N_POINTS = 10

  @inline val MIN_DATASPACE = 0
  @inline val MAX_DATASPACE = 10000

  @inline val RANGE = 250

  @inline var PERCENT_DISTANCE = 0.5

  @inline var TIME_EXPIRATION = 100

  @inline val MIN_EDGE_ID = 0
  @inline val MAX_EDGE_ID = 21692

  @inline val N_STREAM = 100

  @inline var KIND_OF_DATA = 1
  // 1 : anticorrelated
  // 2 : correlated
  // 3 : independence
}
