package ta.grid

import ta.geometry._
import ta.stream.RawObject

case class Object2(id: Int, edgeId: Int, var skyProb: Double, isImpossible: Boolean, nodeId: Int, points: List[Point2d], distance: Double, position: Double) {
  def getRawObject(grid: Grid): Option[RawObject] = {
    grid.getRawObject(id)
  }
}

case class Object(id: Int, edgeId: Int, var skyProb: Double, isImpossible: Boolean, nodeId: Int, rect: Rect5d, distance: Double, position: Double) {
  def points(grid: Grid) : List[Point5d] = {
    grid.getRawObject(this.id).get.points
  }
}