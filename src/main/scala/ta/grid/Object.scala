package ta.grid

import ta.geometry.Point2d
import ta.stream.RawObject

case class Object(id: Int, edgeId: Int, skyProb: Double, isImpossible: Boolean, nodeId: Int, points: List[Point2d], distance: Double, position: Double) {
  def getRawObject(grid: Grid): Option[RawObject] = {
    grid.getRawObject(id)
  }
}
