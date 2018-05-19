package ta.grid

import ta.geometry.Point2d
import ta.stream.RawObject

case class Object(id: Int, skyProb: Double, isImpossible: Boolean, nodeId: Int) {
  def getRawObject[T](grid: Grid[T]): Option[RawObject] = {
    grid.getRawObject(id)
  }
}
