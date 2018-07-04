package ta.grid

import ta.geometry.{Point2d, Rect2d}

case class Object(id: Int, edgeId: Int, var skyProb: Double, isImpossible: Boolean, nodeId: Int, rect: Rect2d, distance: Double, position: Double) {
  def points(grid: Grid) : List[Point2d] = {
    grid.getRawObject(this.id).get.points
  }

  def asImpossible(): Object = {
    Object(id, edgeId, skyProb, isImpossible = true, nodeId, rect, distance, position)
  }

  def updateSkyProb(newSkyProb: Double): Object = {
    Object(id, edgeId, newSkyProb, isImpossible, nodeId, rect, distance, position)
  }
}