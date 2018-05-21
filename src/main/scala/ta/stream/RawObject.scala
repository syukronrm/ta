package ta.stream

import collection.spatial.HyperPoint
import ta.geometry.Point2d

case class RawObject(id: Int, edgeId: Int, position: Double, points: List[Point2d]) extends Stream {
  override def getId: Int = id
}
