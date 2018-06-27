package ta.stream

import collection.spatial.HyperPoint
import ta.geometry.{Point2d, Point4d, Point5d}

case class RawObject(id: Int, edgeId: Int, position: Double, points: List[Point5d]) extends Stream {
  override def getId: Int = id
}
