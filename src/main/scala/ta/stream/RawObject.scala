package ta.stream

import collection.spatial.HyperPoint
import ta.geometry.{Point2d, Point3d}

case class RawObject(id: Int, edgeId: Int, position: Double, points: List[Point3d]) extends Stream {
  override def getId: Int = id
}
