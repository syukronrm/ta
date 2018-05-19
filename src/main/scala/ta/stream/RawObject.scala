package ta.stream

import collection.spatial.HyperPoint

case class RawObject(id: Int, edgeId: Int, position: Double, points: Set[HyperPoint]) extends Stream {
  override def getId: Int = id
}
