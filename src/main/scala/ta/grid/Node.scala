package ta.grid

import collection.spatial.{HyperPoint, RTree}
import ta.geometry.Point2d

sealed abstract class AbstractNode

case class Node[T](id: Int, x: Double, y: Double, _tree: RTree[T], objects: Set[Object]) extends AbstractNode
