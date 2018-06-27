package ta.grid

import collection.spatial.{HyperPoint, RTree}
import ta.geometry._

sealed abstract class AbstractNode

case class Node(id: Int, x: Double, y: Double, var tree: RTree[Point5d], var objects: Set[Object]) extends AbstractNode
