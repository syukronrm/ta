package ta.grid

import collection.spatial.{HyperPoint, RTree}
import ta.geometry.Point3d

sealed abstract class AbstractNode

case class Node(id: Int, x: Double, y: Double, var tree: RTree[Point3d], var objects: Set[Object]) extends AbstractNode
