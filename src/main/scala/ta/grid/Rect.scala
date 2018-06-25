package ta.grid

import collection.spatial.{HyperPoint, HyperRect}
import ta.stream.RawObject
import ta.Constants._
import ta.geometry.{Point2d, Point3d, Rect2d, Rect3d}

import scala.collection.JavaConverters._

object Rect {
  def createRect(points: List[Point3d]) = {
    val objectList = points.toList.asJava
    new Rect3d(objectList)
  }
}
