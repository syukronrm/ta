package ta.grid

import collection.spatial.{HyperPoint, HyperRect}
import ta.stream.RawObject
import ta.Constants._
import ta.geometry.{Point2d, Rect2d, Rect3d}

import scala.collection.JavaConverters._

object Rect {
  def createRect(points: List[Point2d]) = {
    val objectList = points.toList.asJava
    DIMENSION match {
      case 2 =>
        new Rect2d(objectList)
    }
  }
}
