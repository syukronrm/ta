package ta.grid

import ta.Constants._
import ta.geometry.{Point2d, Rect2d}

import scala.collection.JavaConverters._

object Rect {
  def createRect(points: List[Point2d]): Rect2d = {
    val objectList = points.toList.asJava
    DIMENSION match {
      case 2 =>
        new Rect2d(objectList)
    }
  }
}
