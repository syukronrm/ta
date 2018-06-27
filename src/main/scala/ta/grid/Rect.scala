package ta.grid

import collection.spatial.{HyperPoint, HyperRect}
import ta.stream.RawObject
import ta.Constants._
import ta.geometry._

import scala.collection.JavaConverters._

object Rect {
  def createRect(points: List[Point5d]) = {
    val objectList = points.toList.asJava
    new Rect5d(objectList)
  }
}
