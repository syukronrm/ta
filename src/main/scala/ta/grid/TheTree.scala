package ta.grid

import collection.spatial.{HyperPoint, HyperRect, RTree}
import ta.Constants._
import ta.geometry.{Point2d, Point3d}
import scala.collection.JavaConverters._

import scala.collection.immutable.Set
import archery._
import scalax.collection.edge.{WLkUnDiEdge, WUnDiEdge}
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.collection.mutable

object TheTree {
  /** create empty RTree */
  def createNewTree() = {
    new RTree(new Point2d.Builder, 2, 8, RTree.Split.AXIAL)
    DIMENSION match {
      case 2 =>
        new RTree(new Point2d.Builder(), 2, 8, RTree.Split.AXIAL)
      case 3 =>
        new RTree(new Point3d.Builder(), 2, 8, RTree.Split.AXIAL)
    }
  }

  def createTree2D(): RTree[Point2d] = {
    new RTree(new Point2d.Builder(), 2, 8, RTree.Split.AXIAL)
  }

  def createTree3D(): RTree[Point3d] = {
    new RTree(new Point3d.Builder(), 2, 8, RTree.Split.AXIAL)
  }
}
