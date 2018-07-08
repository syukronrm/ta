package visualize

import ta.Dataset.generateUncertainData
import ta.algorithm.TheAlgorithm._
import ta.grid.Grid
import ta.stream.{ExpiredObject, RawObject}
import ta.{RawEdge, RawNode}
import ta.Constants._
import DataNodeEdge._

object Streamer {
  val maxObject: Int = 10

  def main(args: Array[String]): Unit = {
    D_EPSILON = 50

    var grid = new Grid

    grid.addRawNodes(table_nodes.toSet)
    grid.addRawEdges(table_edges.toSet)

    var currentObjectId = 1
    while (true) {
      Thread.sleep(2500)

      var stream = generateRawObject(currentObjectId)
      print("\n generate " + currentObjectId + " on " + stream.edgeId + " ")
      grid = TheAlgorithm(grid, stream)
      ObjectConverter.sendRawObject(grid.rawObjects)

      currentObjectId += 1

      if (currentObjectId > maxObject) {
        Thread.sleep(2500)
        val expiredObject = ExpiredObject(currentObjectId - maxObject)
        print("\n delete " + (currentObjectId - maxObject))
        grid = TheAlgorithm(grid, expiredObject)
      }
      ObjectConverter.sendRawObject(grid.rawObjects)
    }
  }

  def generateRawObject(objectId: Int) = {
    val edgeIndex = Math.floor(Math.random() * (table_edges.size - 1)).toInt
    val edgeId = table_edges.lift(edgeIndex).get.id
    val position = Math.random

    RawObject(objectId, edgeId, position, generateUncertainData(objectId))
  }

  def generate(objectId: Int) = {
    if (objectId > maxObject) {
      Some(ExpiredObject(objectId - maxObject))
    } else {
      None
    }
  }

}
