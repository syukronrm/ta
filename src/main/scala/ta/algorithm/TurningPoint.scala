package ta.algorithm

import collection.spatial.RTree

import scala.collection.mutable
import ta.grid.Node
import ta.grid.Edge
import ta.grid.Object
import ta.Constants._
import ta.geometry._
import ta.landmark._
import ta.algorithm.TheAlgorithm._
import ta.grid.Grid
import visualize.ObjectConverter
import java.text.DecimalFormat

import scala.collection.JavaConverters._

object TurningPoint {
  def processLandmark(grid: Grid, nodeS: Node, edge: Edge, nodeE: Node): Vector[TurningPointResult] = {
    val spNodeS = nodeS.objects.filter(o => !o.isImpossible)
    val spNodeE = nodeE.objects.filter(o => !o.isImpossible)
    val Se = edge.objects

    val findSimilar = (objects: Set[Object], obj: Object) =>
      objects.find(o => o.id == obj.id & o != obj)

    val SP = spNodeS ++ spNodeE ++ Se

    val dataPoints = SP.map { o =>
      o.id -> grid.getRawObject(o.id).get.points
    }.toMap

    val Q = mutable.Queue[Landmark]()

    def findDominatedObjects(obj: Object, objects: Set[Object]): Set[Object] = {
      objects.filter(o => {
        val pointsL = dataPoints(obj.id)
        val points = dataPoints(o.id)

        val tree = new RTree(new Point2d.Builder(), 2, 8, RTree.Split.AXIAL)

        pointsL.foreach(p => tree.add(p))
        points.foreach(p => tree.add(p))

        val ddrRect = new Rect2d(pointsL.asJava).getDDR.asInstanceOf[Rect2d]

        val objProb = getDominationProbability(tree, ddrRect, o.id)

        if (objProb > 1 - P_THRESHOLD)
          true
        else
          false
      })
    }

    val SeId = Se.map(_.id)

    Se.foreach { obj =>
      val llDistance = obj.distance - D_EPSILON
      if (llDistance >= 0 & llDistance <= edge.length) {
        val landmarkLeft = new LandmarkLeft(obj.distance - D_EPSILON, Some(edge.id), obj.id)
        Q.enqueue(landmarkLeft)
      }

      val lrDistance = obj.distance + D_EPSILON
      if (lrDistance <= edge.length & lrDistance >= 0) {
        val landmarkRight = new LandmarkRight(obj.distance + D_EPSILON, Some(edge.id), obj.id)
        Q.enqueue(landmarkRight)
      }
    }

    spNodeS
      .filterNot(o => SeId.contains(o.id))
      .foreach { obj =>
        val distance = findDistanceFromNodeS(obj, edge, spNodeE)
        val distanceLandmark = D_EPSILON + distance
        if (distanceLandmark >= 0 & distanceLandmark <= edge.length) {
          val landmarkRight = new LandmarkRight(distanceLandmark, Some(edge.id), obj.id)
          Q.enqueue(landmarkRight)
        }
      }

    spNodeE
      .filterNot(o => SeId.contains(o.id))
      .foreach { obj =>
        val distanceObject = findDistanceFromNodeE(obj, edge, spNodeS)
        val distanceLandmark = distanceObject - D_EPSILON
        if (distanceLandmark >= 0 & distanceLandmark <= edge.length) {
          val landmarkLeft = new LandmarkLeft(distanceLandmark, Some(edge.id), obj.id)

          val similar = findSimilar(spNodeS, obj)
          if (similar.isDefined) {
            if (math.abs(similar.get.distance - obj.distance) != edge.length) {
              Q.enqueue(landmarkLeft)
            }
          } else {
            Q.enqueue(landmarkLeft)
          }
        }
      }

    SP.foreach { o =>
      val distanceO = findDistance(o, edge, spNodeS, spNodeE)

      val dominatedObjects = findDominatedObjects(o, SP.filterNot(_.id == o.id))
      dominatedObjects.foreach { dominatedObj =>
        val distanceDominatedObj = findDistance(dominatedObj, edge, spNodeS, spNodeE)
        var distanceLandmark: Double = -1.0

        if (distanceO > distanceDominatedObj) {
          // landmark mid left
          distanceLandmark = -1.0
          if (spNodeS.contains(dominatedObj) & !SeId.contains(dominatedObj.id)) {
            distanceLandmark = (distanceO - distanceDominatedObj)/2
          } else if (SeId.contains(dominatedObj.id)) {
            distanceLandmark = (distanceO + distanceDominatedObj)/2
          }

          if (distanceLandmark >= 0 & distanceLandmark <= edge.length) {
            val landmark = new LandmarkLeftMid(distanceLandmark, Some(edge.id), o.id, dominatedObj.id)

            val isExists = Q.exists {
              case mid: LandmarkLeftMid =>
                if (mid.objId == o.id & mid.objDominatedId == dominatedObj.id) {
                  true
                } else {
                  false
                }
              case _ =>
                false
            }

            if (!isExists) {
              Q.enqueue(landmark)
            }
          }
        } else {
          // landmark mid right
          distanceLandmark = -1.0
          if (spNodeE.contains(dominatedObj) & !SeId.contains(dominatedObj.id)) {
            distanceLandmark = (distanceDominatedObj - distanceO)/2
          } else if (SeId.contains(dominatedObj.id)) {
            distanceLandmark = (distanceO + distanceDominatedObj)/2
          }

          if (distanceLandmark >= 0 & distanceLandmark <= edge.length) {
            val landmark = new LandmarkRightMid(distanceLandmark, Some(edge.id), o.id, dominatedObj.id)

            val isExists = Q.exists {
              case mid: LandmarkRightMid =>
                if (mid.objId == o.id & mid.objDominatedId == dominatedObj.id) {
                  true
                } else {
                  false
                }
              case _ =>
                false
            }

            if (!isExists) {
              Q.enqueue(landmark)
            }
          }
        }
      }
    }

    processLandmark(Q.toList, spNodeS, spNodeE, edge)
  }

  def findDistanceFromNodeS(obj: Object, edge: Edge, spNodeE: Set[Object]): Double = {
    if (obj.edgeId == edge.id) {
      edge.length * obj.position
    } else {
      val objInSpNodeEMaybe = spNodeE.find(_.id == obj.id)
      if (objInSpNodeEMaybe.isDefined) {
        if (objInSpNodeEMaybe.get.distance < obj.distance) {
          obj.distance + edge.length
        } else {
          obj.distance * -1
        }
      } else {
        obj.distance * -1
      }
    }
  }

  def findDistanceFromNodeE(obj: Object, edge: Edge, spNodeS: Set[Object]): Double = {
    if (obj.edgeId == edge.id) {
      edge.length * obj.position
    } else {
      val objInSpNodeSMaybe = spNodeS.find(_.id == obj.id)
      if (objInSpNodeSMaybe.isDefined) {
        if (objInSpNodeSMaybe.get.distance < obj.distance) {
          obj.distance * -1
        } else {
          obj.distance + edge.length
        }
      } else {
        obj.distance + edge.length
      }
    }
  }

  def findDistance(obj: Object, edge: Edge, spNodeS: Set[Object], spNodeE: Set[Object]): Double = {
    if (obj.edgeId == edge.id) {
      edge.length * obj.position
    } else {
      if (spNodeS.contains(obj)) {
        findDistanceFromNodeS(obj, edge, spNodeE)
      } else {
        findDistanceFromNodeE(obj, edge, spNodeS)
      }
    }
  }

  def processLandmark(landmarks: List[Landmark], spNodeS: Set[Object], spNodeE: Set[Object], edge: Edge): Vector[TurningPointResult] = {
    val sortedLandmarks = landmarks.sortBy(_.distance)
    val objects = spNodeS ++ spNodeE ++ edge.objects

    var SP = spNodeS

    val filteredLandmarks = sortedLandmarks.filterNot(_.distance < 0)
    var queue = scala.collection.mutable.Queue[Landmark](filteredLandmarks: _*)

    var dStart: Double = 0
    val dEnd: Double = edge.length

    var turningPointList = Vector[TurningPointResult]()

    def isObjectInSP(SP: Set[Object], objId: Int): Boolean = {
      SP.exists(_.id == objId)
    }

    while (queue.nonEmpty) {
      val l = queue.dequeue()

      l match {
        case _: LandmarkLeft =>
          val isLandmarkRightMidExist = queue.exists {
            case a: LandmarkRightMid =>
              a.objDominatedId == l.objId
            case _ =>
              false
          }

          if (!isLandmarkRightMidExist) {
            val obj = objects.find(_.id == l.objId).get

            turningPointList = turningPointList :+ TurningPointResult(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP + obj
          }
        case _: LandmarkRight =>
          if (isObjectInSP(SP, l.objId)) {
            val obj = SP.find(_.id == l.objId).get

            turningPointList = turningPointList :+ TurningPointResult(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP - obj
          }
        case landmark: LandmarkLeftMid =>
          if (isObjectInSP(SP, landmark.objId)
            & isObjectInSP(SP, landmark.objDominatedId)) {
            val objDominated = SP.find(_.id == landmark.objDominatedId).get

            turningPointList = turningPointList :+ TurningPointResult(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP - objDominated
          }
        case _ =>
          val landmark = l.asInstanceOf[LandmarkRightMid]

          val isLandmarkRightMidExist = queue.exists {
            case a: LandmarkRightMid =>
              a.objDominatedId == landmark.objDominatedId
            case _ =>
              false
          }

          if (isObjectInSP(SP, l.objId) & !isLandmarkRightMidExist) {
            val objDominated = objects.find(_.id == landmark.objDominatedId).get

            turningPointList = turningPointList :+ TurningPointResult(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP + objDominated
          }
      }
    }

    val formatter = new DecimalFormat("#.##")

    if (ENV != "TESTING") {
      println("  Update Edge, ID: " + edge.id)
      turningPointList = turningPointList :+ TurningPointResult(dStart, dEnd, SP)
      turningPointList.foreach { tp =>
        var objectIds = tp.SP.map(_.id)
        println("    - Dari jarak " + formatter.format(tp.dStart) +
          "\thingga " + formatter.format(tp.dEnd) +
          "\tSkylinePoint-nya adalah objek " + objectIds)
      }
    }

    if (IS_VISUALIZATION)
      ObjectConverter.sendTP(turningPointList.toList, edge)

    turningPointList
  }
}
