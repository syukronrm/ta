package ta.algorithm

import collection.spatial.RTree

import scala.collection.mutable
import ta.grid.Node
import ta.grid.Edge
import ta.grid.Object
import ta.Constants._
import ta.geometry._
import ta.stream.RawObject
import ta.landmark._
import ta.algorithm.TheAlgorithm._

import scala.collection.JavaConverters._

case class TP(dStart: Double, dEnd: Double, SP: Set[Object])

object TurningPoint {
  def processLandmark(nodeS: Node, edge: Edge, nodeE: Node): Unit = {
    val spNodeS = nodeS.objects
    val spNodeE = nodeE.objects
    val spEdge = edge.objects
    val uncertainDataSpEdge = edge.objects

    val findSimilar = (objects: Set[Object], obj: Object) =>
      objects.find(o => o.id == obj.id & o != obj)

    def filterSPs(spNodeS: Set[Object], spNodeE: Set[Object], spEdge: Set[Object]): Set[Object] = {
      spNodeS ++ spNodeE ++ spEdge
    }

    val SP = filterSPs(spNodeS, spNodeE, spEdge)
    println("GSP " + SP.map(_.id).toString())
    val Q = mutable.Queue[Landmark]()

    def findLandmarkMid(sp: Set[Object], objL: Object, edge: Edge,
                        ll: LandmarkLeft, lr: LandmarkRight,
                        spNodeS: Set[Object], spNodeE: Set[Object]
                       ): Set[Landmark] = {
      val LMidObjects = determineLMidObjects(sp, objL)

      var landmarks = Set[Landmark]()

      LMidObjects.foreach(objLMid => {
        if (objL.id != objLMid.id) {
          val landmarkMaybe = determineLMid(objL, objLMid, edge, ll, lr, spNodeS, spNodeE)

          if (landmarkMaybe.isDefined) {
            landmarks += landmarkMaybe.get
          }
        }
      })

      landmarks
    }

    var similarObjectIds = Set[Int]()

    SP.foreach(objL => {
      println("  Object " + objL.id)

      val similarObject = findSimilar(SP, objL)

      similarObject match {
        case None =>
          val distance = findDistance(objL, edge, spNodeS, spNodeE)
          println("    distance from node S " + distance)

          val ll = createLandmarkLeft(distance, edge, objL.id)
          val lr = createLandmarkRight(distance, edge, objL.id)

          Q.enqueue(ll)
          println("      enqueue " + ll)

          if (lr.distance >= 0 & lr.distance <= edge.length) {
            Q.enqueue(lr)
            println("      enqueue " + lr)
          }


          println("    Landmark Left  : " + ll.toString)
          println("    Landmark Right : " + lr.toString)

          val sp = SP - objL
          val lm = findLandmarkMid(sp, objL, edge, ll, lr, spNodeS, spNodeE)

          lm.foreach(Q.enqueue(_))
        case _ =>
          if (!similarObjectIds.contains(similarObject.get.id)) {
            similarObjectIds = similarObjectIds + similarObject.get.id

            val objId = objL.id
            println("      There are same objects in edge: objId " + objId)

            val obj1 = objL
            val obj2 = similarObject.get
            println("        obj1 distance " + obj1.distance)
            println("        obj2 distance " + obj2.distance)

            if (Math.abs(obj2.distance - obj1.distance) == edge.length) {
              // same object
              val ll = new LandmarkLeft(-0.001, Some(edge.id), objId)

              Q.enqueue(ll)
            } else {
              // difference object come from node S and node E
              val obj1Distance = findDistance(obj1, edge, spNodeS, spNodeE)
              println("        obj1Distance " + obj1Distance)
              val llObj1 = createLandmarkLeft(obj1Distance, edge, objId)
              val lrObj1 = createLandmarkRight(obj1Distance, edge, objId)
              println("        obj1 LandmarkLeft " + llObj1)
              println("        obj1 LandmarkLeft " + lrObj1)

              val obj2Distance = findDistance(obj2, edge, spNodeS, spNodeE)
              println("        obj2Distance " + obj2Distance)
              val llObj2 = createLandmarkLeft(obj2Distance, edge, objId)
              val lrObj2 = createLandmarkRight(obj2Distance, edge, objId)
              println("        obj2 LandmarkLeft " + llObj2)
              println("        obj2 LandmarkLeft " + lrObj2)

              if ((lrObj1.distance > llObj2.distance & llObj1.distance < lrObj2.distance)
                  | (lrObj2.distance > llObj1.distance & llObj2.distance < lrObj1.distance)) {
                println("        Overlapped")
                val ll = new LandmarkLeft(-0.001, Some(edge.id), objId)
                Q.enqueue(ll)
              } else {
                println("        Not overlapped")
                val spObj1 = SP - obj1
                val lmObj1 = findLandmarkMid(spObj1, obj1, edge, llObj1, lrObj1, spNodeS, spNodeE)

                Q.enqueue(llObj1)
                println("      enqueue " + llObj1)

                if (lrObj1.distance >= 0 & lrObj1.distance <= edge.length) {
                  Q.enqueue(lrObj1)
                  println("      enqueue " + lrObj1)
                }

                lmObj1.foreach(Q.enqueue(_))

                val spObj2 = SP - obj1
                val lmObj2 = findLandmarkMid(spObj2, obj2, edge, llObj2, lrObj2, spNodeS, spNodeE)

                Q.enqueue(llObj2)
                println("      enqueue " + llObj2)

                if (lrObj2.distance >= 0 & lrObj2.distance <= edge.length) {
                  Q.enqueue(lrObj2)
                  println("      enqueue " + lrObj2)
                }

                lmObj2.foreach(Q.enqueue(_))
              }
            }
          }
      }
    })

    if (Q.nonEmpty) {
      println("    Total Landmarks: ")
      Q.foreach(l => {
        println("        " + l)
      })
    }

    processLandmark(Q.toList, SP, edge)
  }

  def createLandmarkLeft(distance: Double, edge: Edge, objLId: Int): LandmarkLeft = {
    val loc = distance - D_EPSILON
    val edgeIdMaybe =
      if (loc >= 0 & loc <= edge.length) {
        Some(edge.id)
      } else {
        None
      }

    new LandmarkLeft(loc, edgeIdMaybe, objLId)
  }

  def createLandmarkRight(distance: Double, edge: Edge, objLId: Int): LandmarkRight = {
    val loc = distance + D_EPSILON
    val edgeIdMaybe =
      if (loc >= 0 & loc <= edge.length) {
        Some(edge.id)
      } else {
        None
      }

    new LandmarkRight(loc, edgeIdMaybe, objLId)
  }

  def findDistance(obj: Object, edge: Edge, spNodeS: Set[Object], spNodeE: Set[Object]): Double = {
    if (obj.edgeId == edge.id) {
      println("    DEBUG distance obj " + obj.id + " : in edge " + edge.id + " len edge " + edge.length + " position " + obj.position)
      edge.length * obj.position
    } else {
      if (spNodeS.contains(obj)) {
        println("    DEBUG distance obj " + obj.id + " from node s " + obj.distance)
        obj.distance * -1
      } else {
        println("    DEBUG distance obj " + obj.id + " from node e " + obj.distance)
        obj.distance + edge.length
      }
    }
  }

  def determineLMid(objL: Object, objLMid: Object, edge: Edge, ll: LandmarkLeft, lr: LandmarkRight, spNodeS: Set[Object], spNodeE: Set[Object]): Option[Landmark] = {
    val objLdistance = findDistance(objL, edge, spNodeS, spNodeE)
    val objLMidDistance = findDistance(objLMid, edge, spNodeS, spNodeE)

    val objLId = objL.id
    val objLMidId = objLMid.id

    val distanceBetween = (objLdistance + objLMidDistance) / 2
    println("      distance between obj1 " + objLId + " obj2 " + objLMidId + " is " + distanceBetween)

    if (distanceBetween >= 0 & distanceBetween <= edge.length & Math.abs(objLdistance - distanceBetween) <= D_EPSILON) {
      if (objLdistance > objLMidDistance) {
        println("      obj1 " + objLId + " distance " + objLdistance)
        println("      obj2 " + objLMidId + " distance " + objLMidDistance)
        val landmark = new LandmarkLeftMid(distanceBetween, Some(edge.id), objLId, objLMidId)
        Some(landmark)
      } else {
        println("      obj1 " + objLdistance)
        println("      obj2 " + objLMidDistance)
        val landmark = new LandmarkRightMid(distanceBetween, Some(edge.id), objLId, objLMidId)
        Some(landmark)
      }
    } else {
      None
    }
  }

  def determineLMidObjects(sp: Set[Object], objL: Object): Set[Object] = {
    sp.filter(o => {
      val pointsL = objL.points
      val points = o.points

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

  def findInitialSPIds(sortedLandmarks: Seq[Landmark]): Seq[Int] = {
    val initialSP = mutable.Stack[Int]()

    sortedLandmarks.foreach(l => {
      if (l.distance <= 0) {
        val objId = l.objId
        l match {
          case _: LandmarkLeft =>
            initialSP.push(objId)
          case _ =>
            None
        }
      } else {
        return initialSP.toSeq
      }
    })

    initialSP.toSeq
  }

  def processLandmark(landmarks: List[Landmark], objects: Set[Object], edge: Edge): Unit = {
    val sortedLandmarks = landmarks.toSeq.sortBy(_.distance)

    var SP = findInitialSPIds(sortedLandmarks).map(objId => objects.find(_.id == objId).get).toSet

    val filteredLandmarks = sortedLandmarks.filterNot(_.distance < 0)
    val queue = scala.collection.mutable.Queue[Landmark](filteredLandmarks: _*)

    var dStart: Double = 0
    val dEnd: Double = edge.length

    var turningPointList = Vector[TP]()

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

            turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP + obj
          }
        case _: LandmarkRight =>
          if (isObjectInSP(SP, l.objId)) {
            val obj = SP.find(_.id == l.objId).get

            turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP - obj
          }
        case landmark: LandmarkLeftMid =>

          if (isObjectInSP(SP, landmark.objId)
            & isObjectInSP(SP, landmark.objDominatedId)) {
            val objDominated = SP.find(_.id == landmark.objDominatedId).get

            turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

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

            turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP + objDominated
          }
      }
    }

    turningPointList = turningPointList :+ TP(dStart, dEnd, SP)

    println("      Total Turning Points:")
    turningPointList.foreach(t => {
      println("        Start: " + t.dStart + "\t End: " + t.dEnd + "\t SP: " + t.SP.map(_.id).toString())
    })
  }
}
