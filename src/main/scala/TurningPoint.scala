import scala.collection.mutable
import archery._
import HelloWorld._
import HelloBox._
import Constants._

import scala.collection.mutable.ListBuffer

trait Landmark {
  val distance: Double // Distance to Node S
  val edgeId: Option[Int]
  val objId: Int

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "edgeId " + edge + " distance " + distance.toString + " obj " + objId
  }
}

class LandmarkLeft(_distance: Double, _edgeId: Option[Int], _objId: Int) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val objId: Int = _objId

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "LandmarkLeft in edgeId " + edge + " between objId " + objId + " in location " + distance
  }
}

class LandmarkLeftMid(_distance: Double, _edgeId: Option[Int], _objId: Int, _objDominatedId: Int) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val objId: Int = _objId
  val objDominatedId: Int = _objDominatedId

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "LandmarkLeftMid in edgeId " + edge + " between objId " + objId + " and objId' " + objDominatedId + " in location " + distance
  }
}

class LandmarkRight(_distance: Double, _edgeId: Option[Int], _objId: Int) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val objId: Int = _objId

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "LandmarkRight in edgeId " + edge + " between objId " + objId + " in location " + distance
  }
}

class LandmarkRightMid(_distance: Double, _edgeId: Option[Int], _objId: Int, _objDominatedId: Int) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val objId: Int = _objId
  val objDominatedId: Int = _objDominatedId

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "LandmarkRightMid in edgeId " + edge + " between objId " + objId + " and objId' " + objDominatedId + " in location " + distance
  }
}

case class TP(dStart: Double, dEnd: Double, SP: Set[NodeObject])

object TurningPoint {
  def processLandmark(nodeS: NodeGrid, edge: EdgeGrid, nodeE: NodeGrid): Unit = {
    val spNodeS = nodeS.objects
    val spNodeE = nodeE.objects
    val uncertainDataSpEdge = edge.objects

    val findSimilar = (objects: Set[NodeObject], obj: NodeObject) =>
      objects.find(o => o.obj.id == obj.obj.id & o != obj)

    val determineObject = (obj1: NodeObject, obj2: NodeObject, edgeId: Int) => {
      if (obj1.obj.edgeId == edgeId) {
        spNodeS.contains(obj1)
      } else {
        obj1.distance < obj2.distance
      }
    }

    def convertToNodeObject (objects: Set[UncertainObject], edgeLength: Double): Set[NodeObject] = {
      objects.map(o => {
        val distance = edgeLength * o.pos
        println("  objId " + o.id + " distance " + distance)
        NodeObject(o, 100, isImpossible = false, distance)
      })
    }

    /*
      MARKED AS LOGIC ERROR
      filter object yang sama terdapat pada node S dan node E,
      cari object yang terdekat
     */
    def filterSPs(spNodeS: Set[NodeObject], spNodeE: Set[NodeObject], uncertainDataSpEdge: Set[UncertainObject], edge: EdgeGrid): Set[NodeObject] = {
      var SPs = spNodeS ++ spNodeE
      val spEdge = convertToNodeObject(uncertainDataSpEdge, edge.length.get)
//      val edgeId = edge.id
//
//      val objIdsOnEdge = uncertainDataSpEdge.map(_.id)
//      println("  objectId on edge "+ edgeId + " is " + objIdsOnEdge)
//
//      SPs = SPs.filterNot(o => {
//        objIdsOnEdge.contains(o.obj.id)
//      })
//
//      SPs = SPs.filter(o => {
//        findSimilar(SPs, o) match {
//          case None =>
//            true
//          case objMaybe =>
//            val obj = objMaybe.get
//            determineObject(o, obj, edgeId)
//        }
//      })

      SPs = SPs ++ spEdge
      SPs
    }

    val SP = filterSPs(spNodeS, spNodeE, uncertainDataSpEdge, edge)
    println("GSP " + SP.map(_.obj.id).toString())
    val Q = mutable.Queue[Landmark]()

    val groupedSP = SP.groupBy(_.obj.id)

    val doubleObjIds = mutable.Stack[Int]()

    def findLandmarkMid(sp: Set[NodeObject], objL: NodeObject, edge: EdgeGrid,
                        ll: LandmarkLeft, lr: LandmarkRight,
                        spNodeS: Set[NodeObject], spNodeE: Set[NodeObject]
                       ): Set[Landmark] = {
      val LMidObjects = determineLMidObjects(sp, objL)
      println(LMidObjects)

      var landmarks = Set[Landmark]()

      LMidObjects.foreach(objLMid => {
        val landmarkMaybe = determineLMid(objL, objLMid, edge, ll, lr, spNodeS, spNodeE)

        if (landmarkMaybe.isDefined) {
          landmarks += landmarkMaybe.get
        }
      })

      landmarks
    }

    var similarObjectIds = Set[Int]()

    SP.foreach(objL => {
      println("  Object " + objL.obj.id)

      val similarObject = findSimilar(SP, objL)

      similarObject match {
        case None =>
          val distance = findDistance(objL, edge, spNodeS, spNodeE)
          println("    distance from node S " + distance)

          val ll = createLandmarkLeft(distance, edge, objL.obj.id)
          val lr = createLandmarkRight(distance, edge, objL.obj.id)

          Q.enqueue(ll)

          if (lr.distance >= 0 & lr.distance <= edge.length.get)
            Q.enqueue(lr)

          println("    Landmark Left  : " + ll.toString)
          println("    Landmark Right : " + lr.toString)

          val sp = SP - objL
          val lm = findLandmarkMid(sp, objL, edge, ll, lr, spNodeS, spNodeE)

          lm.foreach(Q.enqueue(_))
        case _ =>
          if (!similarObjectIds.contains(similarObject.get.obj.id)) {
            similarObjectIds = similarObjectIds + similarObject.get.obj.id

            val objId = objL.obj.id
            println("      There are same objects in edge: objId " + objId)
            val obj1 = objL
            val obj2 = similarObject.get

            val obj1Distance = findDistance(obj1, edge, spNodeS, spNodeE)
            val llObj1 = createLandmarkLeft(obj1Distance, edge, objId)
            val lrObj1 = createLandmarkRight(obj1Distance, edge, objId)

            val obj2Distance = findDistance(obj2, edge, spNodeS, spNodeE)
            val llObj2 = createLandmarkLeft(obj2Distance, edge, objId)
            val lrObj2 = createLandmarkRight(obj2Distance, edge, objId)

            if (lrObj1.distance > llObj2.distance & llObj1.distance < lrObj2.distance) {
              println("        Overlapped LandmarkRight Object 1 > LandmarkLeft Object 2")
              val ll = createLandmarkLeft(obj1Distance, edge, objId)
              val lr = createLandmarkRight(obj2Distance, edge, objId)

              Q.enqueue(ll)

              if (lr.distance >= 0 & lr.distance <= edge.length.get) {
                Q.enqueue(lr)
              }

              val sp = SP - obj1 - obj2
              val lm = findLandmarkMid(sp, obj1, edge, ll, lr, spNodeS, spNodeE)

              lm.foreach(Q.enqueue(_))
            } else if (llObj2.distance > lrObj1.distance & lrObj2.distance < llObj1.distance) {
              println("        Overlapped LandmarkLeft Object 1 > LandmarkRight Object 2")
              val ll = createLandmarkLeft(obj2Distance, edge, objId)
              val lr = createLandmarkRight(obj1Distance, edge, objId)

              Q.enqueue(ll)

              if (lr.distance >= 0 & lr.distance <= edge.length.get) {
                Q.enqueue(lr)
              }

              val sp = SP - obj1 - obj2
              val lm = findLandmarkMid(sp, obj1, edge, ll, lr, spNodeS, spNodeE)

              lm.foreach(Q.enqueue(_))
            } else {
              println("        Not overlapped")
              val spObj1 = SP - obj1
              val lmObj1 = findLandmarkMid(spObj1, obj1, edge, llObj1, lrObj1, spNodeS, spNodeE)

              Q.enqueue(llObj1)
              if (lrObj1.distance >= 0 & lrObj1.distance <= edge.length.get) {
                Q.enqueue(lrObj1)
              }

              lmObj1.foreach(Q.enqueue(_))

              val spObj2 = SP - obj1
              val lmObj2 = findLandmarkMid(spObj2, obj2, edge, llObj2, lrObj2, spNodeS, spNodeE)

              Q.enqueue(llObj2)
              if (lrObj2.distance >= 0 & lrObj2.distance <= edge.length.get) {
                Q.enqueue(lrObj2)
              }

              lmObj2.foreach(Q.enqueue(_))
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

  def createLandmarkLeft(distance: Double, edge: EdgeGrid, objLId: Int): LandmarkLeft = {
    val loc = distance - D_EPSILON
    val edgeIdMaybe =
      if (loc >= 0 & loc <= edge.length.get) {
        Some(edge.id)
      } else {
        None
      }

    new LandmarkLeft(loc, edgeIdMaybe, objLId)
  }

  def createLandmarkRight(distance: Double, edge: EdgeGrid, objLId: Int): LandmarkRight = {
    val loc = distance + D_EPSILON
    val edgeIdMaybe =
      if (loc >= 0 & loc <= edge.length.get) {
        Some(edge.id)
      } else {
        None
      }

    new LandmarkRight(loc, edgeIdMaybe, objLId)
  }

  def findDistance(obj: NodeObject, edge: EdgeGrid, spNodeS: Set[NodeObject], spNodeE: Set[NodeObject]): Double = {
    if (obj.obj.edgeId == edge.id) {
      println("    DEBUG distance obj " + obj.obj.id + " : in edge " + edge.id + " len edge " + edge.length.get + " position " + obj.obj.pos)
      edge.length.get * obj.obj.pos
    } else {
      if (spNodeS.contains(obj)) {
        println("    DEBUG distance obj " + obj.obj.id + " from node s " + obj.distance)
        obj.distance * -1
      } else {
        println("    DEBUG distance obj " + obj.obj.id + " from node e " + obj.distance)
        obj.distance + edge.length.get
      }

//      val objIdsNodeS = spNodeS.map(_.obj.id)
//      val objIdsNodeE = spNodeE.map(_.obj.id)
//
//      val isObjInNodeS = objIdsNodeS.contains(obj.obj.id)
//      val isObjInNodeE = objIdsNodeE.contains(obj.obj.id)
//
//      if (isObjInNodeS & isObjInNodeE) {
//        val distanceFromNodeS = spNodeS.find(_.obj.id == obj.obj.id).get.distance
//        val distanceFromNodeE = spNodeE.find(_.obj.id == obj.obj.id).get.distance
//
//        if (distanceFromNodeS < distanceFromNodeE) {
//          println("    DEBUG distance obj " + obj.obj.id + " from node s " + obj.distance)
//          obj.distance * -1
//        } else {
//          println("    DEBUG distance obj " + obj.obj.id + " from node e " + obj.distance)
//          obj.distance + edge.length.get
//        }
//      } else if (isObjInNodeS) {
//        println("    DEBUG distance obj " + obj.obj.id + " from node s " + obj.distance)
//        obj.distance * -1
//      } else {
//        println("    DEBUG distance obj " + obj.obj.id + " from node e " + obj.distance)
//        obj.distance + edge.length.get
//      }
    }
  }

  def determineLMid(objL: NodeObject, objLMid: NodeObject, edge: EdgeGrid, ll: LandmarkLeft, lr: LandmarkRight, spNodeS: Set[NodeObject], spNodeE: Set[NodeObject]): Option[Landmark] = {
    val objLdistance = findDistance(objL, edge, spNodeS, spNodeE)
    val objLMidDistance = findDistance(objLMid, edge, spNodeS, spNodeE)

    val objLId = objL.obj.id
    val objLMidId = objLMid.obj.id

    val distanceBetween = (objLdistance + objLMidDistance) / 2
    println("      distance between obj1 " + objLId + " obj2 " + objLMidId + " is " + distanceBetween)

    if (distanceBetween >= 0 & distanceBetween <= edge.length.get) {
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

  def determineLMidObjects(sp: Set[NodeObject], objL: NodeObject): Set[NodeObject] = {
    sp.filter(o => {
      val tupleObj = createEntryTuples(objL.obj)
      val tupleObj_ = createEntryTuples(o.obj)
      val tree = RTree().insertAll(tupleObj).insertAll(tupleObj_)

      val bbox = expandDdr(objL.obj.bbox)
      val objProb = getDominationProbability(tree, bbox, o.obj.id)

//      println("      obj "+ objL.obj.id +" obj " + o.obj.id + " objProb " + objProb)
      if (objProb > 1 - P_THRESHOLD)
        true
      else
        false
    })
  }

  def findInitialSPIds(sortedLandmarks: Seq[Landmark]): Seq[Int] = {
    val initialSP = mutable.Stack[Int]()
    val tempLandmarkLeftId = mutable.Stack[Int]()

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

  def processLandmark(landmarks: List[Landmark], objects: Set[NodeObject], edge: EdgeGrid): Unit = {
    val sortedLandmarks = landmarks.toSeq.sortBy(_.distance)

    var SP = findInitialSPIds(sortedLandmarks).map(objId => objects.find(_.obj.id == objId).get).toSet

    val filteredLandmarks = sortedLandmarks.filterNot(_.distance < 0)
    val queue = scala.collection.mutable.Queue[Landmark](filteredLandmarks: _*)

    var dStart: Double = 0
    var dEnd: Double = edge.length.get

    var turningPointList = Vector[TP]()

    def isObjectInSP(SP: Set[NodeObject], objId: Int): Boolean = {
      SP.exists(_.obj.id == objId)
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
            val obj = objects.find(_.obj.id == l.objId).get

            turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP + obj
          }
        case _: LandmarkRight =>
          if (isObjectInSP(SP, l.objId)) {
            val obj = SP.find(_.obj.id == l.objId).get

            turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP - obj
          }
        case landmark: LandmarkLeftMid =>

          if (isObjectInSP(SP, landmark.objId)
            & isObjectInSP(SP, landmark.objDominatedId)) {
            val objDominated = SP.find(_.obj.id == landmark.objDominatedId).get

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
            val objDominated = objects.find(_.obj.id == landmark.objDominatedId).get

            turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

            dStart = l.distance
            SP = SP + objDominated
          }
      }
    }

    turningPointList = turningPointList :+ TP(dStart, dEnd, SP)

    println("      Total Turning Points:")
    turningPointList.foreach(t => {
      println("        Start: " + t.dStart + "\t End: " + t.dEnd + "\t SP: " + t.SP.map(_.obj.id).toString())
    })
  }
}

