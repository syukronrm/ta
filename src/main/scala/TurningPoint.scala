import scala.collection.mutable
import archery._

import HelloWorld._
import HelloBox._
import Constants._

trait Landmark {
  val distance: Double // Distance to Node S
  val edgeId: Option[Int]
  val obj: NodeObject

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "edgeId " + edge + " distance " + distance.toString + " obj " + obj.obj.id
  }
}

class LandmarkLeft(_distance: Double, _edgeId: Option[Int], _obj: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val obj: NodeObject = _obj
}

class LandmarkLeftMid(_distance: Double, _edgeId: Option[Int], _obj: NodeObject, _objDominated: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val obj: NodeObject = _obj
  val objDominated: NodeObject = _objDominated

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "LandmarkLeftMid in edgeId " + edge + " between objId " + obj.obj.id + " and objId' " + objDominated.obj.id + " in location " + distance
  }
}

class LandmarkRight(_distance: Double, _edgeId: Option[Int], _obj: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val obj: NodeObject = _obj
}

class LandmarkRightMid(_distance: Double, _edgeId: Option[Int], _obj: NodeObject, _objDominated: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Option[Int] = _edgeId
  override val obj: NodeObject = _obj
  val objDominated: NodeObject = _objDominated

  override def toString: String = {
    val edge = if (edgeId.isDefined) edgeId.get.toString else "None"
    "LandmarkRightMid in edgeId " + edge + " between objId " + obj.obj.id + " and objId' " + objDominated.obj.id + " in location " + distance
  }
}

object TurningPoint {
  def processTurningPoint(nodeS: NodeGrid, edge: EdgeGrid, nodeE: NodeGrid): Unit = {
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


    def filterSPs(spNodeS: Set[NodeObject], spNodeE: Set[NodeObject], uncertainDataSpEdge: Set[UncertainObject], edge: EdgeGrid) = {
      var SPs = spNodeS ++ spNodeE
      val edgeId = edge.id

      val objIdsOnEdge = uncertainDataSpEdge.map(_.id)
      println("  objectId on edge "+ edgeId + " is " + objIdsOnEdge)

      SPs = SPs.filterNot(o => {
        objIdsOnEdge.contains(o.obj.id)
      })

      val spEdge = convertToNodeObject(uncertainDataSpEdge, edge.length.get)

      SPs = SPs.filter(o => {
        findSimilar(SPs, o) match {
          case None =>
            true
          case objMaybe =>
            val obj = objMaybe.get
            determineObject(o, obj, edgeId)
        }
      })

      SPs = SPs ++ spEdge
      SPs
    }

    val SP = filterSPs(spNodeS, spNodeE, uncertainDataSpEdge, edge)
    val Q = mutable.Queue[Landmark]()

    SP.foreach(objL => {
      println("  Object " + objL.obj.id)

      val distance = findDistance(objL, edge, spNodeS)
      println("    distance from node S " + distance)

      val landmarkLeft = createLandmarkLeft(distance, edge, objL)
      val landmarkRight = createLandmarkRight(distance, edge, objL)
      println("    Landmark Left  : " + landmarkLeft.toString)
      println("    Landmark Right : " + landmarkRight.toString)

      val sp = SP - objL
      val LMidObjects = determineLMidObjects(sp, objL)

      LMidObjects.foreach(objLMid => {
        val landmarkMaybe = determineLMid(objL, objLMid, edge, landmarkLeft, landmarkRight, spNodeS)

        if (landmarkMaybe.isDefined) {
          Q.enqueue(landmarkMaybe.get)
        }
      })
    })

    if (Q.nonEmpty) {
      println("    Total Landmarks: ")
      Q.foreach(l => {
        println("        " + l)
      })
    }
  }

  def createLandmarkLeft(distance: Double, edge: EdgeGrid, objL: NodeObject): LandmarkLeft = {
    val loc = distance - D_EPSILON
    val edgeIdMaybe =
      if (loc >= 0 & loc <= edge.length.get) {
        Some(edge.id)
      } else {
        None
      }

    new LandmarkLeft(loc, edgeIdMaybe, objL)
  }

  def createLandmarkRight(distance: Double, edge: EdgeGrid, objL: NodeObject): LandmarkRight = {
    val loc = distance + D_EPSILON
    val edgeIdMaybe =
      if (loc >= 0 & loc <= edge.length.get) {
        Some(edge.id)
      } else {
        None
      }

    new LandmarkRight(loc, edgeIdMaybe, objL)
  }

  def findDistance(obj: NodeObject, edge: EdgeGrid, spNodeS: Set[NodeObject]): Double = {
    val ids = spNodeS.map(_.obj.id)
    if (obj.obj.edgeId == edge.id) {
      println("    DEBUG distance obj " + obj.obj.id + " : in edge " + edge.id + " len edge " + edge.length.get + " position " + obj.obj.pos)
      edge.length.get * obj.obj.pos
    } else if (ids.contains(obj.obj.id)) {
      println("    DEBUG distance obj " + obj.obj.id + " from node s " + obj.distance)
      obj.distance * -1
    } else {
      println("    DEBUG distance obj " + obj.obj.id + " from node e " + obj.distance)
      obj.distance + edge.length.get
    }
  }

  def determineLMid(objL: NodeObject, objLMid: NodeObject, edge: EdgeGrid, ll: LandmarkLeft, lr: LandmarkRight, spNodeS: Set[NodeObject]): Option[Landmark] = {
    val objLdistance = findDistance(objL, edge, spNodeS)
    val objLMiddistance = findDistance(objLMid, edge, spNodeS)

    val distanceBetween = (objLdistance + objLMiddistance) / 2
    println("      distance between obj1 " + objL.obj.id + " obj2 " + objLMid.obj.id + " is " + distanceBetween)

    if (distanceBetween >= 0 & distanceBetween <= edge.length.get) {
      if (objLdistance > objLMiddistance) {
        println("      obj1 " + objL.obj.id + " distance " + objLdistance)
        println("      obj2 " + objLMid.obj.id + " distance " + objLMiddistance)
        val landmark = new LandmarkRightMid(distanceBetween, Some(edge.id), objL, objLMid)
        Some(landmark)
      } else {
        println("      obj1 " + objLdistance)
        println("      obj2 " + objLMiddistance)
        val landmark = new LandmarkRightMid(distanceBetween, Some(edge.id), objL, objLMid)
        Some(landmark)
      }
    } else {
      None
    }

////    val objLdistance = findDistance(objL, edge, spNodeS)
//    val objLMiddistance = findDistance(objLMid, edge, spNodeS)
//    val ll2 = createLandmarkLeft(objLMiddistance, edge, objLMid)
//    val lr2 = createLandmarkRight(objLMiddistance, edge, objLMid)
//
//    println("      Landmark 1: " + ll + " || " + lr)
//    println("      Landmark 2: " + ll2 + " || " + lr2)
//
//    if (ll.distance < lr2.distance) {
//        val distance = (ll.distance + lr2.distance) / 2
//        if (distance >= 0 & distance < edge.length.get) {
//          val landmark = new LandmarkLeftMid(distance, Some(edge.id), objL, objLMid)
//          println("      Landmark " + landmark)
//          return Some(landmark)
//        }
//    } else if (lr.distance > ll2.distance) {
//        val distance = (lr.distance + ll2.distance) / 2
//        if (distance >= 0 & distance < edge.length.get) {
//          val landmark = new LandmarkRightMid(distance, Some(edge.id), objL, objLMid)
//          println("      Landmark " + landmark)
//          return Some(landmark)
//        }
//    }
//
//    println("      NONE")
//    None
//
//    val distance = (objLMiddistance + objLdistance) / 2
//    println("      objL "+ objL.obj.id +" dist " + objLdistance)
//    println("      objLMid " + objLMid.obj.id + " dist " + objLMiddistance)
//    println("      obj "+ objL.obj.id +" obj "+ objLMid.obj.id +" distance " + distance + " length " + edge.length.get)
//
//    if (distance >= 0 & distance <= edge.length.get) {
//      val edgeId = edge.id
//      if (distance < objLdistance) {
//        Some(new LandmarkLeftMid(distance, Some(edgeId), objL, objLMid))
//      } else {
//        Some(new LandmarkRightMid(distance, Some(edgeId), objL, objLMid))
//      }
//    } else {
//      None
//    }
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
}
