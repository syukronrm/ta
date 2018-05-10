import scala.collection.mutable

import HelloWorld._

trait Landmark {
  val distance: Double // Distance to Node S
  val edgeId: Int
  val obj: NodeObject
}

class LandmarkLeft(_distance: Double, _edgeId: Int, _obj: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Int = _edgeId
  override val obj: NodeObject = _obj
}

class LandmarkLeftMid(_distance: Double, _edgeId: Int, _obj: NodeObject, _objDominated: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Int = _edgeId
  override val obj: NodeObject = _obj
  val objDominated: NodeObject = _objDominated
}

class LandmarkRight(_distance: Double, _edgeId: Int, _obj: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Int = _edgeId
  override val obj: NodeObject = _obj
}

class LandmarkRightMid(_distance: Double, _edgeId: Int, _obj: NodeObject, _objDominated: NodeObject) extends Landmark {
  override val distance: Double = _distance
  override val edgeId: Int = _edgeId
  override val obj: NodeObject = _obj
  val objDominated: NodeObject = _objDominated
}

object TurningPoint {
  def processTurningPoint(nodeS: NodeGrid, edge: EdgeGrid, nodeE: NodeGrid): Unit = {
    val spNodeS = nodeS.objects
    val spNodeE = nodeE.objects
    val spEdge = edge.objects

    val findSimilar = (objects: Set[NodeObject], obj: NodeObject) =>
      objects.find(o => o.obj.id == obj.obj.id & o != obj)

    val determineObject = (obj1: NodeObject, obj2: NodeObject, edgeId: Int) => {
      if (obj1.obj.edgeId == edgeId) {
        spNodeS.contains(obj1)
      } else {
        obj1.distance < obj2.distance
      }
    }

    val filterSPs = (spNodeS: Set[NodeObject], spNodeE: Set[NodeObject], edgeId: Int) => {
      val SPs = spNodeS ++ spNodeE

      SPs.filter(o => {
        findSimilar(SPs, o) match {
          case None =>
            true
          case objMaybe =>
            val obj = objMaybe.get
            determineObject(o, obj, edgeId)
        }
      })
    }

    val findDistance = (obj: NodeObject, edge: EdgeGrid, nodeSId: Int, nodeEId: Int) => {
      if (spNodeS.contains(obj) & obj.obj.edgeId == edge.id) {
        println("    DEBUG distance: in edge " + edge.id + " len edge " + edge.length.get + " position " + obj.obj.pos)
        edge.length.get * obj.obj.pos
      } else if (spNodeS.contains(obj)) {
        println("    DEBUG distance from node s " + obj.distance)
        obj.distance * -1
      } else {
        println("    DEBUG distance from node e " + obj.distance)
        obj.distance + edge.length.get
      }
    }

    val SPs = spNodeS ++ spNodeE

    val SP = filterSPs(spNodeS, spNodeE, edge.id)

    val Q = mutable.Queue()

    SP.foreach(o => {
      println("  Object " + o.obj.id)

      val distance = findDistance(o, edge, nodeS.id, nodeE.id)
      val landmarkLeft = new LandmarkLeft(distance, edge.id, o)
      val landmarkRight = new LandmarkRight(distance, edge.id, o)

      println("    distance from node S " + distance)
    })
  }
}
