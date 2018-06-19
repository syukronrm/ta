package ta.naive_approach

import ta.geometry.{Point2d, Rect2d}
import ta.Constants._
import ta.algorithm.TheAlgorithm.{SkyPrX, getDominationProbability}
import collection.spatial.RTree
import com.rits.cloning.Cloner
import scalax.collection.immutable.Graph
import scalax.collection.edge.WLkUnDiEdge
import scalax.collection.edge.Implicits._
import ta.{RawEdge, RawNode}
import ta.algorithm.TurningPoint.processLandmark
import ta.landmark._

import scala.collection.JavaConverters._
import scala.collection.mutable
import ta.stream._

case class Edge(id: Int, i: Int, j: Int, length: Double)
case class Node(id: Int, x: Double, y: Double)
//case class RawObject(id: Int, edgeId: Int, position: Double, points: List[Point2d])

case class Object(id: Int, distance: Double, skyProb: Double, points: List[Point2d], edgeId: Int, position: Double)

case class TP(dStart: Double, dEnd: Double, SP: Set[Object])

class TempGraph {
  var graph: Graph[Int, WLkUnDiEdge] = Graph()

  def addNode(i: Int) = {
    graph = graph ++ Graph(i)
  }

  def loadGraph(edges: Set[Edge]): Unit = {
    edges.foreach { e =>
      addEdge(e.i, e.j, e.length, e.id)
    }
  }

  def addEdge(i: Int, j: Int, len: Double, id: Int): Unit = {
    graph = graph + WLkUnDiEdge(i, j)(len, id)
  }

  private def n(g: Graph[Int, WLkUnDiEdge], outer: Int): g.NodeT = g get outer

  def addObject(obj: RawObject, edge: Edge): Graph[Int, WLkUnDiEdge] = {
    val lengthNodei: Double = edge.length * obj.position
    val lengthNodej: Double = edge.length * (1 - obj.position)

    val g1 = graph + WLkUnDiEdge(edge.i, 0)(lengthNodei, edge.id)
    val g2 = g1 + WLkUnDiEdge(edge.j, 0)(lengthNodej, edge.id)

    g2
  }

  def findDistance(obj: RawObject, edge: Edge, nodeId: Int): Option[Double] = {
    val g2 = addObject(obj, edge)

    val sp = n(g2, 0) shortestPathTo n(g2, nodeId)

    sp match {
      case Some(s) => Some(s.weight)
      case None => None
    }
  }

  def findDistance(g: Graph[Int, WLkUnDiEdge], nodeId: Int) = {
    val sp = n(g, 0) shortestPathTo n(g, nodeId)

    sp match {
      case Some(s) => Some(s.weight)
      case None => None
    }
  }

  def getNodesByDistance(g: Graph[Int, WLkUnDiEdge], objId: Int, nodes: Set[Node]) = {
    nodes.par.map { n =>
      n.id -> findDistance(g, n.id)
    }.toList.toMap
  }

  def getDEpsilonNode(g: Graph[Int, WLkUnDiEdge], objId: Int, nodes: Set[Node]) = {
    getNodesByDistance(g, objId, nodes).filter { m =>
      m._2 match {
        case Some(_2) =>
          m._2.get <= D_EPSILON
        case _ =>
          false
      }
    }.map { m =>
      m._1 -> m._2.get
    }
  }
}

class Naive {
  var nodes: Set[Node] = Set()
  var edges: Set[Edge] = Set()
  var objects: Set[RawObject] = Set()

  // Map(nodeId, Map(objectId, (idObject, distance, skyProb)))
  var nodeWithObjects: mutable.Map[Int, mutable.Map[Int, Object]] = mutable.Map.empty[Int, mutable.Map[Int, Object]]

  var graph = new TempGraph()

  def cloneMe() = {
    val cloner = new Cloner()
    cloner.deepClone(this)
  }

  def getEdge(edgeId: Int) = {
    this.edges.par.find(_.id == edgeId)
  }

  def addObject(rawObject: RawObject) = {
    this.objects += rawObject
  }

  def removeObject(id: Int) = {
    val objMaybe = this.objects.find(_.id == id)

    objMaybe match {
      case Some(obj) =>
        this.objects -= obj
      case _ =>
        None
    }
  }

  def addNodes(rawNodes: Set[RawNode]): Unit = {
    rawNodes.foreach { n =>
      graph.addNode(n.id)
      this.nodes += Node(n.id, n.x, n.y)
    }
  }

  def getNode(id: Int) = {
    nodes.find(_.id == id)
  }

  def addRawEdges(rawEdges: Set[RawEdge]) = {
    val es = rawEdges.map(rawEdge => {
      val nodei = getNode(rawEdge.i).get

      val e = rawEdge.lengthMaybe match {
        case Some(length) =>
          Edge(rawEdge.id, rawEdge.i, rawEdge.j, length)
        case None =>
          val nodej = getNode(rawEdge.j).get
          val dx = nodej.x - nodei.x
          val dy = nodej.y - nodei.y
          val length = Math.sqrt(dx*dx + dy*dy)
          Edge(rawEdge.id, rawEdge.i, rawEdge.j, length)
      }

      graph.addEdge(e.i, e.j, e.length, e.id)

      e
    }).toList.toSet

    addEdges(es)
  }

  def addEdges(edges: Set[Edge]): Unit = {
    edges.foreach((e: Edge) => {
      this.edges += e
    })
  }

  def naiveAlgorithm(stream: Stream): Unit = {
    stream match {
      case obj: RawObject =>
        val edgeMaybe = getEdge(obj.edgeId)
        addObject(obj)
        val g = graph.addObject(obj, edgeMaybe.get)
        val nodesEplison = graph.getDEpsilonNode(g, obj.id, this.nodes)

        nodesEplison.foreach { nodeIdAndDistance =>
          val objectMaybe = nodeWithObjects.get(nodeIdAndDistance._1)

          objectMaybe match {
            case Some(nodeObjects) =>
              val newMap = nodeObjects +
                (obj.id -> Object(obj.id, nodeIdAndDistance._2, 0.0, obj.points, obj.edgeId, obj.position))
              nodeWithObjects(nodeIdAndDistance._1) = newMap
            case None =>
              nodeWithObjects +=
                (nodeIdAndDistance._1 -> mutable.Map(obj.id -> Object(obj.id, nodeIdAndDistance._2, 0.0, obj.points, obj.edgeId, obj.position)))
          }
        }

        val affectedEdges = nodesEplison.keys.flatMap { nodeId =>
          this.edges.par.filter(e => e.i == nodeId | e.j == nodeId)
        }

        affectedEdges.foreach { e =>
          val nodeSMaybe = nodeWithObjects.par.find(_._1 == e.i)
          val nodeEMaybe = nodeWithObjects.par.find(_._1 == e.j)

          val objectIdsNodeS = nodeSMaybe match {
            case Some(nodeS) =>
              nodeS._2.keys.toSet
            case None =>
              Set[Int]()
          }

          val objectIdsNodeE = nodeEMaybe match {
            case Some(nodeE) =>
              nodeE._2.keys.toSet
            case None =>
              Set[Int]()
          }

          process(objectIdsNodeS, e.i, objectIdsNodeE, e.j, e)
        }
      case ExpiredObject(id) =>
        val obj = this.objects.find(_.id == id).get

        val edgeMaybe = getEdge(obj.edgeId)

        val g = graph.addObject(obj, edgeMaybe.get)

        val nodesEplison = graph.getDEpsilonNode(g, obj.id, this.nodes)

        nodesEplison.foreach { nodeIdAndDistance =>
          val objectMaybe = nodeWithObjects.get(nodeIdAndDistance._1)

          objectMaybe match {
            case Some(nodeObjects) =>
              val newMap = nodeObjects - obj.id
              nodeWithObjects(nodeIdAndDistance._1) = newMap
            case None =>
              None
          }
        }

        val affectedEdges = nodesEplison.keys.flatMap { nodeId =>
          this.edges.par.filter(e => e.i == nodeId | e.j == nodeId)
        }

        affectedEdges.foreach { e =>
          val nodeSMaybe = nodeWithObjects.par.find(_._1 == e.i)
          val nodeEMaybe = nodeWithObjects.par.find(_._1 == e.j)

          val objectIdsNodeS = nodeSMaybe match {
            case Some(nodeS) =>
              nodeS._2.keys.toSet
            case None =>
              Set[Int]()
          }

          val objectIdsNodeE = nodeEMaybe match {
            case Some(nodeE) =>
              nodeE._2.keys.toSet
            case None =>
              Set[Int]()
          }

          process(objectIdsNodeS, e.i, objectIdsNodeE, e.j, e)
        }

        removeObject(id)
    }
  }

  def findSP(objectIds: Set[Int], nodeId: Int) = {
    val points = objectIds.flatMap { objectId =>
      val a = this.objects.par.find(_.id == objectId)

      a.get.points
    }

    var tree = new RTree(new Point2d.Builder(), 2, 8, RTree.Split.AXIAL)

    points.foreach { p =>
      tree.add(p)
    }

    var spNode: Set[Object] = Set()

    objectIds.foreach { objectId =>
      val skyProb = SkyPrX(tree, objectId)

      if (skyProb >= P_THRESHOLD) {
        val obj = this.nodeWithObjects(nodeId)(objectId)
        spNode += obj
      }
    }

    spNode
  }

  def findSP(edge: Edge) = {
    this.objects.par
      .filter(_.edgeId == edge.id)
      .toList
      .map(o => {
        Object(o.id, edge.length * o.position, 100, o.points, o.edgeId, o.position)
      })
      .toSet
  }

  def process(objectIdsNodeS: Set[Int], nodeSId: Int, objectIdsNodeE: Set[Int], nodeEId: Int, edge: Edge) = {
    val spNodeS = findSP(objectIdsNodeS, nodeSId)
    val spNodeE = findSP(objectIdsNodeE, nodeEId)
    val sEdge = findSP(edge)

    processLandmark(spNodeS, spNodeE, sEdge, edge)
  }

  def processLandmark(spNodeS: Set[Object], spNodeE: Set[Object], Se: Set[Object], edge: Edge) = {
    val findSimilar = (objects: Set[Object], obj: Object) =>
      objects.find(o => o.id == obj.id & o != obj)

    val SP = spNodeS ++ spNodeE ++ Se
    //println("GSP " + SP.map(_.id).toString())
    val Q = mutable.Queue[Landmark]()

    def findDominatedObjects(obj: Object, objects: Set[Object]): Set[Object] = {
      objects.filter(o => {
        val pointsL = obj.points
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

    processTurningPoint(Q.toList, spNodeS, spNodeE, Se, edge)
  }

  def processTurningPoint(landmarks: List[Landmark], spNodeS: Set[Object], spNodeE: Set[Object], sE: Set[Object], edge: Edge): Unit = {
    val sortedLandmarks = landmarks.sortBy(_.distance)
    val objects = spNodeS ++ spNodeE ++ sE

    var SP = spNodeS

    val filteredLandmarks = sortedLandmarks.filterNot(_.distance < 0)
    var queue = scala.collection.mutable.Queue[Landmark](filteredLandmarks: _*)

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

          val landmarkRightMaybe = queue.find { landmark =>
            landmark.isInstanceOf[LandmarkRight] & landmark.objId == l.objId
          }

          if (landmarkRightMaybe.isDefined) {
            queue = queue.filterNot(_ == landmarkRightMaybe.get)
          } else {
            if (!isLandmarkRightMidExist) {
              val obj = objects.find(_.id == l.objId).get

              turningPointList = turningPointList :+ TP(dStart, l.distance, SP)

              dStart = l.distance
              SP = SP + obj
            }
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

//    println("      Total Turning Points " + edge.id + " len " + edge.length + ":")
//    turningPointList.foreach(t => {
//      println("        Start: " + t.dStart + "\t End: " + t.dEnd + "\t SP: " + t.SP.map(_.id).toString())
//    })
  }

  def findDistanceFromNodeS(obj: Object, edge: Edge, spNodeE: Set[Object]): Double = {
    if (obj.edgeId == edge.id) {
      edge.length * obj.position
    } else {
      val objInSpNodeEMaybe = spNodeE.find(_.id == obj.id)
      if (objInSpNodeEMaybe.isDefined) {
        if (objInSpNodeEMaybe.get.distance < obj.distance) {
          // dari arah node E
          obj.distance + edge.length
        } else {
          // dari arah node S
          obj.distance * -1
        }
      } else {
        // cuma ada di node S
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
          // dari arah node S
          obj.distance * -1
        } else {
          // dari arah node E
          obj.distance + edge.length
        }
      } else {
        // cuma di node E
        obj.distance + edge.length
      }
    }
  }

  def findDistance(obj: Object, edge: Edge, spNodeS: Set[Object], spNodeE: Set[Object]): Double = {
    if (obj.edgeId == edge.id) {
      //println("    DEBUG distance obj " + obj.id + " : in edge " + edge.id + " len edge " + edge.length + " position " + obj.position)
      edge.length * obj.position
    } else {
      if (spNodeS.contains(obj)) {
        findDistanceFromNodeS(obj, edge, spNodeE)
        //        //println("    DEBUG distance obj " + obj.id + " from node s " + obj.distance)
        //        obj.distance * -1
      } else {
        findDistanceFromNodeE(obj, edge, spNodeS)
        //println("    DEBUG distance obj " + obj.id + " from node e " + obj.distance)
        //        obj.distance + edge.length
      }
    }
  }
}



