package ta.grid

import java.io.FileWriter

import scala.collection.immutable.Set
import ta.stream.RawObject
import ta.{RawEdge, RawNode}
import collection.spatial.RTree
import ta.algorithm.TheAlgorithm.SkyPrX
import ta.grid.Rect._
import ta.Constants._
import ta.geometry.Point2d

import scala.math.{floor, round}

case class EdgesNodes(edges: Set[Edge], nodes: Set[Node])
case class Table(edges: Set[Int], nodes: Set[Int])

class Grid extends Cloneable {
  private var edges: Set[Edge] = Set()
  var nodes: Set[Node] = Set()
  private var rawObjects: Set[RawObject] = Set()

  def createDataNaive = {
    val filename = "n"+N_OBJECTS+"np"+N_POINTS+"g"+N_GRID_CELL+"d"+PERCENT_DISTANCE+"p"+P_THRESHOLD+"data"+KIND_OF_DATA+"dim"+DIMENSION
    val fwclear = new FileWriter("import/grid-obj-"+filename+".txt")
    fwclear.close()
    rawObjects.foreach { ro =>
      /*
        id edgeId pos sizePoints
        foreach sizePoints
          px py pp po
      */
      val fw = new FileWriter("import/grid-obj-"+filename+".txt", true)
      fw.write(
        ro.id + " " + ro.edgeId + " " + ro.position + " " + ro.points.size + " "
      )
      var strp = ""
      ro.points.foreach { p =>
        strp += p.x + " " + p.y + " " + p.p + " " + p.o + " "
      }
      fw.write(strp + "\n")
      fw.close()
    }

    val fw2clear = new FileWriter("import/grid-node-"+filename+".txt")
    fw2clear.close()
    nodes.foreach { n =>
      val fw2 = new FileWriter("import/grid-node-"+filename+".txt", true)
      /*
        nodeID objectSize
        foreach objectSize
          id distance skyprob edgeId pos
       */
      fw2.write(
        n.id + " " + n.objects.size + " "
      )
      var str = ""
      n.objects.foreach { o =>
        str += o.id + " " + o.distance + " " + o.skyProb + " " + o.edgeId + " " + o.position + " "
      }
      fw2.write(str + "\n")
      fw2.close()
    }
  }

  def updateAllSkyProb(): Unit = {
    this.nodes = this.nodes.par.map { node =>
      println(node.id)
      node.objects = node.objects.map { o =>
        if (o.isImpossible) {
          o
        } else {
          o.skyProb = SkyPrX(node.tree, o.id)
          o
        }
      }

      node
    }.toList.toSet
  }

  var tableGrid: Map[Int, Map[Int, Table]] = Map()

  def getRawObject(objectId: Int): Option[RawObject] = rawObjects.par.find(_.id == objectId)
  def addRawObject(rawObject: RawObject): Unit =
    this.rawObjects = this.rawObjects + rawObject
  def removeRawObject(objectId: Int): Unit = {
    val rawObject = this.rawObjects.par.find(_.id == objectId).get
    this.rawObjects = this.rawObjects - rawObject
  }

  def addRawNodes(nodes: Set[RawNode]): Unit = {
    nodes.map(raw => {
      val emptyTree = new RTree(new Point2d.Builder(), 2, 8, RTree.Split.AXIAL)
      Node(raw.id, raw.x, raw.y, emptyTree, Set())
    }).foreach(addNode)
  }

  def getNode(nodeId: Int): Option[Node] = this.nodes.par.find(_.id == nodeId)
  def addNode(node: Node): Unit = this.nodes = this.nodes + node
  def addNodes(nodes: Set[Node]): Unit = this.nodes = this.nodes ++ nodes
  def updateNode(node: Node): Unit = {
    this.synchronized {
      val currentNode = this.nodes.par.find(_.id == node.id).get
      currentNode.tree = node.tree
      currentNode.objects = node.objects
    }
  }

  def updateNodes(nodes: Set[Node]): Unit = {
    val updatedNodeIds = nodes.map(_.id)

    this.nodes =
      this.nodes
        .filterNot(n => {
          updatedNodeIds.contains(n.id)
        })
        .++(nodes)
  }

  def isNodeExist(nodeId: Int): Boolean = this.nodes.exists(_.id == nodeId)

  /** Find all nodes inside GridLocation */
  def getNodes(g: GridLocation): List[Node] = {
    this.nodes.par.filter((n: Node) => {
      (round(floor(n.x / GRID_WIDTH)) == g.x) & (round(floor(n.y / GRID_HEIGHT)) == g.y)
    }).toList
  }

  def getNodesFromId(nodeIds: Set[Int]): List[Node] = {
    this.nodes.par.filter((n: Node) => nodeIds.contains(n.id)).toList
  }

  /** Find all nodes connected to edges */
  def getNodes(edges: List[Edge]): List[Node] = {
    val nodeIds = edges.flatMap((e: Edge) => Set(e.i, e.j))

    this.nodes.par.filter((n: Node) => nodeIds.contains(n.id)).toList
  }

  def addRawEdges(edges: Set[RawEdge]): Unit = {
    val es = edges.par.map(rawEdge => {
      val nodei = getNode(rawEdge.i).get

      rawEdge.lengthMaybe match {
        case Some(length) =>
          Edge(rawEdge.id, rawEdge.i, rawEdge.j, length, Set())
        case None =>
          val nodej = getNode(rawEdge.j).get
          val dx = nodej.x - nodei.x
          val dy = nodej.y - nodei.y
          val length = Math.sqrt(dx*dx + dy*dy)
          Edge(rawEdge.id, rawEdge.i, rawEdge.j, length, Set())
      }
    }).toList.toSet

    addEdges(es)
  }

  // edge
  def getEdge(edgeId: Int): Option[Edge] = this.edges.par.find(_.id == edgeId)

  def addEdge(edge: Edge): Unit = {
    this.edges = this.edges + edge
  }

  def addEdges(edges: Set[Edge]): Unit = {
    edges.foreach((e: Edge) => {
      addEdge(e)
    })
  }

  def addObjectToEdge(rawObject: RawObject): Unit = {
    val edgeMaybe = this.edges.par.find(_.id == rawObject.edgeId)

    if (edgeMaybe.isEmpty) {
      println(rawObject)
    }

    val edge = edgeMaybe.get

    val newObject = Object(
      rawObject.id,
      rawObject.edgeId,
      100,
      isImpossible = false,
      edge.i,
      createRect(rawObject.points),
      rawObject.position * edge.length,
      rawObject.position)

    addObjectToEdge(newObject)
  }

  def addObjectToEdge(obj: Object): Unit = {
    val e = this.edges.par.find(_.id == obj.edgeId).get
    val newEdge = Edge(e.id, e.i, e.j, e.length, e.objects + obj)

    this.edges = this.edges - e + newEdge
  }

  def removeObjectFromEdge(obj: Object): Unit = {
    val e = this.edges.par.find(_.id == obj.edgeId).get

    this.edges = this.edges - e + Edge(e.id, e.i, e.j, e.length, e.objects - obj)
  }

  def removeObjectFromEdge(objectId: Int): Unit = {
    val o = this.rawObjects.par.find(_.id == objectId).get
    val e = this.edges.par.find(_.id == o.edgeId).get
    val deletedObject = e.objects.find(_.id == objectId).get

    this.edges = this.edges - e + Edge(e.id, e.i, e.j, e.length, e.objects - deletedObject)
  }

  def getEdges(nodes: List[Node]): List[Edge] = {
    val nodeIds = nodes.map((n: Node) => n.id)

    this.edges.par.filter((e: Edge) => {
      nodeIds.contains(e.i) | nodeIds.contains(e.j)
    }).toList
  }

  def getGridLocation(node: Node): GridLocation = {
    val gx = math.floor(node.x / GRID_WIDTH).toInt
    val gy = math.floor(node.y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def getGridLocation(x: Double, y: Double): GridLocation = {
    val gx = math.floor(x / GRID_WIDTH).toInt
    val gy = math.floor(y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def getDataGrid(g: GridLocation): EdgesNodes = {
    val _nodes = getNodes(g)
    val _nodeIds = _nodes.map(_.id)
    val edges = getEdges(_nodes)
    val nodeIds = edges
      .flatMap(e => Set(e.i, e.j))
      .filterNot(nid => _nodeIds.contains(nid))
      .toSet

    val nodes = getNodesFromId(nodeIds).toSet

    EdgesNodes(edges.toSet, _nodes.toSet ++ nodes)
  }
}
