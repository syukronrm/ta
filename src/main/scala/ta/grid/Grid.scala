package ta.grid


import collection.spatial.{HyperPoint, RTree, RectBuilder}

import scala.collection.immutable.Set
import ta.stream.RawObject
import ta.Constants._
import ta.{RawEdge, RawNode}
import ta.geometry.{Point2d, Point3d}
import TheTree._

import scala.collection.parallel.ParSet
import scala.math.{floor, round}

case class EdgesNodes(edges: Set[Edge], nodes: Set[Node])

class Grid {
  private var edges: Set[Edge] = Set()
  private var nodes: Set[Node] = Set()
  private var rawObjects: Set[RawObject] = Set()
  private var objects: Set[Object] = Set()

  // objects
  def getObject(id: Int): Option[Object] = objects.find(_.id == id)
  def addObject(obj: Object): Unit = this.objects = this.objects + obj
  def removeObject(id: Int): Unit = {
    val obj = this.objects.find(_.id == id).get

    this.objects = this.objects - obj
  }

  // raw object
  def getRawObject(objectId: Int): Option[RawObject] = rawObjects.find(_.id == objectId)
  def addRawObject(rawObject: RawObject): Unit =
    this.rawObjects = this.rawObjects + rawObject

  // node
  def addRawNodes(nodes: Set[RawNode]): Unit = {
    nodes.map(raw => {
      val emptyTree = createTree2D()
      Node(raw.id, raw.x, raw.y, emptyTree, Set())
    }).foreach(addNode)
  }

  def getNode(nodeId: Int): Option[Node] = this.nodes.find(_.id == nodeId)
  def addNode(node: Node): Unit = this.nodes = this.nodes + node
  def addNodes(nodes: Set[Node]): Unit = this.nodes = this.nodes ++ nodes
  def updateNode(node: Node): Unit = {
    val currentNode = this.nodes.find(_.id == node.id).get
    currentNode.tree = node.tree
    currentNode.objects = node.objects
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
  def getNodes(g: GridLocation): Set[Node] = {
    this.nodes.filter((n: Node) => {
      (round(floor(n.x / GRID_WIDTH)) == g.x) & (round(floor(n.y / GRID_HEIGHT)) == g.y)
    })
  }

  /** Find all nodes connected to edges */
  def getNodes(edges: Set[Edge]): Set[Node] = {
    val nodeIds = edges.flatMap((e: Edge) => Set(e.i, e.j))

    this.nodes.filter((n: Node) => nodeIds.contains(n.id))
  }

  def addRawEdges(edges: Set[RawEdge]): Unit = {
    edges.par.foreach(rawEdge => {
      val nodei = getNode(rawEdge.i).get
      val g = getGridLocation(nodei)

      val newEdge = rawEdge.lengthMaybe match {
        case Some(length) =>
          Edge(rawEdge.id, rawEdge.i, rawEdge.j, length, g, Set())
        case None =>
          val nodej = getNode(rawEdge.j).get
          val dx = nodej.x - nodei.x
          val dy = nodej.y - nodei.y
          val length = Math.sqrt(dx*dx + dy*dy)
          Edge(rawEdge.id, rawEdge.i, rawEdge.j, length, g, Set())
      }

      addEdge(newEdge)
    })
  }

  // edge
  def getEdge(edgeId: Int): Option[Edge] = this.edges.find(_.id == edgeId)

  def addEdge(edge: Edge): Unit = {
    val nodei: Int = edge.i

    if (!isNodeExist(nodei)) {
      val edgeId = edge.id
      throw new Error("addEdge "+ edgeId +" : Node "+ nodei +" tidak tersedia")
    }

    this.edges = this.edges + edge
  }

  def addEdges(edges: Set[Edge]): Unit = {
    edges.foreach((e: Edge) => {
      addEdge(e)
    })
  }

  def addObjectToEdge(rawObject: RawObject): Unit = {
    val edge = this.edges.find(_.id == rawObject.edgeId).get
    val newObject = Object(rawObject.id, rawObject.edgeId, 100, isImpossible = false, edge.i, rawObject.points, 0, rawObject.position)
    addObjectToEdge(newObject)
  }

  /** Insert object to edge.
    * Get edge by edgeId in class Object
    *
    * @param obj object to be inserted
    */
  def addObjectToEdge(obj: Object): Unit = {
    val e = this.edges.find(_.id == obj.edgeId).get
    val newEdge = Edge(e.id, e.i, e.j, e.length, e.g, e.objects + obj)

    this.edges = this.edges - e + newEdge
  }


  /** Remove object from edge by object.
    *
    * @param obj object should be exist
    */
  def removeObjectFromEdge(obj: Object): Unit = {
    val e = this.edges.find(_.id == obj.edgeId).get

    this.edges = this.edges - e + Edge(e.id, e.i, e.j, e.length, e.g, e.objects - obj)
  }


  /** Remove object from edge by object ID
    *
    * @param objectId object to be removed
    */
  def removeObjectFromEdge(objectId: Int): Unit = {
    val o = this.rawObjects.find(_.id == objectId).get
    val e = this.edges.find(_.id == o.edgeId).get
    val deletedObject = e.objects.find(_.id == objectId).get

    this.edges = this.edges - e + Edge(e.id, e.i, e.j, e.length, e.g, e.objects - deletedObject)
  }

  /** Find all edges connected to nodes
    *
    *  @param nodes list of nodes
    *  @return Edge instance, all edges connected to nodes
    */
  def getEdges(nodes: Set[Node]): Set[Edge] = {
    val nodeIds = nodes.map((n: Node) => n.id)

    this.edges.filter((e: Edge) => {
      nodeIds.contains(e.i) | nodeIds.contains(e.j)
    })
  }

  /** Create new GridLocation object based on x
    * and y coordinate of node
    *
    * @param node node
    * @return a GridLocation instance
    */
  def getGridLocation(node: Node): GridLocation = {
    val gx = math.floor(node.x / GRID_WIDTH).toInt
    val gy = math.floor(node.y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  /** Create new GridLocation object based on x
    * and y coordinate of node
    *
    * @param x x coordinate
    * @param y y coordinate
    * @return a GridLocation instance in form of gx: Int
    *         and gy: Int
    */
  def getGridLocation(x: Double, y: Double): GridLocation = {
    val gx = math.floor(x / GRID_WIDTH).toInt
    val gy = math.floor(y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  /** Get all nodes and edges connected to grid
    *
    * @param g location of grid
    * @return Set[Edge] and Set[Node] in form of List
    */
  def getDataGrid(g: GridLocation): EdgesNodes = {
    val _nodes = getNodes(g)
    val edges = getEdges(_nodes)
    val nodes = getNodes(edges)

    EdgesNodes(edges, nodes)
  }
}
