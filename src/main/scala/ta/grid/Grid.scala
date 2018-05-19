package ta.grid


import collection.spatial.{HyperPoint, RTree, RectBuilder}

import scala.collection.immutable.Set
import ta.stream.RawObject
import ta.Constants._
import ta.{RawEdge, RawNode}
import ta.geometry.{Point2d, Point3d}

import scala.math.{floor, round}

class Grid {
  private var edges: Set[Edge] = Set()
  private var nodes: Set[Node[_ <: HyperPoint]] = Set()
  private var rawObjects: Set[RawObject] = Set()

  /** create empty RTree */
  def createNewTree(): RTree[_ <: HyperPoint] = {
    new RTree(new Point2d.Builder, 2, 8, RTree.Split.AXIAL)
    DIMENSION match {
      case 2 =>
        new RTree(new Point2d.Builder(), 2, 8, RTree.Split.AXIAL)
      case 3 =>
        new RTree(new Point3d.Builder(), 2, 8, RTree.Split.AXIAL)
    }
  }

  // raw object
  def getRawObject(objectId: Int): Option[RawObject] = rawObjects.find(_.id == objectId)
  def addRawObject(rawObject: RawObject): Unit =
    this.rawObjects = this.rawObjects + rawObject

  // node
  def addRawNodes(nodes: Set[RawNode]): Unit = {
    val emptyTree = createNewTree()
    nodes.map(raw => Node(raw.id, raw.x, raw.y, emptyTree, Set()))
      .foreach(addNode)
  }

  def getNode(nodeId: Int): Option[Node[_ <: HyperPoint]] = this.nodes.find(_.id == nodeId)
  def addNode(node: Node[_ <: HyperPoint]): Unit = this.nodes = this.nodes + node
  def addNodes(nodes: Set[Node[_ <: HyperPoint]]): Unit = this.nodes = this.nodes ++ nodes
  def updateNode(node: Node[_ <: HyperPoint]): Unit = {
    val removedNode = this.nodes.find(_.id == node.id).get

    nodes = this.nodes - removedNode + node
  }
  def isNodeExist(nodeId: Int): Boolean = this.nodes.exists(_.id == nodeId)

  /** Find all nodes inside GridLocation */
  def getNodes(g: GridLocation): Set[Node[_ <: HyperPoint]] = {
    this.nodes.filter((n: Node[_ <: HyperPoint]) => {
      (round(floor(n.x / GRID_WIDTH)) == g.x) & (round(floor(n.y / GRID_HEIGHT)) == g.y)
    })
  }

  /** Find all nodes connected to edges */
  def getNodes(edges: Set[Edge]): Set[Node[_ <: HyperPoint]] = {
    val nodeIds = edges.flatMap((e: Edge) => Set(e.i, e.j))

    this.nodes.filter((n: Node[_ <: HyperPoint]) => nodeIds.contains(n.id))
  }

  def addRawEdges(edges: Set[RawEdge]): Unit = {
    edges.foreach(rawEdge => {
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
      throw new Error("addEdge $edgeId : Node $nodei tidak tersedia")
    }

    this.edges = this.edges + edge
  }

  def addEdges(edges: Set[Edge]): Unit = {
    edges.foreach((e: Edge) => {
      addEdge(e)
    })
  }

  /** Find all edges connected to nodes
    *
    *  @param nodes list of nodes
    *  @return Edge instance, all edges connected to nodes
    */
  def getEdges(nodes: Set[Node[_ <: HyperPoint]]): Set[Edge] = {
    val nodeIds = nodes.map((n: Node[_ <: HyperPoint]) => n.id)

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
  def getGridLocation(node: Node[_ <: HyperPoint]): GridLocation = {
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
  def getGridLocation(x: Int, y: Int): GridLocation = {
    val gx = math.floor(x / GRID_WIDTH).toInt
    val gy = math.floor(y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  /** Get all nodes and edges connected to grid
    *
    * @param g location of grid
    * @return Set[Edge] and Set[Node] in form of List
    */
  def getDataGrid(g: GridLocation): List[Any] = {
    val _nodes = getNodes(g)
    val edges = getEdges(_nodes)
    val nodes = getNodes(edges)

    List(edges, nodes)
  }
}
