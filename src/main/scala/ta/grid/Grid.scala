package ta.grid

import ta.Constants._
import ta.stream.RawObject
import ta.grid.Node
import ta.grid.Edge

import scala.collection.immutable.Set
import ta.Constants._
import ta.RawNode

import scala.math.{floor, round}

class Grid[T] {
  private var edges: Set[Edge] = Set()
  private var nodes: Set[Node[T]] = Set()
  private var rawObjects: Set[RawObject] = Set()

  // raw object
  def getRawObject(objectId: Int): Option[RawObject] = rawObjects.find(_.id == objectId)
  def addRawObject(rawObject: RawObject): Unit =
    this.rawObjects = this.rawObjects + rawObject

  // node
  def addRawNodes(nodes: Set[RawNode]): Unit = {
    nodes.map(raw => Node(raw.id, raw.x, raw.y, ))
  }

  def getNode(nodeId: Int): Option[Node[T]] = this.nodes.find(_.id == nodeId)
  def addNode(node: Node[T]): Unit = this.nodes = this.nodes + node
  def addNodes(nodes: Set[Node[T]]): Unit = this.nodes = this.nodes ++ nodes
  def updateNode(node: Node[T]): Unit = {
    val removedNode = this.nodes.find(_.id == node.id).get

    nodes = this.nodes - removedNode + node
  }
  def isNodeExist(nodeId: Int): Boolean = this.nodes.exists(_.id == nodeId)

  /** Find all nodes inside GridLocation */
  def getNodes(g: GridLocation): Set[Node[T]] = {
    this.nodes.filter((n: Node[T]) => {
      (round(floor(n.x / GRID_WIDTH)) == g.x) & (round(floor(n.y / GRID_HEIGHT)) == g.y)
    })
  }

  /** Find all nodes connected to edges */
  def getNodes(edges: Set[Edge]): Set[Node[T]] = {
    val nodeIds = edges.flatMap((e: Edge) => Set(e.i, e.j))

    this.nodes.filter((n: Node[T]) => nodeIds.contains(n.id))
  }

  // edge
  def getEdget(edgeId: Int): Option[Edge] = this.edges.find(_.id == edgeId)
  def addEdge(edge: Edge): Unit = {
    val nodei: Int = edge.i

    if (!isNodeExist(nodei)) {
      val edgeId = edge.id
      throw new Error("addEdge $edgeId: Node $nodei tidak tersedia")
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
  def getEdges(nodes: Set[Node[T]]): Set[Edge] = {
    val nodeIds = nodes.map((n: Node[T]) => n.id)

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
  def getGridLocation(node: Node[T]): GridLocation = {
    val gx = math.floor(node.x / GRID_WIDTH).toInt
    val gy = math.floor(node.y / GRID_HEIGHT).toInt

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
