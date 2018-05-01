import scala.collection.immutable.Set

import math._

// TODO: ADD INSERTED_STREAM
class GridIndex() {
  var GRID_WIDTH = 5
  var GRID_HEIGHT = 5

  var edges: Set[Edge] = Set()
  var nodes: Set[Node] = Set()
  var uncertainDatas: Set[UncertainObject] = Set()

  def isObjectExist(obj: UncertainObject): Boolean = this.uncertainDatas.contains(obj)
  def getObject(objectId: Int): Option[UncertainObject] = this.uncertainDatas.find(u => u.id == objectId)
  def addObject(obj: UncertainObject): Unit = this.uncertainDatas = this.uncertainDatas + obj
  def removeObject(objectId: Int): Unit =
    this.uncertainDatas = this.uncertainDatas.filterNot((n: UncertainObject) => n.id == objectId)

  def addNode(node: Node): Unit = this.nodes = this.nodes + node
  def addNodes(nodes: Set[Node]): Unit = this.nodes = this.nodes ++ nodes

  def addEdge(edge: Edge): Unit = {
    val nodei: Int = edge.nodei

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

  def isNodeExist(node: Node): Boolean = this.nodes.contains(node)
  def isNodeExist(nodeId: Int): Boolean = {
    val res = this.nodes.find((n: Node) => n.id == nodeId)

    res match {
      case None => false
      case Some(_) => true
    }
  }

  def calculateEdgesLengthAndGrid(): Unit = {
    this.edges = this.edges.map(fillLengthAndGridLocation)
  }

  def findNodeById(nodeId: Int): Option[Node] = this.nodes.find((n: Node) => n.id == nodeId)
  def findEdgeById(edgeId: Int): Option[Edge] = this.edges.find((e: Edge) => e.id == edgeId)

  def fillLengthAndGridLocation(edge: Edge): Edge = {
    val nodei = this.findNodeById(edge.nodei)
    val nodej = this.findNodeById(edge.nodej)

    val Node(_, x1, y1, _, _) = nodei.get
    val Node(_, x2, y2, _, _) = nodej.get

    val length = getLength(x1, y1, x2, y2)
    val g = getGridLocation(nodei.get)

    Edge(edge.id, edge.nodei, edge.nodej, Some(length), Some(g))
  }

  def getGridLocation(node: Node): GridLocation = {
    val gx = math.floor(node.x / GRID_WIDTH).toInt
    val gy = math.floor(node.y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def getLength(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val height = y2 - y1
    val width = x2 - x1

    math.sqrt(height * height + width * width)
  }

  def getNodeIds(edges: Set[Edge]): Set[Int] =
    edges.foldLeft(Set(): Set[Int]) {(acc, i) =>  acc + i.nodei + i.nodej}

  def getNodes(grid: GridIndex, gx: Int, gy: Int): Set[Node] = {
    grid.nodes.filter((n: Node) => {
      (round(floor(n.x / grid.GRID_WIDTH)) == gx) & (round(floor(n.y / grid.GRID_HEIGHT)) == gy)
    })
  }

  def getEdgesFromNodes(grid: GridIndex, nodes: Set[Node]): Set[Edge] = {
    val nodeIds = nodes.map((n: Node) => n.id)

    grid.edges.filter((e: Edge) => {
      nodeIds.contains(e.nodei) | nodeIds.contains(e.nodej)
    })
  }

  def getNodesFromEdges(grid: GridIndex, edges: Set[Edge]): Set[Node] = {
    val nodeIds = edges.flatMap((e: Edge) => Set(e.nodei, e.nodej))

    grid.nodes.filter((n: Node) => nodeIds.contains(n.id))
  }

  def getGridEdges(grid: GridIndex, gridLoc: GridLocation): EdgesNodes = {
    val _nodes = getNodes(grid, gridLoc.x, gridLoc.y)
    val edges = getEdgesFromNodes(grid, _nodes)
    val nodes = getNodesFromEdges(grid, edges)

    EdgesNodes(edges, nodes)
  }

  //  def getGridContent(gx: Int, gy: Int): GridIndex =
}
