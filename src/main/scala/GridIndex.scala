import scala.collection.immutable.Set

import math._

// TODO: ADD INSERTED_STREAM
class GridIndex() {
  var GRID_WIDTH = 5
  var GRID_HEIGHT = 5

  var edges: Set[EdgeGrid] = Set()
  var nodes: Set[NodeGrid] = Set()
  var uncertainDatas: Set[UncertainObject] = Set()

  def updateNode(nodeGrid: NodeGrid): Unit = {
    val removedNode = this.nodes.find(_.id == nodeGrid.id).get

    this.nodes = this.nodes - removedNode + nodeGrid
  }

  def isObjectExist(obj: UncertainObject): Boolean = this.uncertainDatas.contains(obj)
  def getObject(objectId: Int): Option[UncertainObject] = this.uncertainDatas.find(u => u.id == objectId)
  def addObject(obj: UncertainObject): Unit =
    this.uncertainDatas = this.uncertainDatas + obj

  def addObjectToEdge(obj: UncertainObject): Unit = {
    val e = this.edges.find(_.id == obj.id).get
    val newEdge = EdgeGrid(e.id, e.nodei, e.nodej, e.length, e.g, e.objects + obj)

    this.edges = this.edges - e + newEdge
  }

  def getEdgesFromNodeId(nodeId: Int): Set[EdgeGrid] = {
    this.edges.filter(e => e.nodei == nodeId | e.nodej == nodeId)
  }

  def removeObjectFromEdge(objectId: Int): Unit = {
    val o = this.uncertainDatas.find(_.id == objectId).get
    val e = this.edges.find(_.id == o.edgeId).get

    this.edges = this.edges - e + EdgeGrid(e.id, e.nodei, e.nodej, e.length, e.g, e.objects - o)
  }

  def removeObject(objectId: Int): Unit = {
    val o = this.uncertainDatas.find(_.id == objectId).get

    this.uncertainDatas = this.uncertainDatas - o
  }

  def getEdgeIdByObjectId(objectId: Int): Int =
    this.uncertainDatas.find(_.id == objectId).get.edgeId

  def updateNodes(nodes: Set[NodeGrid]): Unit = {
    val updatedNodeIds = nodes.map(_.id)

    this.nodes =
      this.nodes
        .filterNot(n => {
          updatedNodeIds.contains(n.id)
        })
        .++(nodes)
  }

  def addNode(node: NodeGrid): Unit = this.nodes = this.nodes + node
  def addNodes(nodes: Set[NodeGrid]): Unit = this.nodes = this.nodes ++ nodes

  def addEdge(edge: EdgeGrid): Unit = {
    val nodei: Int = edge.nodei

    if (!isNodeExist(nodei)) {
      val edgeId = edge.id
      throw new Error("addEdge $edgeId: Node $nodei tidak tersedia")
    }

    this.edges = this.edges + edge
  }
  def addEdges(edges: Set[EdgeGrid]): Unit = {
    edges.foreach((e: EdgeGrid) => {
      addEdge(e)
    })
  }

  def isNodeExist(node: NodeGrid): Boolean = this.nodes.contains(node)
  def isNodeExist(nodeId: Int): Boolean = {
    val res = this.nodes.find((n: NodeGrid) => n.id == nodeId)

    res match {
      case None => false
      case Some(_) => true
    }
  }

  def calculateEdgesLengthAndGrid(): Unit = {
    this.edges = this.edges.map(fillLengthAndGridLocation)
  }

  def findNodeById(nodeId: Int): Option[NodeGrid] = this.nodes.find((n: NodeGrid) => n.id == nodeId)
  def findEdgeById(edgeId: Int): Option[EdgeGrid] = this.edges.find((e: EdgeGrid) => e.id == edgeId)

  def fillLengthAndGridLocation(edge: EdgeGrid): EdgeGrid = {
    val nodei = this.findNodeById(edge.nodei)
    val nodej = this.findNodeById(edge.nodej)

    val NodeGrid(_, x1, y1, _, _) = nodei.get
    val NodeGrid(_, x2, y2, _, _) = nodej.get

    val length = getLength(x1, y1, x2, y2)
    val g = getGridLocation(nodei.get)

    EdgeGrid(edge.id, edge.nodei, edge.nodej, Some(length), Some(g), Set())
  }

  def getGridLocation(node: NodeGrid): GridLocation = {
    val gx = math.floor(node.x / GRID_WIDTH).toInt
    val gy = math.floor(node.y / GRID_HEIGHT).toInt

    GridLocation(gx, gy)
  }

  def getLength(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    val height = y2 - y1
    val width = x2 - x1

    math.sqrt(height * height + width * width)
  }

  def getNodeIds(edges: Set[EdgeGrid]): Set[Int] =
    edges.foldLeft(Set(): Set[Int]) {(acc, i) =>  acc + i.nodei + i.nodej}

  def getNodes(grid: GridIndex, gx: Int, gy: Int): Set[NodeGrid] = {
    grid.nodes.filter((n: NodeGrid) => {
      (round(floor(n.x / grid.GRID_WIDTH)) == gx) & (round(floor(n.y / grid.GRID_HEIGHT)) == gy)
    })
  }

  def getEdgesFromNodes(grid: GridIndex, nodes: Set[NodeGrid]): Set[EdgeGrid] = {
    val nodeIds = nodes.map((n: NodeGrid) => n.id)

    grid.edges.filter((e: EdgeGrid) => {
      nodeIds.contains(e.nodei) | nodeIds.contains(e.nodej)
    })
  }

  def getNodesFromEdges(grid: GridIndex, edges: Set[EdgeGrid]): Set[NodeGrid] = {
    val nodeIds = edges.flatMap((e: EdgeGrid) => Set(e.nodei, e.nodej))

    grid.nodes.filter((n: NodeGrid) => nodeIds.contains(n.id))
  }

  def getGridEdges(grid: GridIndex, gridLoc: GridLocation): EdgesNodes = {
    val _nodes = getNodes(grid, gridLoc.x, gridLoc.y)
    val edges = getEdgesFromNodes(grid, _nodes)
    val nodes = getNodesFromEdges(grid, edges)

    EdgesNodes(edges, nodes)
  }

  //  def getGridContent(gx: Int, gy: Int): GridIndex =
}
