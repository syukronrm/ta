import HelloWorld.updateNode

import scala.collection.immutable.Set
import archery._
import scalax.collection.edge.{WLkUnDiEdge, WUnDiEdge}
import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.edge.Implicits._
import scalax.collection.Graph
import scalax.collection.edge.WUnDiEdge

import scala.collection.mutable

object Constants {
  // $COVERAGE-OFF$
  @inline final val D_EPSILON = 10
  @inline final val P_THRESHOLD = 0.5
  // $COVERAGE-ON$
}

import Constants._

import HelloBox._

abstract class UncertainStream {
  def getId: Int
}
case class UncertainObject(id: Int, edgeId: Int, pos: Double, tuples: Set[UTuple], bbox: Box, isPossible: Boolean)
  extends UncertainStream {
  override def getId: Int = id
}
case class StreamDelete(id: Int)
  extends UncertainStream {
  override def getId: Int = id
}

case class UTuple(x: Double, y: Double, p: Double)
case class EntryTuple(n: Int, prob: Double)
case class NodeObject(obj: UncertainObject, skyProb: Double, isImpossible: Boolean)
case class NodeGrid(id: Int, x: Double, y: Double, tree: RTree[EntryTuple], objects: Set[NodeObject])

case class GridLocation(x: Int, y: Int) {}
case class Edge(id: Int, nodei: Int, nodej: Int, length: Option[Double], g: Option[GridLocation], objects: Set[UncertainObject])
case class EdgesNodes(edges: Set[Edge], nodes: Set[NodeGrid])


object HelloBox {
  def expandPdr(bbox: Box): Box = {
    val xMin = bbox.lowerLeft.x
    val yMin = bbox.lowerLeft.y
    val xMax = Float.MaxValue
    val yMax = Float.MaxValue

    Box(xMin, yMin, xMax, yMax)
  }

  def expandDdr(bbox: Box): Box = {
    val xMin = bbox.upperRight.x
    val yMin = bbox.upperRight.y
    val xMax = Float.MaxValue
    val yMax = Float.MaxValue

    Box(xMin, yMin, xMax, yMax)
  }
}

object HelloWorld {
  /**
    * Pretty prints a Scala value similar to its source represention.
    * Particularly useful for case classes.
    * @param a - The value to pretty print.
    * @param indentSize - Number of spaces for each indent.
    * @param maxElementWidth - Largest element size before wrapping.
    * @param depth - Initial depth to pretty print indents.
    * @return
    */
  private def prettyPrint(a: Any, indentSize: Int = 2, maxElementWidth: Int = 30, depth: Int = 0): String = {
    val indent = " " * depth * indentSize
    val fieldIndent = indent + (" " * indentSize)
    val thisDepth = prettyPrint(_: Any, indentSize, maxElementWidth, depth)
    val nextDepth = prettyPrint(_: Any, indentSize, maxElementWidth, depth + 1)
    a match {
      // Make Strings look similar to their literal form.
      case s: String =>
        val replaceMap = Seq(
          "\n" -> "\\n",
          "\r" -> "\\r",
          "\t" -> "\\t",
          "\"" -> "\\\""
        )
        '"' + replaceMap.foldLeft(s) { case (acc, (c, r)) => acc.replace(c, r) } + '"'
      // For an empty Seq just use its normal String representation.
      case xs: Seq[_] if xs.isEmpty => xs.toString()
      case xs: Seq[_] =>
        // If the Seq is not too long, pretty print on one line.
        val resultOneLine = xs.map(nextDepth).toString()
        if (resultOneLine.length <= maxElementWidth) return resultOneLine
        // Otherwise, build it with newlines and proper field indents.
        val result = xs.map(x => s"\n$fieldIndent${nextDepth(x)}").toString()
        result.substring(0, result.length - 1) + "\n" + indent + ")"
      // Product should cover case classes.
      case p: Product =>
        val prefix = p.productPrefix
        // We'll use reflection to get the constructor arg names and values.
        val cls = p.getClass
        val fields = cls.getDeclaredFields.filterNot(_.isSynthetic).map(_.getName)
        val values = p.productIterator.toSeq
        // If we weren't able to match up fields/values, fall back to toString.
        if (fields.length != values.length) return p.toString
        fields.zip(values).toList match {
          // If there are no fields, just use the normal String representation.
          case Nil => p.toString
          // If there is just one field, let's just print it as a wrapper.
          case (_, value) :: Nil => s"$prefix(${thisDepth(value)})"
          // If there is more than one field, build up the field names and values.
          case kvps =>
            val prettyFields = kvps.map { case (k, v) => s"$fieldIndent$k = ${nextDepth(v)}" }
            // If the result is not too long, pretty print on one line.
            val resultOneLine = s"$prefix(${prettyFields.mkString(", ")})"
            if (resultOneLine.length <= maxElementWidth) return resultOneLine
            // Otherwise, build it with newlines and proper field indents.
            s"$prefix(\n${prettyFields.mkString(",\n")}\n$indent)"
        }
      // If we haven't specialized this type, just use its toString.
      case _ => a.toString
    }
  }

  def addEdge(graph: Graph[NodeGrid, WLkUnDiEdge], nodei: NodeGrid, nodej: NodeGrid, weight: Double, key: Int): Graph[NodeGrid, WLkUnDiEdge] = {

    // SAVE ALL EDGES TO

    graph ++ Graph(WLkUnDiEdge(nodei, nodej)(weight, key))
  }

  def addEdges(graph: Graph[NodeGrid, WLkUnDiEdge], nodes: Set[NodeGrid], edges: Set[Edge]): Graph[NodeGrid, WLkUnDiEdge] = {
    edges.foldLeft(graph)((acc, e) => {
      val nodei = nodes.find((n: NodeGrid) => n.id == e.nodei).get
      val nodej = nodes.find((n: NodeGrid) => n.id == e.nodej).get

      val existingNodei = graph.nodes.find(_.value.id == nodei.id)
      val existingNodej = graph.nodes.find(_.value.id == nodej.id)

      val updatedGraphWithNodei = existingNodei match {
        case None =>
          acc
        case _ =>
          val a = updateNode(acc, existingNodei.get.value)
          a
      }

      val updatedGraphWithNodej = existingNodej match {
        case None =>
          updatedGraphWithNodei
        case _ =>
          updateNode(updatedGraphWithNodei, existingNodej.get.value)
      }

      addEdge(updatedGraphWithNodej, nodei, nodej, e.length.get, e.id)

//      addEdge(acc, nodei, nodej, e.length.get, e.id)
    })
  }

  def updateNode(graph: Graph[NodeGrid, WLkUnDiEdge], node: NodeGrid): Graph[NodeGrid, WLkUnDiEdge] = {
    def deletedNode = graph.nodes.filter(_.toOuter.id == node.id).head

    def newEdges = graph.get(deletedNode).edges.map(e => {
      val nodeNeighbor = e.nodes.filterNot(n => n == deletedNode).head.toOuter
      val edgeWeight = e.weight
      val edgeLabel = e.label
      WLkUnDiEdge(node, nodeNeighbor)(edgeWeight, edgeLabel)
    })

    val a = graph - deletedNode ++ newEdges
    a
  }

  def addGraphsFromNodeGrid(graph: Graph[NodeGrid, WLkUnDiEdge], addedGrid: scala.collection.mutable.Set[GridLocation], grid: GridIndex, node: NodeGrid): Graph[NodeGrid, WLkUnDiEdge] = {
    val gridLoc: GridLocation = grid.getGridLocation(node)
    val EdgesNodes(edges, nodes) = grid.getGridEdges(grid, gridLoc)

    addEdges(graph, nodes, edges)
  }

  def n(g: Graph[NodeGrid, WLkUnDiEdge], outer: NodeGrid): g.NodeT = g get outer

  def calculateDistance(graph: Graph[NodeGrid, WLkUnDiEdge], obj: UncertainObject, node: NodeGrid): Double = {
    val fakeNode = NodeGrid(0, 0, 0, RTree(), Set())
    val edgeFakeNode = graph.edges.find(_.label == obj.edgeId).get

    val node1 = edgeFakeNode._1.toOuter
    val lenToNode1 = edgeFakeNode.weight * obj.pos

    val node2 = edgeFakeNode._2.toOuter
    val lenToNode2 = edgeFakeNode.weight * (1 - obj.pos)

    val graphNode1: Graph[NodeGrid, WLkUnDiEdge] = Graph(WLkUnDiEdge(node1, fakeNode)(lenToNode1, 0))
    val graphNode2: Graph[NodeGrid, WLkUnDiEdge] = Graph(WLkUnDiEdge(node2, fakeNode)(lenToNode2, 0))

    val addedGraph = graph ++ graphNode1 ++ graphNode2

    val spO = n(addedGraph, fakeNode) shortestPathTo n(addedGraph, node)

    val a = spO match {
      case None =>
        spO
      case _ => spO
    }

    spO.get.weight
  }

  def findPdrOverlappedObjects(node: NodeGrid, obj: UncertainObject): Set[NodeObject] = {
    val boxStream = obj.bbox
    val tree = node.tree

    val pdrBox = expandPdr(boxStream)
    val overlappedTuples = tree.search(pdrBox)

    val nodeIds = overlappedTuples.map(o => o.value.n).toSet[Int]
    val objects = nodeIds.map(id => node.objects.find(_.obj.id == id).get)

    objects
  }

  def insertToNode(obj: UncertainObject, node: NodeGrid): NodeGrid = {
    val incomingEntries = obj.tuples.map(t =>
      Entry(
        Point(t.x.toFloat, t.y.toFloat),
        EntryTuple(obj.id, t.p)
      )
    )

    // FIND OBJECT OVERLAPPED PDR
    val pdrOverlappedObjects = findPdrOverlappedObjects(node, obj)

    val updatedOverlappedObjects = pdrOverlappedObjects.map(q => {
      val ddrObj = expandDdr(obj.bbox)
      var bboxQ = q.obj.bbox
      if (ddrObj.contains(bboxQ)) {
        NodeObject(q.obj, q.skyProb, isImpossible = true)
      } else {
        val bbox = expandDdr(obj.bbox)

        val objProb = node.tree
          .search(bbox)
          .map(a => {
            a
          })
          .filter(_.value.n == q.obj.id)
          .map(a => {
            a
          })
          .foldLeft(0.0)((acc, e) => acc + e.value.prob)

        if (objProb > (1 - P_THRESHOLD)) {
          NodeObject(q.obj, q.skyProb, isImpossible = true)
        } else {
          // TODO UPDATE SKYPROB q
          val skyProb = SkyPrX(node.tree.insertAll(incomingEntries), q.obj.id)
          NodeObject(q.obj, skyProb, q.isImpossible)
        }
      }
    })

    // INSERT OBJECT TO TREE
    val tree = node.tree.insertAll(incomingEntries)

    val updatedObjects = node.objects.map(o => {
      val updated = updatedOverlappedObjects.find(_.obj.id == o.obj.id)

      if (updated.nonEmpty)
        updated.get
      else
        o
    })

    val skyProbU = SkyPrX(tree, obj.id)
    val finalObjects = updatedObjects + NodeObject(obj, skyProbU, isImpossible = false)
    NodeGrid(node.id, node.x, node.y, tree, finalObjects)
  }

  def deleteFromNode(id: Int, node: NodeGrid): NodeGrid = {
    val obj = node.objects
      .find(_.obj.id == id)
      .get
      .obj

    val deletingEntries = obj.tuples.map(t =>
      Entry(
        Point(t.x.toFloat, t.y.toFloat),
        EntryTuple(obj.id, t.p)
      )
    )

    val pdrOverlappedObjects = findPdrOverlappedObjects(node, obj)

    val treeORemoved = node.tree.removeAll(deletingEntries)

    val updatedOverlappedObjects = pdrOverlappedObjects.map {
      case q@NodeObject(_, _, false) => q
      case q@NodeObject(obj_, _, isImpossible) =>
        val skyPr = SkyPrX(treeORemoved, obj.id)
        NodeObject(obj_, skyPr, isImpossible)
    }

    val objectsORemoved = node.objects.filterNot(_.obj.id == id)

    NodeGrid(node.id, node.x, node.y, treeORemoved, objectsORemoved)
  }

  def theAlgorithm(grid: GridIndex, uncertainData: UncertainStream): GridIndex = {
    var queue: scala.collection.mutable.Queue[Int] = scala.collection.mutable.Queue[Int]()
    var graph: Graph[NodeGrid, WLkUnDiEdge] = Graph()

    var updatedNodes: Set[Int] = Set()
    var visitedNodes: Set[Int] = Set()

    val obj = uncertainData match {
      case uncertainData: UncertainObject =>
        uncertainData.asInstanceOf[UncertainObject]
      case StreamDelete(objectId) =>
        grid.getObject(objectId).get
    }

    uncertainData match {
      case uncertainObject: UncertainObject =>
        grid.addObjectToEdge(uncertainObject)
        grid.addObject(obj)
      case StreamDelete(objectId) =>
        grid.removeObjectFromEdge(objectId)
        grid.removeObject(objectId)
    }

    val addedGrid: mutable.Set[GridLocation] = scala.collection.mutable.Set()

    var edge: Edge = grid.findEdgeById(obj.edgeId).get

    // enqueue
    val nodei = grid.findNodeById(edge.nodei).get
    val nodej = grid.findNodeById(edge.nodej).get

    queue.enqueue(nodei.id)
    queue.enqueue(nodej.id)

    while (queue.nonEmpty) {
      var currentNode = grid.nodes.filter(_.id == queue.dequeue()).head

      graph = addGraphsFromNodeGrid(graph, addedGrid, grid, currentNode)
      currentNode = graph.nodes.filter((x: Graph[NodeGrid, WLkUnDiEdge]#NodeT) => currentNode.id == x.value.id).head.value
      val len = calculateDistance(graph, obj, currentNode)

      if (len < D_EPSILON) {
        var node = uncertainData match {
          case UncertainObject(_, _, _, _, _, _) =>
            insertToNode(uncertainData.asInstanceOf[UncertainObject], currentNode)
          case StreamDelete(id) =>
            deleteFromNode(id, currentNode)
        }

        grid.updateNode(node)
        graph = updateNode(graph, node)
        currentNode = graph.nodes.filter((x: Graph[NodeGrid, WLkUnDiEdge]#NodeT) => currentNode.id == x.value.id).head.value
        updatedNodes = updatedNodes + node.id

//        println(currentNode.toString)
        val neighborNodes = graph.get(currentNode)
          .neighbors
          .map(_.toOuter)

        neighborNodes.foreach(node_ => {
          if (!visitedNodes.contains(node_.id)) {
            graph = addGraphsFromNodeGrid(graph, addedGrid, grid, node_)

            queue.enqueue(node_.id)

            visitedNodes = visitedNodes + node_.id
          } else if (updatedNodes.contains(node_.id)) {
            println("COMPUTE SKYLINE " + node_.toString)
          }
        })
      }
    }

    grid.updateNodes(
      graph.nodes
        .filter((n: Graph[NodeGrid, WLkUnDiEdge]#NodeT) => updatedNodes.contains(n.value.id))
        .map(_.value)
        .toSet
    )

    println(prettyPrint(updatedNodes))
    grid
  }

  def isyNotDominatex(y: Entry[EntryTuple], x: Entry[EntryTuple]): Boolean = {
    val expandedY = Box(y.geom.x, y.geom.y, y.geom.x2, y.geom.y2)

    if (expandDdr(expandedY).contains(x.geom))
      false
    else
      true
  }

  def PrYnotdominatex(Y: List[Entry[EntryTuple]], x: Entry[EntryTuple]): Double = {
    Y.filter(y => isyNotDominatex(y, x))
      .map(a => {
        a
      })
      .foldLeft(0.0)((acc, e) => acc + e.value.prob)
  }

  def SkyPrx(tree: RTree[EntryTuple], X: Set[Entry[EntryTuple]], x: Entry[EntryTuple]): Double = {
    val a = tree.entries
      .toList
      .filterNot(e => X.contains(e))
      .groupBy(_.value.n) // filtered Y Map
      .values
      .map(a => {
        a
      })
      .map(Y => PrYnotdominatex(Y, x))
      .product

    a
  }

  def SkyPrX(tree: RTree[EntryTuple], objectId: Int): Double = {
    val X = tree.entries.filter(_.value.n == objectId).toSet

    X.map(x => {
        val skyPrx = SkyPrx(tree, X, x)
        x.value.prob * skyPrx
      })
      .map(a => {
        a
      })
      .sum
  }

  def main(args: Array[String]): Unit = {
    val table_nodes: Set[NodeGrid] = Set(
      NodeGrid(1, 2, 1, RTree(), Set()),
      NodeGrid(2, 19, 1, RTree(), Set()),
      NodeGrid(3, 3, 3, RTree(), Set()),
      NodeGrid(4, 9, 5, RTree(), Set()),
      NodeGrid(5, 16, 5, RTree(), Set()),
      NodeGrid(6, 3, 8, RTree(), Set()),
      NodeGrid(7, 8, 12, RTree(), Set()),
      NodeGrid(8, 16, 12, RTree(), Set())
    )

    val table_edges: Set[Edge] = Set(
      Edge(1, 1, 2, None, None, Set()),
      Edge(2, 1, 3, None, None, Set()),
      Edge(3, 2, 5, None, None, Set()),
      Edge(4, 3, 4, None, None, Set()),
      Edge(4, 3, 6, None, None, Set()),
      Edge(5, 4, 5, None, None, Set()),
      Edge(6, 4, 7, None, None, Set()),
      Edge(7, 5, 8, None, None, Set()),
      Edge(8, 6, 7, None, None, Set()),
      Edge(9, 7, 8, None, None, Set())
    )

    val streams: Set[UncertainStream] = Set(
      UncertainObject(1, 1, 0.5, Set(UTuple(5, 7, .6), UTuple(4, 5, .1), UTuple(7, 6, .3)), Box(4, 5, 7, 7), isPossible = true),
      UncertainObject(2, 2, 0.5, Set(UTuple(6, 8, .6), UTuple(4, 4, .1), UTuple(7, 6, .3)), Box(4, 4, 7, 8), isPossible = true),
      StreamDelete(1)
//      UncertainObject(3, 2, 0.6, Set(UTuple(5, 6, .4), UTuple(5, 6, .2), UTuple(6, 6, .4)), Box(5, 6, 6, 6), isPossible = true),
//      UncertainObject(4, 3, 0.5, Set(UTuple(1, 3, .2), UTuple(3, 2, .3), UTuple(1, 4, .5)), Box(1, 2, 3, 4), isPossible = true)
    )

    var grid = new GridIndex()

    grid.addNodes(table_nodes)
    grid.addEdges(table_edges)

    grid.calculateEdgesLengthAndGrid()

    streams.foldLeft(streams) {(acc, stream) => {
      theAlgorithm(grid, stream)
      acc
    }}
  }
}
