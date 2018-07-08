package visualize

import java.io.FileWriter

import ta.{Dataset, RawEdge, RawNode}

object Exporter {
  def main(args: Array[String]): Unit = {
    val cal_table_nodes: Set[RawNode] = Dataset.readNodePartial().toSet
    val cal_table_edges: Set[RawEdge] = Dataset.readEdgePartial().toSet

    exportAll(cal_table_edges.toSet, cal_table_nodes.toSet)
  }

  def exportAll(edges: Set[RawEdge], nodes: Set[RawNode]): Unit = {
    exportNodes(nodes)
    exportEdges(edges)
  }

  def exportNodes(nodes: Set[RawNode]): Unit = {
    val fwclear = new FileWriter("export/nodes.txt")
    fwclear.close()
    val fw = new FileWriter("export/nodes.txt", true)
    nodes.foreach { n =>
      fw.write(n.id + ", " + n.x + ", " + n.y + "\n")
    }
    fw.close()
  }

  def exportEdges(edges: Set[RawEdge]): Unit = {
    val fwclear = new FileWriter("export/edges.txt")
    fwclear.close()
    val fw = new FileWriter("export/edges.txt", true)
    edges.foreach { e =>
      fw.write(e.id + ", " + e.i + "," + e.j + "\n")
    }
    fw.close()
  }
}
