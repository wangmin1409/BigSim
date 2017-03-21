package org.bigraph.bigsim.utils

import scala.collection.mutable.ListBuffer
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex

class PathSet(root: Graph) {

  var nodes: Map[Int, Int] = Map()
  var index: Int = 0
  root.lut.values.map(node => {
    nodes += (node.hash -> index)
    index += 1
  })
  var adjMatrix: Array[Array[Int]] = new Array(nodes.size);
  var nodeCycle: ListBuffer[Vertex] = ListBuffer()
  var hasCycle: Boolean = false
  var cycleList: ListBuffer[ListBuffer[Vertex]] = ListBuffer()
  var paths: ListBuffer[String] = ListBuffer()

  /**
   * init the adj matrix
   */
  def initialAdjMatrix() {
    root.lut.values.map(srcNode => {
      var srcIndex = nodes.getOrElse(srcNode.hash, 0)
      srcNode.target.keys.map(tarNode => {
        var tarIndex = nodes.getOrElse(tarNode.hash, 0)
        adjMatrix(srcIndex)(tarIndex) = 1
      })
    })
  }

  /**
   * find cycle of a node, store into nodeCycle
   */
  def findCycle(v: Vertex) {
    var tempList: ListBuffer[Vertex] = ListBuffer()
    var index: Int = nodeCycle.indexOf(v)
    if (index != -1) {
      hasCycle = true
      while (index < nodeCycle.size) {
        tempList.append(nodeCycle(index))
        index += 1
      }
      if (!(tempList.size > 1 && (nodes.getOrElse(tempList(0).hash, 0) > nodes.getOrElse(tempList(1).hash, 0)))
        && !cycleList.contains(tempList)) {
        cycleList.append(tempList);
      }
      return
    } else {
      nodeCycle.append(v);
    }

    for (k <- 0 to nodes.size - 1) {
      if (adjMatrix(nodes.getOrElse(v.hash, 0))(k) == 1) {
        findCycle(root.lut.getOrElse(nodes.getOrElse(k, 0), null));
      }
    }
    nodeCycle.remove(nodeCycle.size - 1);
  }

  /**
   * remove cycle
   */
  def removeCycle(listOfCycles: ListBuffer[ListBuffer[Vertex]]) {
    listOfCycles.map(cycleList => {
      adjMatrix(nodes.getOrElse(cycleList(cycleList.size - 1).hash, 0))(nodes.getOrElse(cycleList(0).hash, 0)) = 0
    })
  }

  def findPath(start: Vertex, end: Vertex, res: String) {
    start.pathVisited = true;
    for (i <- 0 to nodes.size - 1) {

      if (adjMatrix(nodes.getOrElse(start.hash, 0))(i) == 0
        || root.lut.getOrElse(nodes.getOrElse(i, 0), root.root).pathVisited == true) {
        ;
      } else {

        if (nodes.getOrElse(i, 0) == end.hash) {
          paths.append(res + "-->" + nodes.getOrElse(end.hash, 0))
        }
        var nextNode: Vertex = root.lut.getOrElse(end.hash, null)
        findPath(nextNode, end, res + "-->")

      }

    }
  }

}