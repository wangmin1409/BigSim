package org.bigraph.bigsim
import scala.collection.mutable.Map
import scala.collection.mutable.Stack
import java.lang.Boolean
import scala.collection.mutable.Set;

/*
 * author: yanwei
 * this algorithm is for find the strong connected component in graph
 */
class TarJanAlgorithm(graph:Set[GraphNode]) {
  var g = graph;
  var time:Int = 0;
  var dfn:Map[GraphNode,Int] = Map(); 
  var low:Map[GraphNode,Int] = Map(); 
  var visited:Map[GraphNode,Boolean] = Map(); 
  var inStack:Map[GraphNode,Boolean] = Map(); 
  var stack:Stack[GraphNode] = Stack();
  
  def run() = {
    init();
    g.foreach { node => 
      if(!visited(node)){
        tarjan(node);
      } 
    }
  }
  
  def init() = {
    g.foreach { node =>
      dfn += node -> 0;
      low += node -> 0;
      visited += node -> false;
      inStack += node -> false;
    }
  }
  
  def tarjan(curNode:GraphNode):Unit = {
    time = time+1;
    dfn(curNode) = time;
    low(curNode) = time;
    stack.push(curNode);
    visited(curNode) = true;
    inStack(curNode) = true;
    curNode.next.foreach { linkedNode => 
        if(!visited(linkedNode)){
          tarjan(linkedNode);
          if(low(curNode)>low(linkedNode)){
            low(curNode) = low(linkedNode);
          }
        }
        else{
          if(dfn(linkedNode)<low(curNode)){
            low(curNode) = dfn(linkedNode);
          }
        }
    }
    
    if(dfn(curNode) ==low (curNode)){
      var n:GraphNode = null;
      do{
        n = stack.pop();
        inStack(n) = false;
        println("TarJan:------>"+n.s.verifyID+" " +n.q.verifyID);
      }while(n!=curNode);
        println("TarJan:------>end");
    } 
  }
}