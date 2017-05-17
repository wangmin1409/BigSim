package org.bigraph.bigsim.model

import com.sun.org.apache.xpath.internal.operations.Bool
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Set
import scala.collection.mutable.Map



/**
 * @author amy
 */

object BiNode{
  
  var head:BiNode = null;
  
  var allBiNodes: ListBuffer[BiNode] = null;
  
  def addBiNode(b: BiNode): Unit = {
    allBiNodes.append(b);
  }
  
  var curNode:BiNode = null;
  
  def isContains(b: Bigraph): Boolean = {
    allBiNodes.foreach { x => 
      if((x.bigraph.root).toString().equals(b.root.toString())){
        curNode = x;
         return true;
      }
    }
    curNode = null;
    return false;
  }
  var activeTrans: Set[ReactionRule] = null;
}


class BiNode(b: Bigraph, ff: Map[ReactionRule, BiNode]){
  var bigraph: Bigraph = b;
  var childList: ListBuffer[BiNode] = ListBuffer();
  var TSP: Map[ReactionRule, BiNode] = ff;
  def addChild(node:BiNode){
    childList.append(node)
  }

  var isTotalExpansion:Boolean = true;   //默认完全展开

  def GetEnable: List[BiNode] = {
    return null;
  }
  
  def SetAmple(ample:List[BiNode]) = {
    
  }
  
   def GetAmple: List[BiNode] = {
    return null;
  }
}



object Test {
 
}
