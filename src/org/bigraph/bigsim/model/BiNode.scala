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
  
  var allBiNodes: ListBuffer[BiNode] = ListBuffer();
  
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
  var activeTrans: Set[ReactionRule] = Set()
  
  def printAllBiNode() = {
    println("BiNode的数量" + allBiNodes.size + "\n");
    allBiNodes.foreach { x => 
      println(x.toString())}
  }
}


class BiNode(b: Bigraph, ff: Map[ReactionRule, BiNode]){
  var bigraph: Bigraph = b;
  var childList: ListBuffer[BiNode] = ListBuffer();
  var TSP: Map[ReactionRule, BiNode] = ff;  //变迁-状态对
  
  var ample:List[ReactionRule] = null;
 
  def addChild(node:BiNode){
    childList.append(node)
  }

  var isTotalExpansion:Boolean = true;   //默认完全展开

  def GetEnable: Map[ReactionRule, BiNode] = {
    return TSP;
  }
  
  def SetAmple(ample:List[ReactionRule]) = {
    this.ample = ample;
  }
  
   def GetAmple: List[ReactionRule] = {
    return ample;
  }
  override def toString = {
    val s: StringBuffer = new StringBuffer();
    s.append("BiNode: \n");
    s.append("当前节点:" + bigraph.root + "\n");
    /*for (key <- TSP.keySet.toArray) {
      s.append("变迁内容：" + key.toString() + "\t");
      s.append("变迁后Bigraph" + TSP.get(key));
    }*/
    s.append("子节点个数:" + childList.size + "\n");
    childList.foreach { x => 
      s.append("\tchild" + x.bigraph.root + "\n"); 
      }
 //   s.append("childList" + childList + "\n");
    //s.append("TSP" + TSP.toList + "\n");
    s.toString();
  }
}



object Test {
 
}
