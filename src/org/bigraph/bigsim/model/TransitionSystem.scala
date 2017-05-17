package org.bigraph.bigsim.model

import scala.collection.mutable.Map

/**
 * @author amy
 */
object TransitionSystem {
  def GetHead: BiNode = {
    var res: BiNode = null;
    return res;
  }
  
  def GetChild: List[BiNode] = {
    var res: List[BiNode] = null;
    return res;
  }
  
  def GetTransFromNode(parBiNode: BiNode, childBiNode: BiNode): ReactionRule = {
    return null;
  }
  
  def GetPidFromTrans(rr:Reaction) = {
    return null;
    
  }
}

class TransitionSystem(init: BiNode) {
  val root: BiNode = init;
  val trans: Map[Int, BiNode] = Map();
  if (root != null) trans(root.hash) = root;
  
  def add(node: BiNode): Unit = {
    trans(node.hash) = node;
  }
  
  
}