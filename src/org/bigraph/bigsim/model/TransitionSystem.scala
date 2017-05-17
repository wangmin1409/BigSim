package org.bigraph.bigsim.model

import scala.collection.mutable.Map

/**
 * @author amy
 */
<<<<<<< HEAD

=======
object TransitionSystem {
  def GetHead: BiNode = {
    var res: BiNode = null;
    return res;
  }
  
  
  
  def GetTransFromNode(parBiNode: BiNode, childBiNode: BiNode): ReactionRule = {
    return null;
  }
}
>>>>>>> 207ff948d72d13445ec5c8a31e93c6dc86abf4ed

class TransitionSystem(init: BiNode) {
  val root: BiNode = init;
  val trans: Map[Int, BiNode] = Map();
  if (root != null) trans(root.hash) = root;
  
  def add(node: BiNode): Unit = {
    trans(node.hash) = node;
  }
  
  
}