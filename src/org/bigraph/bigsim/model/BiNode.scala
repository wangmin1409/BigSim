package org.bigraph.bigsim.model

/**
 * @author amy
 */
class BiNode(b: Bigraph, childPair: Map[Int, Int]) {
  var bigraph: Bigraph = b;
  
  var hash: Int = {//根据当前偶图的root生成唯一的hashCode
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }
  
}