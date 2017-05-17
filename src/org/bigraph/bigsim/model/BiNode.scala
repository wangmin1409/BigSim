package org.bigraph.bigsim.model

/**
 * @author amy
 */

object BiNode{
    def GetHead: BiNode = {
    var res: BiNode = null;
    return res;
  }
  
}

class TSPair{
  var T: ReactionRule; //trans
  var S: BiNode;  //state
  var pid: String; //this trans belongs which process
}
class BiNode(b: Bigraph, childPair: Map[Int, Int]) {
  var bigraph: Bigraph = b;
  var childPair: Map[Int, Int]
  var hash: Int = {//根据当前偶图的root生成唯一的hashCode
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }
  
  def GetChild: List[TSPair] = {
    return null;
  }
  
  
  
}