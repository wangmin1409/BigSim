package org.bigraph.bigsim.model

import com.sun.org.apache.xpath.internal.operations.Bool

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
  var T: ReactionRule = null; //trans
  var S: BiNode = null;  //state
  var pid: String = null; //this trans belongs which process
}


class BiNode(b: Bigraph, child: TSPair) {
  var bigraph: Bigraph = b;
  var isTotalExpansion:Boolean = true;   //默认完全展开

  var hash: Int = {//根据当前偶图的root生成唯一的hashCode
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }
  
  def GetEnable: List[TSPair] = {
    return null;
  }
  
  def SetAmple(ample:List[TSPair]) = {
    
  }
  
   def GetAmple: List[TSPair] = {
    return null;
  }

}
