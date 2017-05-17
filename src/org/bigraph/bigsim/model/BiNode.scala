package org.bigraph.bigsim.model

import com.sun.org.apache.xpath.internal.operations.Bool
import scala.collection.mutable.ListBuffer





/**
 * @author amy
 */

object BiNode{
    def GetHead: BiNode = {
    var res: BiNode = null;
    return res;
  }
    
    
}


class TSPair(t: ReactionRule, s: BiNode, pn: String){
  var T: ReactionRule = t; //trans
  var S: BiNode = s;  //state
  var pid: String = pn; //this trans belongs which process
  
}


class BiNode(b: Bigraph, childTrans: ListBuffer[TSPair]) {
  var bigraph: Bigraph = b;
  var childList: ListBuffer[TSPair] = childTrans;
  var isTotalExpansion:Boolean = true;   //默认完全展开

  var hash: Int = {//根据当前偶图的root生成唯一的hashCode
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }
  
  def addTSElem(ts: TSPair): Unit = {
    childList.append(ts);
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


object Test {
 
}
