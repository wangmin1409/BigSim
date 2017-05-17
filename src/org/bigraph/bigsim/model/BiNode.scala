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

class BiNode(b: Bigraph, rule:ReactionRule,name:String) {
  var bigraph: Bigraph = b;
  var T: ReactionRule = rule; //rules happened
  var pid: String = name;   //rules belong
  var childList: ListBuffer[BiNode] = ListBuffer();
  
  def addChild(node:BiNode){
    childList.append(node)
  }

  var isTotalExpansion:Boolean = true;   //默认完全展开

  var hash: Int = {//根据当前偶图的root生成唯一的hashCode
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }
  
  def addTSElem(ts: BiNode): Unit = {
    childList.append(ts);
  }
  
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
