package org.bigraph.bigsim

import org.bigraph.bigsim.model.BiNode
import org.bigraph.bigsim.model.ReactionRule
import java.lang.Boolean
import jdk.nashorn.internal.ir.ContinueNode


object POR {
  
  def check_c1(transMap:Map[String,List[ReactionRule]]):Boolean = {
    return false;
  }
  
  def check_c2(trans:List[ReactionRule]):Boolean = {
    return false;
  }
  
  def check_c3(trans:List[ReactionRule]):Boolean = {
    return false;
  }
  
  def Cal_Ample(in:BiNode):Unit = {
    if(in==null)return;
    var transMap:Map[String,List[ReactionRule]] = Map();
    in.GetEnable.foreach { children => 
      if(transMap.contains(children.pid)){
         var l:List[ReactionRule] = transMap(children.pid)
         l:+children.T;
      }
      else{
        var l:List[ReactionRule] = List()
        l:+=children.T;
        transMap+=(children.pid ->l)
      }
    }
    transMap.keys.foreach { key => 
        if(transMap(key)!=null){
           if(check_c1(transMap)&&check_c2(transMap(key))&&check_c3(transMap(key))){
             in.isTotalExpansion = false;
            
           }
        }
    }
  }
}