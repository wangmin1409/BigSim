package org.bigraph.bigsim

import org.bigraph.bigsim.model.BiNode
import org.bigraph.bigsim.model.ReactionRule
import java.lang.Boolean
import jdk.nashorn.internal.ir.ContinueNode


object POR {
  
  def check_c1(transMap:Map[String,List[BiNode]]):Boolean = {
    return false;
  }
  
  def check_c2(trans:List[BiNode]):Boolean = {
    return false;
  }
  
  def check_c3(trans:List[BiNode]):Boolean = {
    return false;
  }
  
  def Cal_Ample(in:BiNode):Unit = {
    if(in==null)return;
    var transMap:Map[String,List[BiNode]] = Map();
    in.GetEnable.foreach { children => 
      if(transMap.contains(children.pid)){
         var node = transMap(children.pid)
         node:+children
      }
      else{
        var l:List[BiNode] = List()
        l:+children
        transMap+=(children.pid ->l)
      }
    }
    transMap.keys.foreach { key => 
        if(transMap(key)!=null){
           if(check_c1(transMap)&&check_c2(transMap(key))&&check_c3(transMap(key))){
             in.isTotalExpansion = false;
             
             in.SetAmple(transMap(key))
           }
        }
    }
  }
}