package org.bigraph.bigsim

import org.bigraph.bigsim.model.BiNode
import org.bigraph.bigsim.model.ReactionRule
import java.lang.Boolean
import jdk.nashorn.internal.ir.ContinueNode
import org.bigraph.bigsim.model.ReactionRule
import scala.collection.mutable.Set

/*
 *  this mod is for partial order reduction
 *  author: yan wei
 */

object POR {
  
  def DFS(node:BiNode):Unit = {
    Cal_Ample(node)
    node.TSP.keys.foreach { childName => 
        DFS(node.TSP(childName));
    }
  }
  
  def Run()={
    var head = BiNode.head;
    DFS(head);  
  }

  def isDependent(state: BiNode, trans1: ReactionRule, trans2: ReactionRule): Boolean = {
    var children = state.GetEnable
    var flag1: Boolean = false;
    var flag2: Boolean = false;
    var node1: BiNode = null;
    var node2: BiNode = null;
    children.keys.foreach { tran =>
        if (tran == trans1) {
          flag1 = true;
          node1 = state.TSP(tran)
        }
        if (tran == trans2) {
          flag2 = true;
          node2 = state.TSP(tran)
        }
    }
    if (flag1 && flag2) {
      var f = false;
      var ff = false;
      var n: BiNode = null;
      var nn: BiNode = null;
      node1.TSP.keys.foreach { tran =>
          if (tran == trans2) {
            f = true;
            n = node1.TSP(tran);
          }
       }

      node2.TSP.keys.foreach  { tran =>
        if (tran == trans1) {
            ff = true;
            nn = node2.TSP(tran);
          }
      }
      if (f && ff && (n == nn)) {
        return true;
      }
    }
    return false;
  }

  def dep(state:BiNode,in:List[ReactionRule]): Set[ReactionRule] = {
    var D:Set[ReactionRule] = Set(); //与当前变迁依赖的变迁集 
    var allTrans = BiNode.activeTrans;
    in.foreach { trans =>  
          allTrans.foreach { otherTrans => 
                if(trans!=otherTrans){
                  if(!isDependent(state, trans, otherTrans)){
                    D.add(otherTrans)
                  }
                }
          }
    }
    return D;
  }

  def check_c1(state: BiNode, pid: String, transMap: Map[String, List[ReactionRule]]): Boolean = {
    var cur  = transMap(pid)
    var D = dep(state,cur)
    
    transMap.keys.foreach { key =>
      if (key != pid) {
         D.foreach { t => 
           transMap(key).foreach { t1 => 
             if(t==t1)return false;  
           }  
         }       
      }
    }
    
    return true;
  }

  def check_c2(trans: List[ReactionRule]): Boolean = {
    return true;
  }

  def check_c3(trans: List[ReactionRule]): Boolean = {
    return true;
  }

  def Cal_Ample(in: BiNode): Unit = {
    if (in == null) return ;
    var transMap: Map[String, List[ReactionRule]] = Map();
    
    in.GetEnable.keys.foreach { trans => 
       if(transMap.contains(trans.pName)){
          var l = transMap(trans.pName)
          l:+ trans;
       }  
       else{
         var l:List[ReactionRule] = List()
         l:+trans
         transMap += (trans.pName->l)
       }
    }

    transMap.keys.foreach { key =>
      if (transMap(key) != null) {
        if (check_c1(in, key, transMap) && check_c2(transMap(key)) && check_c3(transMap(key))) {
          in.isTotalExpansion = false;
          in.SetAmple(transMap(key))
          return ;
        }
      }
    }
  }
}