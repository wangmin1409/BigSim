package org.bigraph.bigsim

import org.bigraph.bigsim.model.BiNode
import org.bigraph.bigsim.model.ReactionRule
import java.lang.Boolean
import jdk.nashorn.internal.ir.ContinueNode

/*
 *  this mod is for partial order reduction
 *  author: yan wei
 */

object POR {

  def isDependent(state: BiNode, trans1: ReactionRule, trans2: ReactionRule): Boolean = {
    var children = state.GetEnable
    var flag1: Boolean = false;
    var flag2: Boolean = false;
    var node1: BiNode = null;
    var node2: BiNode = null;
    children.foreach { child =>
      var trans = child.TSP.keys
      trans.foreach { tran =>
        if (tran == trans1) {
          flag1 = true;
          node1 = child;
        }
        if (tran == trans2) {
          flag2 = true;
          node2 = child;
        }
      }

    }
    if (flag1 && flag2) {
      var f = false;
      var ff = false;
      var n: BiNode = null;
      var nn: BiNode = null;
      node1.childList.foreach { child =>
        var trans = child.TSP.keys
        trans.foreach { tran =>

          if (tran == trans2) {
            f = true;
            n = child;
          }
        }

      }
      node2.childList.foreach { child =>
        var trans = child.TSP.keys
        trans.foreach { tran =>
          if (tran == trans1) {
            ff = true;
            nn = child;
          }
        }
      }
      if (f && ff && (n == nn)) {
        return true;
      }
    }
    return false;
  }

  def dep(b1: BiNode, b2: BiNode): Boolean = {
    b1.childList.foreach { child =>
      if (child == b2) {
        if (b2.bigraph == child.bigraph) {
          return true;
        }
      }
    }
    return false;
  }

  def check_c1(state: BiNode, pid: String, transMap: Map[String, List[BiNode]]): Boolean = {
    transMap.keys.foreach { key =>
      if (key != pid) {
        var allTrans = BiNode.activeTrans;
        var D = List(); //与当前变迁依赖的变迁集 
        var ts = transMap(key)
        ts.foreach { node1 =>
          allTrans.foreach { node2 =>
            // if(node1!=node2&&isDependent(state,node1.T,node2)){

            //  }  
          }
        }
      }
    }

    return true;
  }

  def check_c2(trans: List[BiNode]): Boolean = {
    return false;
  }

  def check_c3(trans: List[BiNode]): Boolean = {
    return false;
  }

  def Cal_Ample(in: BiNode): Unit = {
    if (in == null) return ;
    var transMap: Map[String, List[BiNode]] = Map();
    in.GetEnable.foreach { children =>
      var rules = children.TSP.keys
      rules.foreach { rule =>
        if (transMap.contains(rule.pName)) {
          var node = transMap(rule.pName)
          node :+ children
        } else {
          var l: List[BiNode] = List()
          l :+ children
          transMap += (rule.pName -> l)
        }
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