package org.bigraph.bigsim.strategy

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.bigsim._
import org.bigraph.bigsim.parser.TermParser
import org.bigraph.bigsim.model._

/**
 * @author libaijie
 */
object PatternFlow {
  var HoleResult: Set[Hole] = Set()
  var patternDefRules: Set[String] = Set() //反应规则名字
  var patternCUseRules: Set[String] = Set()
  var patternPUseRules: Set[String] = Set()

  //比较两个Prefix结构是否相同
  def comparePrefix(preA: Prefix, preB: Prefix): Boolean = {
    println("preA.node.ports:" + preA.node.ports)
    println("preB.node.ports:" + preB.node.ports)
    println("checkEqual:" + preA.node.ports.equals(preB.node.ports))
    //    if (preA.size != preB.size || preA.node.ctrl.name != preB.node.ctrl.name || !preA.node.ports.equals(preB.node.ports)) {
    if (preA.size != preB.size || preA.node.ctrl.name != preB.node.ctrl.name || preA.node.ports.intersect(preB.node.ports).size != preA.node.ports.size) {
      println("intersect: " + preA.node.ports.intersect(preB.node.ports))
      false
    } else if (preA.suffix.termType == TermType.TNIL && preB.suffix.termType == TermType.TNIL) {
      true
    } else if (preA.suffix.termType != TermType.TNIL && preB.suffix.termType == TermType.TNIL) {
      false
    } else if (preA.suffix.termType == TermType.TNIL && preB.suffix.termType != TermType.TNIL) {
      true
    } else if (preA.suffix.termType == TermType.THOLE && preB.suffix.termType == TermType.THOLE) {
      if (preA.suffix.asInstanceOf[Hole].index == preB.suffix.asInstanceOf[Hole].index) { //同一个Site才判定为相等，比如$0和$0相同
        println("preA.suffix.asInstanceOf[Hole].index:" + preA.suffix.asInstanceOf[Hole].index + " preB.suffix.asInstanceOf[Hole].index:" + preB.suffix.asInstanceOf[Hole].index)
        return true
      } else {
        false
      }
    } else if (preA.suffix.termType != TermType.THOLE && preB.suffix.termType == TermType.THOLE) {
      false
    } else if (preA.suffix.termType == TermType.THOLE && preB.suffix.termType != TermType.THOLE) {
      false
    } else if (preA.suffix.termType == TermType.TPREF && preB.suffix.termType == TermType.TPREF) {
      comparePrefix(preA.suffix.asInstanceOf[Prefix], preB.suffix.asInstanceOf[Prefix]) //两个Prefix的Suffix递归调用本方法比较
    } else {
      false
    }
  }

  def mergeResults(result1: Set[Node], result2: Set[Node]): Set[Node] = {
    result2.map(it => {
      result1 += it
    })
    result1
  }

  //获得所有节点的父节点，以及是否有子孩子
  def getElementofRule(term: Term): Set[Node] = {
    var result: Set[Node] = Set()
    if (term.termType == TermType.THOLE) { //顶层Hole
      HoleResult += term.asInstanceOf[Hole]
    } else if (term.termType == TermType.TPAR) { //Paraller结构
      var paraResult: Set[Term] = term.asInstanceOf[Paraller].getChildren;
      paraResult.map(it => {
        result = mergeResults(result, getElementofRule(it))
      })
    } else if (term.termType == TermType.TREGION) { //Regions结构
      var children = term.asInstanceOf[Regions].getChildren;
      children.map(it => {
        result = mergeResults(result, getElementofRule(it))
      })
    } else if (term.termType == TermType.TPREF) { //Prefix结构
      if (term.asInstanceOf[Prefix].suffix.termType == TermType.TNIL) {
        result += term.asInstanceOf[Prefix].node //没有child的node
      } else if (term.asInstanceOf[Prefix].suffix.termType == TermType.THOLE) {
        var hole: Hole = term.asInstanceOf[Prefix].suffix.asInstanceOf[Hole]
        hole.parNode = term.asInstanceOf[Prefix].node
        HoleResult += hole
        //        term.asInstanceOf[Prefix].node.hasChild = true //里面有Hole的node不算有孩子
        result += term.asInstanceOf[Prefix].node
      } else {
        term.asInstanceOf[Prefix].node.hasChild = true
        result += term.asInstanceOf[Prefix].node
        var tempResult: Set[Node] = getElementofRule(term.asInstanceOf[Prefix].suffix)
        tempResult.map(it => {
          if (it.parent == null) it.parent = term.asInstanceOf[Prefix].node
        })
        result = mergeResults(result, tempResult)
      }
    }
    result;
  }

  //获得所有独立子结构
  def getIndependentEleOfTerm(term: Term): Set[Term] = {
    var nodes: Set[Node] = PatternFlow.getElementofRule(term)
    var resultStr: Set[String] = Set()
    var result: Set[Term] = Set()
    nodes.map(it => {
      if (it.hasChild) {
        nodes.remove(it)
      }
    })
    println("place1: " + nodes)
    nodes.map(it => { //只包含叶子结点，从这点开始找到顶层节点构成一个独立子结构
      var temp: Node = it
      var tempNodeStr: String = getNodeString(temp)
      HoleResult.map { x => if (x.parNode == temp) tempNodeStr = tempNodeStr + "." + x.toString() }

      while (temp.parent != null) {
        tempNodeStr = getNodeString(temp.parent) + "." + tempNodeStr
        temp = temp.parent
      }
      resultStr.+=(tempNodeStr)
    })

    HoleResult.map { x => if (x.parNode == null) resultStr.+=(x.toString()) } //顶层Hole
    println("place2: " + resultStr)
    resultStr.map { rr =>
      {
        var res = TermParser.apply(rr)
        result.+=(res)
      }
    }
    return result;
  }

  def getNodeString(node: Node): String = {
    var portsList: List[String] = List();
    if (node.name.length() == 0 && node.ports.size == 0) {
      return node.ctrl.name
    } else if (node.name.length() == 0 && node.ports.size > 0) {

      node.ports.map { x =>
        {
          println(x.name + ":" + x.nameType);
          if ("idle".equals(x.nameType)) portsList = portsList.+:("idle")
          else portsList = portsList.+:(x.name + ":" + x.nameType)
        }
      }
      return node.ctrl.name + "[" + portsList.mkString(",") + "]";
    } else if (node.name.length() > 0 && node.ports.size == 0) {
      return node.name + ":" + node.ctrl.name
    } else {
      node.ports.map { x =>
        if ("idle".equals(x.nameType)) portsList = portsList.+:("idle")
        else portsList = portsList.+:(x.name + ":" + x.nameType)
      }
      return node.name + ":" + node.ctrl.name + "[" + portsList.mkString(",") + "]";
    }
  }

  //设置rule的puses，cuses，defTerm
  def setRuleDCPs(rule: ReactionRule) = {
    var ruleRedex: Term = rule.redex
    var ruleReactum: Term = rule.reactum

    var redexEleSet: Set[Term] = getIndependentEleOfTerm(ruleRedex)
    var reactumEleSet: Set[Term] = getIndependentEleOfTerm(ruleReactum)

    println("The redex elements are: " + redexEleSet)
    println("The reactum elements are: " + reactumEleSet)

    redexEleSet.map(ite => {
      var contains: Boolean = false
      reactumEleSet.map(it => {
        //反应规则前后没有动，仅作为反应的上下文条件的叫做puse  
        if (ite.termType == TermType.THOLE && it.termType == TermType.THOLE) { //反应前后不变的site认为是puse的部分
          if (ite.asInstanceOf[Hole].index == it.asInstanceOf[Hole].index) {
            rule.puses += ite
            contains = true
          }
        }
        if (ite.termType != TermType.THOLE && it.termType != TermType.THOLE) {
          if (comparePrefix(ite.asInstanceOf[Prefix], it.asInstanceOf[Prefix])) {
            rule.puses += ite
            contains = true
          }
        }

      })
      //如果在redex中出现但是在reactum中消失叫做cuse
      if (!contains) {
        rule.cuses += ite
      }
    })

    reactumEleSet.map(ite => {
      var contains: Boolean = false
      redexEleSet.map(it => {
        if (ite.termType == TermType.THOLE && it.termType == TermType.THOLE) { //puse
          if (ite.asInstanceOf[Hole].index == it.asInstanceOf[Hole].index) {
            contains = true
          }
        }
        if (ite.termType != TermType.THOLE && it.termType != TermType.THOLE) { //puse
          if (comparePrefix(ite.asInstanceOf[Prefix], it.asInstanceOf[Prefix])) {
            contains = true
          }
        }
      })
      //如果在reactum中出但是在redex中没有出现叫做def
      if (!contains) {
        rule.defTerm += ite
      }

    })
  }

  def setPatternDURules(b: Bigraph) = { //Pattern Flow功能入口函数
    var pattern = b.pattern

    b.rules.map(it => {
      setRuleDCPs(it) //设置所有规则的def、puse、cuse
      println("pattern: " + pattern)
      println("defTerm: " + it.defTerm + "it.puses: " + it.puses + "it.cuses: " + it.cuses)
      if (pattern.termType == TermType.TPREF) {
        println("pattern: " + pattern)
        println("it.defTerm: " + it.defTerm)
        println("it.puses: " + it.puses)
        println("it.cuses: " + it.cuses)
        it.defTerm.map { x =>
          if (x.termType == TermType.TPREF && comparePrefix(x.asInstanceOf[Prefix], pattern.asInstanceOf[Prefix])) {
            PatternFlow.patternDefRules.+=(it.name)
          }
        }
        it.puses.map { x =>
          if (x.termType == TermType.TPREF && comparePrefix(x.asInstanceOf[Prefix], pattern.asInstanceOf[Prefix])) {
            PatternFlow.patternPUseRules.+=(it.name)
          }
        }
        it.cuses.map { x =>
          if (x.termType == TermType.TPREF && comparePrefix(x.asInstanceOf[Prefix], pattern.asInstanceOf[Prefix])) {
            PatternFlow.patternCUseRules.+=(it.name)
          }
        }
      }
    })
    println("PatternFlow.patternDefRules: " + PatternFlow.patternDefRules)
    println("PatternFlow.patternPUseRules: " + PatternFlow.patternPUseRules)
    println("PatternFlow.patternCUseRules: " + PatternFlow.patternCUseRules)
  }

}

object TestPatternFlow {
  def main(args: Array[String]) {
    var ss1 = "Application.(GUI.$1 | SelectGoods2:Workflow.(w3a1:Activation[w3f1:edge,idle,idle] | w3a2:Activation[w3f2:edge,w3f1:edge,idle] | w3a3:Activation[idle,w3f2:edge,idle])) | S4:Service[idle,idle,idle,idle] | S5:Service[idle,idle,idle,idle] | S6:Service[idle,idle,idle,idle] | $0"
    var ss2 = "A.(GUI.$1 | B.(w3a1:C.(N|F.(G|Y)) | D | M)) | S4:Service[idle,idle,idle,idle] || S5:Service[idle,idle,idle,idle].(V|P.(Q|W)|O) | S6:Service[idle,idle,idle,idle] | $0"
    var tt = TermParser.apply(ss1)
    var nodeResult: Set[Node] = PatternFlow.getElementofRule(tt);
    var holeResult: Set[Hole] = PatternFlow.HoleResult
    println("nodesize: " + nodeResult.size);
    println("holesize: " + holeResult.size);
    nodeResult.map(it => {
      println("this node: " + it.toString)
      println("this node: " + PatternFlow.getNodeString(it))
      if (it.parent != null) {
        println("parent node: " + it.parent.toString)
      }
      println("hasChild: " + it.hasChild)
    })

    holeResult.map(it => {
      println("this hole: " + it.toString())
      if (it.parNode != null) {
        println("parent node: " + it.parNode.toString())
      }
    })

    var results: Set[Term] = PatternFlow.getIndependentEleOfTerm(tt)
    println("Independent Elements: " + results)
  }
}