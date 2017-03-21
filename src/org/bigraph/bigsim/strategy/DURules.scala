package org.bigraph.bigsim.strategy

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.bigsim._
import org.bigraph.bigsim.parser.TermParser
import org.bigraph.bigsim.model._

/**
 * 对rule进行分析，找出其def、cuse、puse的子结构。
 * 分析中，需要针对每一条rule的redex以及reactum, 拆分成节点之间只包含内嵌关系的独立的结构。
 * 然后再对比从redex以及reactum中的子结构，确定哪些是def、cuse、puse
 */
object ParseRules {

  def comparePrefix(preA: Prefix, preB: Prefix): Boolean = {//比较两个Prefix结构是否相同
//    println("preA.size:"+preA.size+",preB.size:"+preB.size+"| preA.node.ctrl.name:"+preA.node.ctrl.name+",preB.node.ctrl.name:"+preB.node.ctrl.name)
//    println("preA.node.ports:"+preA.node.ports+" preB.node.ports:"+preB.node.ports)
    println("checkEqual:"+preA.node.ports.equals(preB.node.ports))
    if (preA.size != preB.size || preA.node.ctrl.name != preB.node.ctrl.name || !preA.node.ports.equals(preB.node.ports)) {//add by lbj 不仅control要相同，还要判断port是否相同
      false
    } else if (preA.suffix.termType == TermType.TNIL && preB.suffix.termType == TermType.TNIL) {
      true
    } else if (preA.suffix.termType != TermType.TNIL && preB.suffix.termType == TermType.TNIL) {
      false
    } else if (preA.suffix.termType == TermType.TNIL && preB.suffix.termType != TermType.TNIL) {
      true
    } else if (preA.suffix.termType == TermType.THOLE && preB.suffix.termType == TermType.THOLE) {
      if (preA.suffix.asInstanceOf[Hole].index == preB.suffix.asInstanceOf[Hole].index) {//同一个Site才判定为相等，比如$0和$0相同
        println("preA.suffix.asInstanceOf[Hole].index:"+preA.suffix.asInstanceOf[Hole].index+" preB.suffix.asInstanceOf[Hole].index:"+preB.suffix.asInstanceOf[Hole].index)
        return true
      }else{
      false}
    } else if (preA.suffix.termType != TermType.THOLE && preB.suffix.termType == TermType.THOLE) {
      false
    } else if (preA.suffix.termType == TermType.THOLE && preB.suffix.termType != TermType.THOLE) {
      false
    } else if (preA.suffix.termType == TermType.TPREF && preB.suffix.termType == TermType.TPREF) {
      //println(preA.suffix.termType.toString + preA.suffix)
      //println(preB.suffix.termType.toString + preB.suffix)
      comparePrefix(preA.suffix.asInstanceOf[Prefix], preB.suffix.asInstanceOf[Prefix])//两个Prefix的Suffix递归调用本方法比较
    } else {
      false
    }
  }

  def setRuleDCPuses(rule: ReactionRule) = {//主要方法
    var ruleRedex: Term = rule.redex
    var ruleReactum: Term = rule.reactum

    //获得独立子结构，集合里都是Prefix和Hole
    var redexEleSet: Set[Term] = getIndependentEleOfTerm(ruleRedex)
    var reactumEleSet: Set[Term] = getIndependentEleOfTerm(ruleReactum)

    //    println("The redexEleSet eles are: " + redexEleSet)
    //    println("The reactumEleSet eles are: " + reactumEleSet)

    redexEleSet.map(ite => {
      //TODO 确定prefix相等需要方法
      var contains: Boolean = false
      reactumEleSet.map(it => {//ite是redex，it是reactum
        if (ite.termType == TermType.THOLE && it.termType == TermType.THOLE) {
          if (ite.asInstanceOf[Hole].index == it.asInstanceOf[Hole].index) {//都为Site且标号相等
            rule.puses += ite
            contains = true
          }
        }
        if (ite.termType != TermType.THOLE && it.termType != TermType.THOLE) {
          if (comparePrefix(ite.asInstanceOf[Prefix], it.asInstanceOf[Prefix])) {//都为Prefix且比较后相等
            rule.puses += ite
            contains = true
          }
        }

      })
      if (!contains) {//ite有it没有
        rule.cuses+=ite
      }
    })

    reactumEleSet.map(ite => {
      var contains: Boolean = false
      redexEleSet.map(it => {
        if (ite.termType == TermType.THOLE && it.termType == TermType.THOLE) {
          if (ite.asInstanceOf[Hole].index == it.asInstanceOf[Hole].index) {
            contains = true
          }
        }
        if (ite.termType != TermType.THOLE && it.termType != TermType.THOLE) {
          if (comparePrefix(ite.asInstanceOf[Prefix], it.asInstanceOf[Prefix])) {
            contains = true
          }
        }
      })
      if (!contains) {
        rule.defTerm+=ite
      }

    })
  }

  /*
   * 现在先跑通两层嵌套的关系，复杂的后面再考虑，例如：A[x].(B[y] | $0) | C[z]
   * 暂时不考虑，A[x].(B[y].(C | D) | $0)这种的问题
   * 现在在一个Term里，并列的暂时只有prefix 与$0 这两种类型。
   * 这里使用map，在下一步查找def等的时候会方便。
   */
  def getIndependentEleOfTerm(term: Term): Set[Term] = {//拆分成独立子结构，只能正确处理到第二层，用魏鑫的方法
    var result: Set[Term] = Set()
    var regionTerms: Set[Term] = Set()

    var paraEleSet: Set[Term] = Set()

    if (term.termType == TermType.TREGION) {
      term.asInstanceOf[Regions].getChildren.map(it => {
        regionTerms += it
      })
    } else {
      regionTerms += term
    }

    regionTerms.map(it => {
      var paraEle: Set[Term] = getParaElementsOfTerm(it)
      paraEleSet ++= paraEle
    })

    paraEleSet.map(ite => {//经过getParaElementsOfTerm返回的只有Prefix和Hole
      if (ite.termType == TermType.THOLE) {
        result += ite
      } else {
        getElementsOfPrefix(ite.asInstanceOf[Prefix]).map(it => {//返回Prefix结构
          result += it
        })
      }
    })

    result
  }

  /**
   * Input:A[x].(B[y] | $0) | C[z]
   * Output:Set(A[x].($0|B[y].nil), C[z].nil)
   */
  def getParaElementsOfTerm(term: Term): Set[Term] = {//构造父子关系
    var subTerms: Set[Term] = Set()

    if (term.termType == TermType.TPAR) {//Paraller还要再分
      var paraEleSet: Set[Term] = getElementsOfParaller(term.asInstanceOf[Paraller])
      subTerms ++= paraEleSet
      //      println("now the subTerms is: " + subTerms)

    } else if (term.termType == TermType.TPREF || term.termType == TermType.THOLE) {//Prefix和Hole直接返回
      subTerms += term
    }
    subTerms
  }

  def getElementsOfParaller(paraller: Paraller): Set[Term] = {//返回Paraller的首层子节点
    var topLevelParElements: Set[Term] = paraller.getChildren
    //    println("The para set is: " + topLevelParElements)
    topLevelParElements
  }

  //对于prefix类型的，要考虑多层嵌套的问题。例如A.(B.(C | D) | E)
  def getElementsOfPrefix(prefix: Prefix): Set[Term] = {

    var paraElements: Set[Term] = Set()
    if (prefix.suffix.termType == TermType.TNIL || prefix.suffix.termType == TermType.THOLE) {//Nil或Hole是最里层叶节点，出口条件，到C.nil这种结构
      paraElements += prefix.asInstanceOf[Term]
    } else if (prefix.suffix.termType == TermType.TPREF) {
      var sufixEleSet: Set[Term] = getElementsOfPrefix(prefix.suffix.asInstanceOf[Prefix])//若为Prefix就递归调用本函数
      sufixEleSet.map(ite => {
        paraElements += new Prefix(prefix.node, ite).asInstanceOf[Term]
      })
    } else if (prefix.suffix.termType == TermType.TPAR) { //这种情况还需要多加考虑，比较困难,暂时未考虑三层嵌套
      var suffixParaElementsSet: Set[Term] = prefix.suffix.asInstanceOf[Paraller].getChildren
      //      println("the suffixParaSet is: " + suffixParaElementsSet)
      suffixParaElementsSet.map(ite => {
        var tempPrefix: Prefix = new Prefix(prefix.node, ite)
        paraElements.add(tempPrefix.asInstanceOf[Term])
      })
    }
    paraElements
  }

  /*
   * 对每一条规则的每一个def，分别遍历其它的规则的cuse以及puse，如果包含在其中，那么将规则的名字
   * 添加到当前规则的cuseRules或者puseRules集合中。
   */
  def setRuleDupairs(rules: Set[ReactionRule]) = {
    rules.map(it => setRuleDCPuses(it))//设置本条规则的def、cuse、puse
    //取出来一条规则，遍历其它的所有规则，找出来当前规则的所有的puse还有cuse的规则,以及有相同def的规则
    rules.map(ite => {
      rules.map(other => {
        if (other.name != ite.name) {//其他的规则和本次遍历的规则比较
          ite.defTerm.map(iteDef => {//本条规则的def
            //找到cuse的rule
            other.cuses.map(it => {
              if (it.termType == TermType.THOLE && iteDef.termType == TermType.THOLE) {
                if (iteDef.asInstanceOf[Hole].index == it.asInstanceOf[Hole].index) {
                  ite.cuseRules += other.name
                }
              }
              if (it.termType != TermType.THOLE && iteDef.termType != TermType.THOLE) {
                if (comparePrefix(iteDef.asInstanceOf[Prefix], it.asInstanceOf[Prefix])) {
                  ite.cuseRules += other.name
                }
              }
            })

            //找到puse的rule
            other.puses.map(it => {
              if (it.termType == TermType.THOLE && iteDef.termType == TermType.THOLE) {
                if (iteDef.asInstanceOf[Hole].index == it.asInstanceOf[Hole].index) {
                  ite.puseRules += other.name
                }
              }
              if (it.termType != TermType.THOLE && iteDef.termType != TermType.THOLE) {
                if (comparePrefix(iteDef.asInstanceOf[Prefix], it.asInstanceOf[Prefix])) {
                  ite.puseRules += other.name
                }
              }
            })

            other.defTerm.map(it => {
              //找到有相同def的rule
              if (it.termType == TermType.THOLE && iteDef.termType == TermType.THOLE) {
                if (iteDef.asInstanceOf[Hole].index == it.asInstanceOf[Hole].index) {
                  ite.defRules += other.name
                }
              }
              if (it.termType != TermType.THOLE && iteDef.termType != TermType.THOLE) {
                if (comparePrefix(iteDef.asInstanceOf[Prefix], it.asInstanceOf[Prefix])) {
                  ite.defRules += other.name
                }
              }
            })

          })
        }
      })
    })
  }

  /*
   * 找到du对，使用map保存规则到规则集合的映射
   */
  def getAllDPUs(rules: Set[ReactionRule]): Map[String, Set[String]] = {
    var dpuMaps: Map[String, Set[String]] = Map()
    setRuleDupairs(rules)
    rules.map(ite => {
      dpuMaps += (ite.name -> ite.puseRules)
    })
       println("The dpus is: " + dpuMaps)

    dpuMaps
  }

  def getAllDCUs(rules: Set[ReactionRule]): Map[String, Set[String]] = {
    var dcuMaps: Map[String, Set[String]] = Map()
    setRuleDupairs(rules)
    rules.map(ite => {
      dcuMaps += (ite.name -> ite.cuseRules)
    })

        println("The dcus is: " + dcuMaps)
    dcuMaps
  }

  def getAllRulesWithSameDef(rules: Set[ReactionRule]): Map[String, Set[String]] = {
    var sameDefRules: Map[String, Set[String]] = Map()
    setRuleDupairs(rules)
    rules.map(ite => {
      sameDefRules += (ite.name -> ite.defRules)
    })

        println("The defs is: " + sameDefRules)
    sameDefRules
  }
}

object TestParseRules {
  def main(args: Array[String]) {
//    var prefixcompare = "A[x].B[y]"
//    var prefixcompare2 = "A[x].B[y].C[z]"
//    var redex = "A[x].(B[y] | $0) | C[z]"
//    var reactum = "A[x].(B[y] | C[z])"
    var prefixcompare = "A.B"
    var prefixcompare2 = "A.B.C"
    var redex = "A.(B.(D|E) | $0) | C"
    var reactum = "A.(B | C)"
    //      var testSuffix = "A[x].(B[y] | $0)"
    var term1 = TermParser.apply(redex)
    var term2 = TermParser.apply(reactum)
    
    var redex3 = "Zone[x,w].(Pax[y,a] | $1) || Zone[z,x].$2"
    var reactum4 = "Zone[x,w].$1 || Zone[z,x].(Pax[y,a]|$2)"
    
    var term3 = TermParser.apply(redex3)
    var term4 = TermParser.apply(reactum4)
    
    var ruletest1: ReactionRule = new ReactionRule(term3, term4)

    var testPre = TermParser.apply(prefixcompare)
    var testPre2 = TermParser.apply(prefixcompare2)
    var ruletest: ReactionRule = new ReactionRule(term1, term2)

    println(testPre.size)

    println(ParseRules.comparePrefix(testPre.asInstanceOf[Prefix], testPre2.asInstanceOf[Prefix]))
    
    var pre1 = "Zone[x,w].$1"
    var pre2 = "Zone[z,x].$1"
    var testPre11 = TermParser.apply(pre1)
    var testPre22 = TermParser.apply(pre2)
    println("Test if can distinguish different port:"+ParseRules.comparePrefix(testPre11.asInstanceOf[Prefix], testPre22.asInstanceOf[Prefix]))

    //    var term2 = term1.asInstanceOf[Prefix]
    println(term1.toString)
    //    println(term1.termType == TermType.TPREF)
    //    println(term2.ctrl)
    //    println(term2.suffix)
    //    println(term2.suffix.termType == TermType.TPAR)
    //    println(term2.suffix.asInstanceOf[Paraller].getChildren)
//    ParseRules.getParaElementsOfTerm(term1).map(ite => println(ite))
//    println(ParseRules.getIndependentEleOfTerm(term1))
//    println(ParseRules.getIndependentEleOfTerm(term2))
    
    println(ruletest.toString)
    println("----test rule def cuse etc.")
    println(ParseRules.setRuleDCPuses(ruletest))
    println("cuses is: " + ruletest.cuses + " puses is: " + ruletest.puses + " def is" + ruletest.defTerm)   
    
    println("-----------------------------")
    println(ruletest1.toString)
    println(ParseRules.setRuleDCPuses(ruletest1))
    println("cuses is: " + ruletest1.cuses + " puses is: " + ruletest1.puses + " def is" + ruletest1.defTerm)
//    
//      var term = TermParser.apply("P[lf,p,rf]");
//      println("Ident:" + term + ", size=" + term.size );

  }
}