package org.bigraph.bigsim.strategy

import scala.Array.canBuildFrom
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.xml.XML
import org.bigraph.bigsim.parser.TermParser
import org.bigraph.bigsim.model.Term
import org.bigraph.bigsim.model.TermType
import org.bigraph.bigsim.model.Bigraph

object ParseXML {
  // 路径中某个状态节点，要有给定的感兴趣模式中的至少某一个，所以，只要找每一条规则的生成物就ok
  // 另外，不管多少感兴趣模式，每一个对应的rule，都放到一个set中去，路径筛选的时候，只要去找
  // 包含这个set中的任意一个rule的路径就ok了。
  def getNamesOfIntrestedPatterns(b: Bigraph, patterns: scala.xml.Elem): Set[String] = {
    var intrestedRule: Set[String] = Set()
    var intrestedPatternSet =
      (Set[String]() /: (patterns \\ "IntrestedPattern")) {
        (set, intrestedPattern) =>
          var intrestedRuleReactum = (intrestedPattern \ "@details").toString()
          set += intrestedRuleReactum
          set
      }

    b.rules.map(it => {
      intrestedPatternSet.map(ite => {
        if (checkInOrLink(ite, it.reactum)) {
          intrestedRule += it.name
        }
      })
    })

    intrestedRule
  }

  // 通过pattern以及nextPattern找到一条规则（pattern找redex，nextPattern找reactum），然后通过pattern找到规则中生成物包含的pattern的rule
  // 然后在规则衍化过程中，对某些规则之间强调反应顺序（就是限定接下来反应那些规则，来减少路径）
  def getNamesMapOfPatternRelationships(b: Bigraph, patterns: scala.xml.Elem): Map[Set[String], Set[String]] = {
    var PatternRelationshipMap =
      (Map[String, String]() /: (patterns \\ "Pattern")) {
        (map, rrnode) =>
          var pattern = (rrnode \ "@details").toString()
          var nextPattern = (rrnode \ "NextPattern" \ "@details").toString()
          map += (pattern -> nextPattern)
          map
      }

    var ruleRelationMap: Map[Set[String], Set[String]] = Map()

    for ((key, value) <- PatternRelationshipMap) {
      var setPN: Set[String] = Set()
      var setP: Set[String] = Set()
      b.rules.map(ite => {
        if (checkInOrLink(key, ite.reactum)) {
          setP.add(ite.name)
        }
        if (checkInOrLink(key, ite.redex) && checkInOrLink(value, ite.reactum)) {
          setPN.add(ite.name)
        }
      })
      ruleRelationMap += (setP -> setPN)
    }
    ruleRelationMap
  }

  def checkInOrLink(str: String, term: Term): Boolean = {
    var res: Boolean = false
    if (str.indexOf(" in ") > 0) {
      res = inCheck(str, term)
    } else if (str.indexOf(" link ") > 0) {
      res = linkCheck(str, term)
    } else if (str.indexOf(" not link ") > 0) {
      res = notLinkCheck(str, term)
    } else if (str.indexOf(" not in ") > 0) {
      res = notInCheck(str, term)
    }
    res
  }

  def notInCheck(str: String, term: Term): Boolean = {
    var notInCheck: Boolean = false
    var notInIndex = str.indexOf(" not in ")
    var notInControl: String = str.substring(0, notInIndex)
    var outerControl: String = str.substring(notInIndex + 8)

    var termChildren = ParseRules.getIndependentEleOfTerm(term)
    termChildren.map(ite => {
      if (!notInCheck) {
        if (ite.termType == TermType.TPREF) {
          var outerCtlIndex = ite.toString.indexOf(outerControl)
          var notInCtlIndex = ite.toString.indexOf(notInControl)
          if (outerCtlIndex != -1 && notInCtlIndex != -1 && outerCtlIndex > notInCtlIndex) {
            notInCheck = true
          }
        }
      }
    })
    notInCheck
  }

  def inCheck(str: String, term: Term): Boolean = {
    var incheck: Boolean = false
    var inIndex = str.indexOf(" in ")
    var innnerControl: String = str.substring(0, inIndex)
    var outerControl: String = str.substring(inIndex + 4)

    var termChildren = ParseRules.getIndependentEleOfTerm(term)
    termChildren.map(ite => {
      if (!incheck) {
        if (ite.termType == TermType.TPREF) {
          var outerCtlIndex = ite.toString.indexOf(outerControl)
          var innnerCtlIndex = ite.toString.indexOf(innnerControl)
          if (outerCtlIndex != -1 && innnerCtlIndex != -1 && outerCtlIndex < innnerCtlIndex) {
            incheck = true
          }
        }
      }
    })
    incheck
  }

  def notLinkCheck(str: String, term: Term): Boolean = {
    var notlinkcheck: Boolean = true
    var notLinkIndex = str.indexOf(" not link ")
    var linkControlA: String = str.substring(0, notLinkIndex)
    var linkControlB: String = str.substring(notLinkIndex + 10)
    //下面，把term转为字符串，然后找A和B的位置，以及后面的连接，[]里面的内容，然后以分号分割，比较里面的
    //是否某个字符串是否有相等的。可以使用charat来找“[“与”]“之间的字符串，然后按照逗号分割就ok。
    var termString = term.toString
    var ctlAIndex = termString.indexOf(linkControlA)
    var ctlBIndex = termString.indexOf(linkControlB)

    if (ctlAIndex != -1 && ctlBIndex != -1) {
      if (termString.charAt(ctlAIndex + linkControlA.size) == '[' &&
        termString.charAt(ctlBIndex + linkControlB.size) == '[') {
        var i: Int = 0
        while (termString.charAt(ctlAIndex + linkControlA.size + i) != ']') {
          i += 1
        }
        var linknameStrOfA = termString.substring(ctlAIndex + linkControlA.size + 1, ctlAIndex + linkControlA.size + i - 1)

        var j: Int = 0
        while (termString.charAt(ctlBIndex + linkControlB.size + j) != ']') {
          j += 1
        }
        println("termString is: " + termString.size + "ctlBIndex is: " + ctlBIndex + "linkControlB is: " + linkControlB.size)
        var linknameStrOfB = termString.substring(ctlBIndex + linkControlB.size + 1, ctlBIndex + linkControlB.size + j - 1)

        //只要link name中有一个相同，就是匹配的
        linknameStrOfA.split(",").map(ite => {
          if (linknameStrOfB.split(",").contains(ite)) {
            notlinkcheck = false
          }
        })

      }
    }

    notlinkcheck
  }

  def linkCheck(str: String, term: Term): Boolean = {
    var linkcheck: Boolean = false
    var linkIndex = str.indexOf(" link ")
    var linkControlA: String = str.substring(0, linkIndex)
    var linkControlB: String = str.substring(linkIndex + 6)
    //下面，把term转为字符串，然后找A和B的位置，以及后面的连接，[]里面的内容，然后以分号分割，比较里面的
    //是否某个字符串是否有相等的。可以使用charat来找“[“与”]“之间的字符串，然后按照逗号分割就ok。
    var termString = term.toString
    var ctlAIndex = termString.indexOf(linkControlA)
    var ctlBIndex = termString.indexOf(linkControlB)

    if (ctlAIndex != -1 && ctlBIndex != -1) {
      if (termString.charAt(ctlAIndex + linkControlA.size) == '[' &&
        termString.charAt(ctlBIndex + linkControlB.size) == '[') {
        var i: Int = 0
        while (termString.charAt(ctlAIndex + linkControlA.size + i) != ']') {
          i += 1
        }
        var linknameStrOfA = termString.substring(ctlAIndex + linkControlA.size + 1, ctlAIndex + linkControlA.size + i - 1)

        var j: Int = 0
        while (termString.charAt(ctlBIndex + linkControlB.size + j) != ']') {
          j += 1
        }
        println("termString is: " + termString.size + "ctlBIndex is: " + ctlBIndex + "linkControlB is: " + linkControlB.size)
        var linknameStrOfB = termString.substring(ctlBIndex + linkControlB.size + 1, ctlBIndex + linkControlB.size + j - 1)

        //只要link name中有一个相同，就是匹配的
        linknameStrOfA.split(",").map(ite => {
          if (linknameStrOfB.split(",").contains(ite)) {
            linkcheck = true
          }
        })

      }
    }

    linkcheck
  }

  def main(args: Array[String]) {
    var patterns = XML.load("filterrules/Patterns.xml")
    println(patterns)
    //println("The reactionRuleRelationship is: " + ParseXML.getNamesOfIntrestedPatterns(null, patterns))
    var str: String = "abc link de"
    var inindex = str.indexOf(" link ")
    println(inindex)
    println(str.substring(0, inindex))
    println(str.substring(inindex + 6))
    println(str.indexOf("zhaoxin"))
    //	  println("The focusRules is: " + focusRules)

    var str2: String = "a,b,c"
    println("The result after split: ")
    str2.split(",").map(it => println(it))

    var term: Term = TermParser.apply("ConsultingRoom.(Patient[p1,p2] | Computer[p1,p2]) | $0")
    //	  var term : Term = TermParser.apply("ConsultingRoom.(Patient[p1,p2] | Computer[connected]) | $0")
    var strIn: String = "Patient in ConsultingRoom"
    var strlink: String = "Patient link Computer"
    println(inCheck(strIn, term))
    println(linkCheck(strlink, term))

  }
}
