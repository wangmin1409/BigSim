package org.bigraph.bigsim.BRS

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.bigsim.utils._
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.simulator.Simulator

/**
 * @author zhaoxin
 * version 0.1
 */
object Match {
  def singleton(t: Match) = {
    var l: Set[Match] = Set()
    l += t
    l
  }

  def merge(a: Set[Match], b: Set[Match]) = {
    if (GlobalCfg.DEBUG) {
      println("BUG: match::merge(): " + a.size + " with " + b.size)
    }
    var n: Set[Match] = Set()
    n ++= a
    n ++= b
    n
  }
}

class Match(reactionRule: ReactionRule) {
  var names = Map[Name, Name]()
  var parameters = Map[Int, Term]()
  var mapping = Map[Term, Term]()
  var nodeMap = Map[Node, Node]()

  var reactNodes: Set[String] = Set() //这里应该是node名不是control名
  var reactNodesMap: Map[String, Set[String]] = Map()

  var rule = reactionRule
  var RRIncr: Double = 0
  var root: Term = null
  var parent: Match = null

  val isWide: Boolean = false
  var hasFailed: Boolean = false
  var hasSucceeded: Boolean = false

  if (GlobalCfg.DEBUG) {
    println("BUG: match::match(): new match created")
  }

  //Simulator.matchMarkDelete(this)

  def getReactNodes: String = {
    if (reactNodes.size > 0) {
      reactNodes.toList.sortWith((a: String, b: String) => a < b).mkString(",") //按字母顺序排好，中间用逗号隔开，如David,James
    } else {
      "" //若集合为空也会返回空串
    }
  }

  def addParam(id: Int, c: Term): Unit = {
    if (c == null) {
      println("BUG: attempted to add_param for null term in hole " + id)
      sys.exit(1)
    }
    parameters += (id -> c)
  }

  def getParam(id: Int): Term = {
    if (!parameters.contains(id)) {
      null
    } else {
      parameters(id)
    }
  }

  def addMatch(src: Term, target: Term) = {
    if (src == null || target == null) {
      println("BUG: attempted to add null match")
      sys.exit(1);
    }
    mapping += (target -> src)
  }

  def getMapping(targ: Term): Term = {
    if (!mapping.contains(targ)) {
      null
    } else {
      mapping(targ)
    }
  }

  def captureName(src: Name, target: Name): Unit = {
    names(src) = target
    if (parent != null) {
      parent.names(src) = target
    }
  }

  def getNames: Map[Name, Name] = {
    if (parent != null) {
      parent.names
    } else {
      names
    }
  }

  def getName(n: Name): Name = {
    if (n == null) {
      null
    } else if (!Bigraph.isFree(n)) {
      n
    } else if (parent == null) {
      if (names.contains(n)) {
        names(n)
      } else {
        n
      }

    } else {
      parent.getName(n)
    }
  }

  def conflict(rns: List[String]): Boolean = {//判断reactNodes:Set[String]与rns这个List中有没有冲突的nodes，冲突就是不可以并发反应
    var conflict = false
    rns.map(rn => {
      if (reactNodes.contains(rn)) {
        if (GlobalCfg.DEBUG)
          println("match " + rule.name + "\tconfilict node:" + rn)
        conflict = true
      }
    })
    return conflict
  }

  def failure = {
    hasFailed = true
    hasSucceeded = false
    Simulator.matchMarkDelete(this)
    var res: Set[Match] = Set()
    res
  }

  def success: Unit = {
    if (hasFailed) {
      println("BUG: match::success(): match has already failed!")
      sys.exit(1)
    }
    hasSucceeded = true
    hasFailed = false
  }

  override def clone: Match = {
    if (GlobalCfg.DEBUG) {
      println("BUG: match::clone(): new match created")
    }

    var m = new Match(rule)
    m.hasSucceeded = hasSucceeded
    m.hasFailed = hasFailed
    m.root = root
    // m.reactNodes = reactNodes

    reactNodes.foreach(rn => {
      m.reactNodes.add(rn)
    })

    reactNodesMap.foreach(rnm => {
      m.reactNodesMap.put(rnm._1, rnm._2)
    })

    names.foreach(n => {
      m.names(n._1) = n._2
    })

    parameters.foreach(p => {
      m.parameters(p._1) = p._2
    })

    mapping.foreach(ma => {
      m.mapping(ma._1) = ma._2
    })

    nodeMap.foreach(cm => {
      m.nodeMap(cm._1) = cm._2
    })

    /*
    for ((key, value) <- names) {
      m.names(key) = value
    }
    for ((key, value) <- parameters) {
      m.parameters(key) = value
    }
    for ((key, value) <- mapping) {
      m.mapping(key) = value
    }

    for ((key, value) <- ctrlMap) {
      m.ctrlMap(key) = value;
    } */

    if (m.root != null) {
      if (GlobalCfg.DEBUG) {
        println("BUG: match::clone(): setting root: " + m.root.toString())
      }
    }
    m
  }

  def fresh(head: Term, rem: List[Term]): Match = {
    if (GlobalCfg.DEBUG)
      println("BUG: match::fresh(): new match created")
    new Match(rule)
  }

  def incorporate(other: Match): Unit = {//合并
    for ((key, value) <- other.parameters) {
      this.parameters(key) = value
    }
    for ((key, value) <- other.mapping) {
      this.mapping(key) = value
    }
    for ((key, value) <- other.names) {
      this.captureName(key, value)
    }

    for ((key, value) <- other.nodeMap) {
      this.nodeMap(key) = value;
    }
    other.reactNodes.foreach(this.reactNodes.add(_))
    other.reactNodesMap.foreach(f => this.reactNodesMap.put(f._1, f._2))

  }

  override def toString = {
    var s = "Match: "

    if (root == null) {
      s += "Root: NULL"
    } else {
      s += "Root: " + root.toString()
    }

    if (hasSucceeded) {
      s += " Succeeded"
    }

    if (hasFailed) {
      s += " Failed"
    }

    s += "\nParameters:\n"

    for ((key, value) <- parameters) {
      if (value != null) {
        s += "\t" + key + ": " + value.toString() + "\n"
      } else {
        s += "\t" + key + ": " + "nil" + "\n"
      }
    }

    s += "Term Mapping:\n"

    for ((key, value) <- mapping) {
      s += "\t" + key.toString() + " -> " + value.toString() + "\n"
    }

    s += "Link Mapping:\n";

    var pn = Map[Name, Name]()

    if (parent != null) {
      pn = parent.names
    } else {
      pn = names
    }

    for ((key, value) <- pn) {

      s += "\t" + Bigraph.nameToString(key) + " -> " + Bigraph.nameToString(value) + "(" + key + " -> " + value + ")\n"
    }

    s += "React Nodes:\n"
    reactNodes.foreach(rn => { s += "\t" + rn + " " })
    s
  }
}

class WideMatch(rule: ReactionRule) extends Match(rule) {
  override val isWide = true
  var submatches: List[Match] = List()

  def addSubMatch(m: Match): Unit = {
    submatches = submatches.:+(m)
    this.incorporate(m)
    m.parent = this
  }

  override def clone: WideMatch = {
    var m: WideMatch = new WideMatch(rule)
    m.submatches = submatches
    for ((key, value) <- names) {
      m.names(key) = value
    }
    for ((key, value) <- parameters) {
      m.parameters(key) = value
    }
    for ((key, value) <- mapping) {
      m.mapping(key) = value
    }
    return m
  }

  override def toString = {
    var s = "WideMatch:\n"
    for (ite <- submatches) {
      s += ite.toString() + "\n"
    }
    s
  }

}

object testMatch {
  def main(args: Array[String]) {
    //    	println("Match::testMatch::Match")
    //    	var redex : Term = new Term()
    //    	var reactum : Term = new Term()
    //    	var rule : ReactionRule = new ReactionRule(redex, reactum)
    //    	
    //    	println("Match::testMatch::Match::addParam")
    //    	var matchtest : Match = new Match(rule)
    //    	var addParamTestA : Term = new Term()
    //    	var addParamTestB : Term = new Term()
    //    	matchtest.addParam(1, addParamTestA)
    //    	matchtest.addParam(2, addParamTestB)
    //    	println(matchtest.parameters.toString())
    //    	
    //    	println("Match::testMatch::Match::getParam")
    //    	println(matchtest.getParam(1).toString())
    //    	println(matchtest.getParam(2).toString())
    //    	
    //    	println("Match::testMatch::Match::addMatch")
    //    	var src : Term = new Term()
    //    	var target : Term = new Term()
    //    	matchtest.addMatch(src, target)
    //    	println(matchtest.mapping.toString())
    //    	
    //    	println("Match::testMatch::Match::getMapping")
    //    	println(matchtest.getMapping(target))
    //    	
    //    	println("Match::testMatch::Match::captureName")
    //    	var srcName : Name = new Name("Src")
    //    	var targetName : Name = new Name("Target")
    //    	var srcNameA : Name = new Name("SrcA")
    //    	var targetNameB : Name = new Name("TargetB")
    //    	matchtest.captureName(srcName, targetName)
    //    	matchtest.captureName(srcNameA, targetNameB)
    //    	println(matchtest.names.toString())
    //    	
    //    	println("Match::testMatch::Match::getNames")
    //    	println(matchtest.getNames)
    //    	
    //    	println("Match::testMatch::Match::getName")
    //    	println(matchtest.getName(srcName))
    //    	
    //    	
    //    	
    //    	println("Match::testMatch::Match::success")
    //    	println(matchtest.success)
    //    	matchtest.hasFailed = false
    //    	println(matchtest.success)
    //    	
    //    	println("Match::testMatch::Match::failure")
    //    	println(matchtest.failure)
    //    	
    //    	println("Match::testMatch::Match::clone")
    //    	var m : Match = matchtest.clone()
    //    	println(matchtest.toString())
    //    	println(m.toString())
    //    	
    //    	println("Match::testMatch::Match::incorporate")
    //    	var other : Match = new Match(rule)
    //    	var srcNameC : Name = new Name("SrcC")
    //    	var targetNameD : Name = new Name("TargetD")
    //    	other.captureName(srcNameC, targetNameD)
    //    	println(other.names)
    //    	matchtest.incorporate(other)
    //    	println(matchtest.names)
    //    	
    //    	println("Match::testMatch::WideMatch::addSubmatch")
    //    	var widematchtest : WideMatch = new WideMatch(rule)
    //    	widematchtest.addSubMatch(matchtest)
    //    	println(widematchtest.submatches)
    //    	println("The parent of matchtest: " + matchtest.parent.toString())
    //    	
    //    	println("Match::testMatch::WideMatch::clone")
    //    	var widematchclonetest = widematchtest.clone()
    //    	println(widematchclonetest)
    //    	
    //    	println("Match::testMatch::Match::singleton")
    //    	var singletonMatch = Match.singleton(matchtest)
    //    	println(singletonMatch)
    //    	
    //    	println("Match::testMatch::Match::merge")
    //    	var otherA : Match = new Match(rule)
    //    	var srcNameD : Name = new Name("SrcD")
    //    	var targetNameE : Name = new Name("TargetE")
    //    	otherA.captureName(srcNameD, targetNameE)
    //    	println("other: " + other.toString() + "otherA: " + otherA.toString())
    //    	var setA : Set[Match] = Set()
    //    	var setB : Set[Match] = Set()
    //    	setA += otherA
    //    	setB += other
    //    	println("setA: " + setA.toString() + "setB: " + setB.toString())
    //    	var mergetest = Match.merge(setA, setB)
    //    	println(mergetest.toString())
    //    	
    //    	var setTestValue = Set("A", "B")
    //    	println("1: " + setTestValue.head + " 2 is: " + setTestValue.tail.head)
    ////    	
    //    	var listTest = List("1", "2", "3")
    //    	for(alibaba <- 0 until listTest.size) {
    //    	  println("the value of listTest is: " + listTest(alibaba))
    //    	}
    //    	println("list.head is 1 ? " + listTest.head)
    //    	
    //    	var listTestDrop = listTest.drop(1)
    //    	for(alibaba <- 0 until listTestDrop.size) {
    //    	  println("the value of listTestDrop is: " + listTestDrop(alibaba))
    //    	}
    //    	
    //    	listTest = listTestDrop
    //    	for(alibaba <- 0 until listTest.size) {
    //    	  println("the value of listTest is: " + listTest(alibaba))
    //    	}

    //    	var isBreak = false
    //    for (ite <- listTestDrop; if isBreak != true) {
    //      if (ite == "3") {
    //        println("test for break and true: " + ite)
    //        isBreak = true
    //      }
    //    }
    //      var m1 =  Set("A", "B")
    //      var m2 =  Set("C", "D")
    //      println(m1.toString())
    //      println(m2.toString())
    //      
    //      testClear(m1, m2)
    //      println(m1.toString())
    //      var m1 =  Set("A", "B")
    //      var m2 =  Set("C", "D")
    //      
    //      var listTestNUll = List("1", "2", "3")
    //      println("listTestNULL before is: " + listTestNUll.toString())
    //      listTestNUll = listTestNUll.drop(listTestNUll.size)
    //      println("listTestNULL after is: " + listTestNUll.toString())
    //      
    //      var map1 = Map("A1" -> 1, "B1" -> 2)
    //      
    ////      map1("A3") += 2
    //      println(map1.contains("A3"))

    var m1 = Set("A", "B")
    var m2 = Set("C", "D")

    m1 ++= m2
    println(m1)

  }
  def testClear(mA: Set[String], mB: Set[String]) {
    mA.clear()
    mB.clear()
  }
}