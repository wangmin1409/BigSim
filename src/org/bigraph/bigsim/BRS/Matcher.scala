package org.bigraph.bigsim.BRS

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.bigsim.model.Control
import org.bigraph.bigsim.model.Hole
import org.bigraph.bigsim.parser.TermParser
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.model.Term
import org.bigraph.bigsim.model.Num
import org.bigraph.bigsim.model.Regions
import org.bigraph.bigsim.model.Paraller
import org.bigraph.bigsim.model.TermType
import org.bigraph.bigsim.model.Prefix
import org.bigraph.bigsim.model.Nil
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.model.Name
import org.bigraph.bigsim.model.Bigraph

/**
 * @author zhaoxin
 * version 0.1
 */

object Matcher {

  def tryMatch(t: Term, r: Term, m: Match): Set[Match] = {
    if (t.termType == TermType.TPREF && r.termType == TermType.TPREF) {
      var tempT = t.asInstanceOf[Prefix]
      var tempR = r.asInstanceOf[Prefix]
      tryMatchPrefixPrefix(tempT, tempR, m)
    } else if (t.termType == TermType.TPAR && r.termType == TermType.TPREF) {
      var tempT = t.asInstanceOf[Paraller]
      var tempR = r.asInstanceOf[Prefix]
      tryMatchParallerPrefix(tempT, tempR, m)
    } else if (t.termType == TermType.TPREF && r.termType == TermType.TPAR) {
      var tempT = t.asInstanceOf[Prefix]
      var tempR = r.asInstanceOf[Paraller]
      tryMatchPrefixParaller(tempT, tempR, m)
    } else if (t.termType == TermType.TPAR && r.termType == TermType.TPAR) {
      var tempT = t.asInstanceOf[Paraller]
      var tempR = r.asInstanceOf[Paraller]
      tryMatchParallerPareller(tempT, tempR, m)
    } else if (t.termType == TermType.TNUM && r.termType == TermType.TNUM) {
      var tempT = t.asInstanceOf[Num]
      var tempR = r.asInstanceOf[Num]
      tryMatchNumNum(tempT, tempR, m)
    } else if (r.termType == TermType.TREGION) {
      var tempR = r.asInstanceOf[Regions]
      tryMatchTermRegions(t, tempR, m)
    } else if (r.termType == TermType.THOLE) {
      var tempR = r.asInstanceOf[Hole]
      tryMatchTermHole(t, tempR, m)
    } else if (t.termType == TermType.TNIL && r.termType == TermType.TNIL) {
      var tempT = t.asInstanceOf[Nil]
      var tempR = r.asInstanceOf[Nil]
      tryMatchNilTerm(tempT, tempR, m)
    } else {
      m.failure
    }
  }

  // t matches r if the value of t equals the value of r
  //比较两个Num类型的话，就是比较值，相同则返回包含传入match的Set，否则返回空Set
  //这个在原来版本中没有，应该是qq加上的。
  def tryMatchNumNum(t: Num, r: Num, m: Match): Set[Match] = {
    if (t.value == r.value) {
      if (m.root == null) {
        m.root = t
      }
      m.success
      Match.singleton(m);
    } else {
      m.failure
    }
  }

  // Term always matches Hole
  def tryMatchTermHole(t: Term, r: Hole, m: Match): Set[Match] = {
    m.success
    m.addParam(r.index, t)
    Match.singleton(m)
  }

  // Paraller doesn't match Prefix---No need to change for new format!
  def tryMatchParallerPrefix(t: Paraller, r: Prefix, m: Match): Set[Match] = {
    if (GlobalCfg.DEBUG) {
      println("matcher::tryMatchParallerPrefix " + "matching par: " + t.toString() + " against pref redex: " + r.toString())
    }
    m.failure
  }

  def tryMatchPrefixPrefix(t: Prefix, r: Prefix, m: Match): Set[Match] = {
    if (GlobalCfg.DEBUG) {
      println("matcher::tryMatchPrefixPrefix " + "matching prefix: " + t.toString() + " against redex: " + r.toString())
    }
    assert(m != null)

    // compare the controls of t1 and t2, return failure if t1's control does not equal with t2's
    /**
     *  @author liangwei
     *  if the RR has node, it will try match the given node's name.
     */
    if (t.node.ctrl.name == r.node.ctrl.name &&
      (r.node.name.equals("") || r.node.name.equals(t.node.name))) {

      if (GlobalCfg.DEBUG) {
        println("matcher::tryMatch " + "Prefix match: " + t.node.ports + " with " + r.node.ports.toString() + " Active: " + t.activeContext)
      }
      if (m.root == null && !(t.parent == null || (t.parent != null && t.parent.activeContext))) {
        return m.failure
      } else if (m.root == null) {
        m.root = t
      }

      m.addMatch(r, t)//放入mapping

      m.nodeMap += (r.node -> t.node);

      var tnm: List[Name] = t.node.ports
      var rnm: List[Name] = r.node.ports
      //外层的t.ctrl == r.ctrl 首先比较control是否相同，然后，tnm取得里面的name的值
      //首先判断，如果里面包含的name个数相同再继续比较，否则的话就不同了。
      if (tnm.size != rnm.size) {
        if (GlobalCfg.DEBUG) {
          println("matcher: tnm: " + tnm.size + " rnm: " + rnm.size)
        }

        return m.failure
      }

      //这一段是比较port列表是否相同 lbj
      if (GlobalCfg.DEBUG) {
        println("MATCHER LINK START")
      }

      for (i <- 0 until tnm.size) {
        if (GlobalCfg.DEBUG) {
          println("MATCH: " + i + ": of term: " + t.toString() + ". " + m.toString())
        }
        // Comments by zhaoxin: check the names. check if the ports names are the same
        if (true) {
          //   if (!Bigraph.isFree(rnm(i))) {
          if (GlobalCfg.DEBUG) {
            println("matcher: !free: " + Bigraph.nameToString(rnm(i)))
          }
          // 为了区分多个实例，modle中link name使用字母+数字来标识，而rule中也用字母+数字，比较的时候只是比较字母
          var linkNameEqual: Boolean = false
          var tempModleLinkName: String = getAlpPartFromStr(tnm(i).name)
          var tempRuleLinkName: String = getAlpPartFromStr(rnm(i).name)
          //println("tempModleLinkName is " + tempModleLinkName)
          //println("tempRuleLinkName is " + tempRuleLinkName)

          //if (tempModleLinkName.equals(tempRuleLinkName)) {
          if (tnm(i).name.equals(rnm(i).name)) {
            linkNameEqual = true
          } /*
          
          if(tempRuleLinkName.size < tempModleLinkName.size){
            if(tempModleLinkName.startsWith(tempRuleLinkName) 
              && tempModleLinkName.charAt(tempRuleLinkName.size).>=('0')
              && tempModleLinkName.charAt(tempRuleLinkName.size).<=('9')) {
            	linkNameEqual = true
            }
          } else if(tempRuleLinkName.size == tempModleLinkName.size){
            linkNameEqual = true
          }
          */

          // if (tnm(i).name != rnm(i).name) {
          if (!linkNameEqual) {
            if (GlobalCfg.DEBUG) {
              println("MATCH FAILED: expected: " + Bigraph.nameToString(rnm(i)) + " got " + Bigraph.nameToString(tnm(i)))
            }
            return m.failure
          } else { //这里添加对于link的类型(idle-->idle, edge---->edge, innername/outername--->edge)的处理，暂时先不抽取成为方法
            var nameTypeR: String = rnm(i).nameType
            var nameTypeT: String = tnm(i).nameType
            var nameTypeSet: Set[String] = Set()
            nameTypeSet.add("edge")
            nameTypeSet.add("innername")
            nameTypeSet.add("outername")

            if ((tempModleLinkName.equals("idle") && tempRuleLinkName.equals("idle")) || (nameTypeSet.contains(nameTypeR) && nameTypeT == "edge")) {
              m.captureName(rnm(i), tnm(i))
              if (GlobalCfg.DEBUG) {
                println("matcher: !free: captured " + rnm(i))
              }
            } else {
              return m.failure
            }
          }
        } else {
          // This is a symbolic link name, not a literal one
          // We need to look it up in the existing mappings
          // If it exists, tnm[i] must match what it previously matched
          // If not, we bind this name to tnm[i]
          if (GlobalCfg.DEBUG) {
            println("matcher: free: " + rnm(i) + " match object: " + m.toString())
          }

          var nmap: Map[Name, Name] = m.getNames

          if (!nmap.contains(rnm(i))) {
            m.captureName(rnm(i), tnm(i))
            if (GlobalCfg.DEBUG) {
              println("Bigraph nameMap is: " + Bigraph.nameMap)
              println("matcher: free: new " + Bigraph.nameToString(rnm(i)) + " = " + Bigraph.nameToString(tnm(i)))
            }
          } else {
            if (GlobalCfg.DEBUG) {
              println("matcher: free: old " + rnm(i))
            }
            if (nmap(rnm(i)) != tnm(i)) {
              return m.failure
            }
            if (GlobalCfg.DEBUG) {
              println("matcher: free: old matched " + rnm(i))
            }
          }
        }
      }

      //  if  the ctrl of redex is changed, add change info就是这里set了Match的reactNodes和reactNodesMap的值
      if (m.rule.reactNodes.contains(r.node.ctrl.name)) {
        m.reactNodes.add(t.node.name)
        val rns = m.reactNodesMap.getOrElse(r.node.ctrl.name, Set())
        rns.add(t.node.name)
        m.reactNodesMap.put(r.node.ctrl.name, rns)
      }

      return tryMatch(t.suffix, r.suffix, m)
    } else {
      return m.failure
    }
  }

  def getAlpPartFromStr(str: String): String = {
    var resultStr: String = ""
    var digit: Boolean = false
    for (i <- 0 to (str.size - 1); if digit == false) {
      if (str.charAt(i) >= '0' && str.charAt(i) <= '9') {
        resultStr = str.substring(0, i)
        digit = true
      }
    }
    if (resultStr.size == 0) {
      resultStr += str
    }
    resultStr
  }

  def tryMatchTermReactionRule(t: Term, r: ReactionRule): Set[Match] = {
    var matches: Set[Match] = Set()

    if (r.redex.termType == TermType.TREGION) {
      var tempMatch = new Match(r)
      return tryMatch(t, r.redex, tempMatch)
    }

    var p: Term = t.next

    /**
     * Here we use get next, to get each children structure
     * in the agent term. The we use each to match the redex.
     */
    while (p != null) {
      if (p.parent == null || p.parent.activeContext) {
        var nm: Match = new Match(r)
        matches = Match.merge(matches, tryMatch(p, r.redex, nm));
      }
      p = t.next
    }
    t.reset
//    var matches1: Set[Match] = Set()
//    if(matches.size>0) matches1.add(matches.head)
    matches
//    matches1
  }

  def tryMatchTermRegions(t: Term, r: Regions, m: Match): Set[Match] = {
    if (GlobalCfg.DEBUG) {
      println("matcher::tryMatchTermRegions " + "matching region: " + t.toString() + " against redex: " + r.toString())
    }
    if (m.root != null || t.parent != null) {
      return m.failure
    }

    // r->get_children() put each element of T||T into list ch.
    var ch: List[Term] = r.getChildren
    m.root = t;
    m.addMatch(r, t)
    var wm: Match = new WideMatch(m.rule)
    var tempWM: Set[Match] = Match.singleton(wm)
    return regionMatch(t, ch, tempWM)
  }

  def regionMatch(t: Term, redexes: List[Term], m: Set[Match]): Set[Match] = {
    if (GlobalCfg.DEBUG) {
      println("Enter into Matcher::regionMatch")
    }

    if (redexes.size == 0) {
      for (ele <- m.toList) {
        ele.success
      }
      return m
    }

    var redex = redexes.head
    var tempRedexes = redexes.drop(1)
    var result: Set[Match] = Set()
    for (element <- m.toList) {
      // For each existing wide match, we get a new set of matches:
      // bug fix, 当反应规则根区域中含nil时，跳过nil的匹配
      if (redex.termType != TermType.TNIL) {
        var ms: Set[Match] = tryMatchAnywhere(t, redex, element.rule, element)
        if (ms.size == 0) {
          element.failure
        }
        var cp: Set[Match] = crossprod(Match.singleton(element), ms)
        result ++= cp
      } else {
        var nilNode: Nil = new Nil();

        result.add(element)
      }
    }
    return regionMatch(t, tempRedexes, result)
  }

  def tryMatchPrefixParaller(t: Prefix, r: Paraller, m: Match): Set[Match] = {
    // This covers cases like redex: a.(b | $0) matching against a.b where $0 = nil
    // should be redex: a.b | $0  ? no! do not act like these

    var ch: Set[Term] = r.getChildren

    if (ch.size > 2) {
      return m.failure
    }

    var hasHole: Hole = null
    var nonHole: Term = null

    for (ite <- ch.toList) {
      if (ite.termType == TermType.THOLE) {
        if (hasHole != null) {
          println("@Alert, a paraller has more than one site: " + r)
        } else {
          hasHole = ite.asInstanceOf[Hole]
        }
      } else {
        if (nonHole != null) {
          return m.failure
        }
        nonHole = ite
      }
    }

    /**
     * If like a | $0 | $1 and m is null,
     * it will be matched, if t.parent is paraller.
     *
     * try {
     * if (t.parent != null && t.parent.termType == TermType.TPAR && m.root == null) {
     * return m.failure
     * } } catch {
     * case ex: Exception => ex.printStackTrace()
     * }
     */

    if (nonHole == null || hasHole == null) {
      return m.failure
    }
    m.addParam(hasHole.index, new Nil())

    return tryMatch(t, nonHole, m)
  }

  // if the type of r is TNIL or THOLE, return match, otherwise return doesn't match
  def tryMatchNilTerm(t: Nil, r: Term, m: Match): Set[Match] = {
    if (GlobalCfg.DEBUG) {
      println("Enter into Matcher::tryMatchNilTerm")
    }
    if (r.termType == TermType.TNIL) {
      m.addMatch(r, t)
      m.success
      Match.singleton(m)
    } else if (r.termType == TermType.THOLE) {
      m.success
      var tempR = r.asInstanceOf[Hole]
      m.addParam(tempR.index, t)
      Match.singleton(m)
    } else {
      m.failure
    }
  }

  def tryMatchAnywhere(t: Term, r: Term, rl: ReactionRule, m: Match): Set[Match] = {
    if (GlobalCfg.DEBUG) {
      println("matcher::tryMatchAnywhere: " + m.toString())
    }

    var matches: Set[Match] = Set()
    var p: Term = t.next

    while (p != null) {
      if (GlobalCfg.DEBUG) {
        println("p: " + p.toString() + ": ")
      }
      if (p.parent == null || p.parent.activeContext) {
        if (GlobalCfg.DEBUG) {
          println("active")
        }
        var nm: Match = new Match(rl)
        nm.incorporate(m)
        if (m.parent != null) {
          nm.incorporate(m.parent);
        }
        matches = Match.merge(matches, tryMatch(p, r, nm))
      } else {
        if (GlobalCfg.DEBUG)
          println("passive")
      }

      p = t.next
    }
    t.reset
    return matches
  }

  def isCompat(m1: Match, m2: Match): Boolean = {
    if (m2.hasFailed) {
      return false
    }

    var m1n: Map[Name, Name] = m1.getNames
    var m2n: Map[Name, Name] = m2.getNames
    for ((key, value) <- m1n) {
      if (m2n.contains(key) && m2n(key) != value) {
        return false
      }
    }

    var mapping = m1.mapping
    for ((key, value) <- mapping) {
      if (m2.getMapping(key) != null && m2.getMapping(key) != value) {
        return false
      }
    }
    return true
  }

  def noOverlap(prev: List[Match], cand: Match): Boolean = {
    for (ite <- prev) {
      if (ite.root.overlap(cand.root) || cand.root.overlap(ite.root)) {
        return false
      }
    }
    return true
  }

  def crossprod(m1: Set[Match], m2: Set[Match]): Set[Match] = {
    // We have two sets {a,b,c} and {d,e,f}:
    // We want to construct:
    // {a.incorporate(d), a.incorporate(e), a.incorporate(f), b.inc...}
    if (GlobalCfg.DEBUG) {
      println("crossprod(): " + m1.size + " with " + m2.size)
    }

    var res: Set[Match] = Set()

    for (item1 <- m1.toList) {
      for (item2 <- m2.toList) {
        var item1Sbumatches = noOverlap(item1.asInstanceOf[WideMatch].submatches, item2)
        if (item1Sbumatches) {
          var nm: WideMatch = item1.asInstanceOf[WideMatch].clone
          nm.addSubMatch(item2)
          res.add(nm)
        }
      }
    }
    m1.clear()
    m2.clear()
    if (GlobalCfg.DEBUG) {
      println("crossprod(): res: " + res.size)
    }
    return res
  }

  /*  def getSubsOfParaller(children : Set[Term]) : Set[Term] = {
    var result = Set[Term]()
    var inner = Set[Term]()
    for(child <- children){
      if(child.termType != TermType.TPAR){
        result += child
      } else {
        inner = getSubsOfParaller(child.asInstanceOf[Paraller].getChildren)
        result ++= inner
      }
    }
    result
  }*/

  def tryMatchParallerPareller(t: Paraller, r: Paraller, m: Match): Set[Match] = {
    if (GlobalCfg.DEBUG) {
      println("matcher::tryMatchParallerPareller " + "matching: " + t.toString() + " against redex: " + r.toString())
    }

    // Term.getChildren is modified the same as C++
    var tch = t.getChildren //getSubsOfParaller(t.getChildren)
    var rch = r.getChildren //getSubsOfParaller(r.getChildren)

    var matches = Map[Term, Set[Match]]()
    // Is there a top level hole?  e.g. A | B | $0
    // Something like A.$0 | B | C does not count.
    // This will be the hole term itself, or NULL.
    var hasHole: Hole = null
    var isBreak = false

    /**
     * Filter holes and alert if has more than one
     */
    for (ite <- rch.toList) {
      if (ite.termType == TermType.THOLE) {
        if (hasHole != null)
          println("@Alert, a paraller has more than one site: " + r)
        else
          hasHole = ite.asInstanceOf[Hole]
        rch -= ite
      }
    }
    /**
     * If has hole and the left is a prefix,
     * we should leave it to the tryPrefixParaller
     *
     * if (hasHole != null && rch.size == 1 && m.root == null) {
     * return m.failure
     * }
     */

    if (hasHole == null && rch.size > tch.size) {
      return m.failure
    }

    /**
     * Bug Fix
     * @author liangwei
     * if the match paraller and paraller not the first time in,
     * t: A | B | C should not match r: A | B
     * But if it is the first time in,
     * t: A | B | C should match r: A | B
     * Here if m.root is not null, than it is not the first time in.
     */
    if (hasHole == null && m.root != null && rch.size != tch.size) {
      return m.failure
    }

    if (m.root == null && !(t.parent == null || (t.parent != null && t.parent.activeContext))) {
      return m.failure // We can't start a new match here.
    } else if (m.root == null) {
      m.root = t // We can start a new match here!
    }

    m.addMatch(r, t)

    for (iteR <- rch.toList) {
      var mcount = 0
      for (iteT <- tch.toList) {
        if (GlobalCfg.DEBUG) {
          println("Matching: " + iteR.toString() + " against " + iteT.toString())
        }
        var mn: Match = m.clone()
        var crossmatch: Set[Match] = tryMatch(iteT, iteR, mn)
        if (crossmatch.size > 0) {
          mcount += 1;
          if (matches.contains(iteR)) {
            matches(iteR) ++= crossmatch
          } else {
            matches(iteR) = crossmatch
          }
          if (GlobalCfg.DEBUG) {
            println(": matches " + crossmatch.size + " times")
          }
        } else {
          if (GlobalCfg.DEBUG) {
            println(": no match")
          }
        }
      }
      // We found nothing matching this part of the redex, so fail now.
      if (mcount == 0) {
        return m.failure
      }
    }

    if (GlobalCfg.DEBUG) {
      println("PARALLEL MATCH:")
      for ((key, value) <- matches) {
        println("REDEX PART: " + key.toString())
        println("MATCH SET: ")
        for (ite <- value.toList) {
          println(ite.toString())
        }
      }
    }

    //// by tanch matches 传到cand时 ctrlMap信息丢失

    var cand: Set[Match] = Set()
    for (ite <- rch.toList) {
      if (cand.size == 0) {
        if (ite != rch.toList.head) {
          return m.failure
        }
        //cand = matches(ite)
        matches(ite).map(x =>
          cand.add(x))
      } else {
        var ns: Set[Match] = matches(ite)
        var newcand: Set[Match] = Set()
        for (iteJ <- cand.toList) {
          for (iteK <- ns.toList) {
            if (isCompat(iteJ, iteK)) {
              var mm: Match = iteJ.clone()
              mm.root = m.root
              mm.incorporate(iteK)
              newcand += mm
            }
          }
        }
        cand = newcand
      }
    }

    if (cand.size == 0) {
      return m.failure
    }

    // OK, now we have to go through and populate the parameter
    // with everything that was not matched.
    if (hasHole != null || t.parent == null) {
      if (t.parent == null && hasHole == null) {
        hasHole = new Hole(999999);
      }
      for (iteI <- cand.toList) {
        var ctx: Set[Term] = Set()
        for (iteJ <- tch.toList) {
          if (iteI.getMapping(iteJ) == null) {
            ctx += iteJ
          }
        }
        if (ctx.size == 0) {
          iteI.addParam(hasHole.index, new Nil())
        } else if (ctx.size == 1) {
          iteI.addParam(hasHole.index, ctx.head)
        } else {
          //iteI.addParam(hasHole.index, new Paraller(ctx.head, ctx.tail.head))
          iteI.addParam(hasHole.index, Paraller.constuctParaller(ctx))
        }
      }
    }

    if (GlobalCfg.DEBUG) {
      println("PARALLEL MATCH: RESULT:")
      for (ite <- cand.toList) {
        println(ite.toString())
      }
    }
    return cand
  }
}

object testMatcher {
  def main(args: Array[String]) {
    println("--------------------Test crossprod function:--------------- ")
    var redexTest: Term = null //TermParser.apply("Pharmacy.(Patient[patient_prescription:edge,patient_bill_payed:edge,isDecoction:edge].IsDecoction[isDecoction:edge,value_is:edge,leftValue:edge].Value[value_is:edge] | Pill[idle] | $0) | Equal[leftValue:edge,rightValue:edge] | False[rightValue:edge] | $1");
    var reactumTest: Term = null //TermParser.apply("ChargingRoom.($0 | Patient[Patient_pill:edge,idle,isDecoction:edge].$2 | Pill[Patient_pill:edge]) | $1");
    var ruleA: ReactionRule = null //new ReactionRule(redexTest, reactumTest)
    var node: Term = null //TermParser.apply("a:Hospital.(Pill[patient_pill:edge].nil|b:Patient[patient_pill:edge,idle,isDecoction:edge].nil|c:ConsultingRoom.f:Computer[connected:edge].Prescription[patient_prescription:edge].nil|d:ChargingRoom.g:Computer[connected:edge].Bill[patient_bill_payed:edge].nil|e:Pharmacy.h:Computer[connected:edge].nil)")
    var matchA: Match = null //new Match(ruleA)

    //Matcher.tryMatchPrefixParaller(node.asInstanceOf[Prefix], redexTest.asInstanceOf[Paraller], matchA);
    //println(result)
    println("hole " + TermType.THOLE)
    println("nil " + TermType.TNIL)
    println("num " + TermType.TNUM)
    println("prefix " + TermType.TPREF)
    println("paraller " + TermType.TPAR)
    println("region " + TermType.TREGION)

    redexTest = TermParser.apply("a.b")
    reactumTest = TermParser.apply("a.c | b")
    node = TermParser.apply("a.b")
    val b: Bigraph = new Bigraph;
    b.root = node
    ruleA = new ReactionRule(redexTest, reactumTest)
    matchA = new Match(ruleA)
    var result = Matcher.tryMatchTermReactionRule(node, ruleA);
    println(result.size)
    result.foreach(f => {
      var nb = b.applyMatch(f)
      print(nb.root.toString())
    })

    /*
    var matchB = new Match(ruleA)
    var matchC = new WideMatch(ruleA)

    var nameA1: Name = new Name("nameA1", "innername")
    var nameA2: Name = new Name("nameA2", "innername")
    var nameA3: Name = new Name("nameA3", "innername")
    var nameA4: Name = new Name("nameA4", "innername")

    var nameB1: Name = new Name("nameB1", "innername")
    var nameB2: Name = new Name("nameB2", "innername")
    var nameB3: Name = new Name("nameB3", "innername")
    var nameB4: Name = new Name("nameB4", "innername")

    matchA.captureName(nameA1, nameA2)
    matchA.captureName(nameA3, nameA4)

    matchB.captureName(nameB1, nameB2)
    matchB.captureName(nameB3, nameB4)

    println("The value of matchA is: " + matchA.toString())
    println("The value of matchB is: " + matchB.toString())

    var matchTestSet: Set[Match] = Set(matchA, matchB)
    println("The set is: " + matchTestSet.toString())

    var crossResultSet = Matcher.crossprod(Match.singleton(matchC), matchTestSet)

    println("The crossResultSet is: " + crossResultSet.toString())

    var resListBuffer = new scala.collection.mutable.ListBuffer[Match]()
    resListBuffer += matchA
    println("The resListBuffer is: " + resListBuffer.toString())
    var set2Convert = scala.collection.mutable.Set(resListBuffer: _*)
    println("The set to set2Convert is: " + set2Convert.toString())
    
    var str : String = "abc12"
    var str2 : String = "abc"
//    println(str.startsWith(str2))
//    println(str.charAt(str2.size).<=('9') && str.charAt(str2.size).>=('0'))
    
    println("abc12 " + Matcher.getAlpPartFromStr(str))
    println("abc" + Matcher.getAlpPartFromStr(str2))
    */

  }
}