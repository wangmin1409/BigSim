package org.bigraph.bigsim.model

import java.io.File
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.bigsim.parser.BGMParser
import org.bigraph.bigsim.parser.BGMTerm
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.BRS.WideMatch
import org.bigraph.bigsim.BRS.Matcher
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.data.Data
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.simulator.Simulator

object Node {
  private var id: Int = 0
  def idIncrement: Int = {
    id += 1
    id
  }
}

class Node(n: String, act: Boolean, p: List[Name], c: Control, par: Node) {
  
  def this(n: String, act: Boolean, p: List[Name], c: Control) = this(n, act, p, c, null);
  
  val id: Int = Node.idIncrement
  var name = n
  var ports: List[Name] = p
  var active = act
  val ctrl = c
  var parent = par //add by lbj
  var hasChild:Boolean = false //add by lbj

  if (!ctrl.active)
    active = ctrl.active

  if (ctrl.arity == 0) //对只给名称的默认Control修正参数个数 
    ctrl.arity = ports.size;

  if (ports.size > c.arity) {
    println("Error: control " + Bigraph.controlToString(c) + " has arity" + c.arity
      + " but " + ports.size + " ports have been linked!");
    sys.exit(1);
  }

  def getNodeStr: String = {
    name + ":" + ctrl.name
  }
  
  def getNodeParent: String = {
    parent + ":" + parent.name
  }

  def getNodePortsStr: String = {
    "[" + ports.map(_.name).mkString(",") + "]"
  }

  def getMatchPortsCount(otherPorts: List[Name]): Int = {
    var m: Int = 0
    if (ports.size == otherPorts.size) {
      var index: Int = 0
      ports.foreach(p => {
        if (p.name.equals(otherPorts(index)))
          m += 1
        index += 1
      })
    }
    m
  }

  override def toString = "Node_" + id + ":(" + getNodeStr + getNodePortsStr + ")";
}

object Control {
  private var id: Int = 0;
  def idIncrement: Int = {
    id += 1;
    id;
  }
}

class Control(n: String, ar: Int, act: Boolean, s: PlaceSort) {
  val id: Int = Control.idIncrement;
  val name: String = n;
  var arity: Int = ar;
  var active: Boolean = act;
  var placeSort: PlaceSort = s;         
  var binding: Boolean = false;//add by lbj add binding control

  def this(n: String, ar: Int, act: Boolean) = this(n, ar, act, null);
  def this(n: String, ar: Int) = this(n, ar, true, null);
  def this(n: String) = this(n, 0, true, null);
  def this(n: String, ar: Int, act: Boolean, b: Boolean)={this(n, ar, act); binding = b;} 

  override def toString = "Control:(" + name + "," + id + "," + arity + "," + placeSort + ")";
}

object Name {
  private var id: Int = 0;
  def idIncrement: Int = {
    id += 1;
    id;
  }
}

class Name(n: String, nt: String) {//即port
  var name = n;
  var id = Name.idIncrement;
  var nameType = nt;//edge
  override def toString = "Name:(" + name + "," + id + "," + nameType + ")";//打印ports
}

// Bigraph is (V,E,ctrl,prnt,link) : <n,K> -> <m,L>
object Bigraph {
  var nameMap: Map[Pair[String, String], Name] = Map();
  var nodeMap: Map[String, Node] = Map();
  var controlMap: Map[String, Control] = Map();
  var modelNames: List[Name] = List();
//  var sorting: Sorting = new Sorting();

  def controlFromString(ctrlName: String): Control = {
    if (ctrlName == "")
      null;
    if (!controlMap.contains(ctrlName)) {
      val fresh: Control = new Control(ctrlName, 0);
      controlMap(ctrlName) = fresh;
    }
    controlMap(ctrlName);
  }

  def nameFromString(n: String, nt: String): Name = {
    var namePair = new Pair(n, nt);
    if (n == "")
      0;
    if (!nameMap.contains(namePair)) {
      val fresh: Name = new Name(n, nt);
      nameMap(namePair) = fresh;
    }
    nameMap(namePair);
  }

  def controlToString(c: Control): String = {
    for (entry <- controlMap.toList if entry._2 == c)
      return c.toString();
    return "<unknown control>";
  }

  def nameToString(n: Name): String = {
    /*		if (n == 0) return "-";
		for (entry <- nameMap.toList if entry._2 == n)
			return n.toString();
		return "<unknown name>";*/
    ""
  }

  def addControl(n: String, ar: Int, act: Boolean): Control = {
    val f: Control = new Control(n, ar, act);
    controlMap(n) = f;
    f;
  }
  
  def addControl(n: String, ar: Int, act: Boolean, binding: Boolean): Control = {//add by lbj
    val f: Control = new Control(n, ar, act, binding);
    controlMap(n) = f;
    f;
  }

  def isFree(n: Name): Boolean = {
    /*if (n== null || modelNames == null || modelNames.size == 0)
			false;
		else !modelNames.contains(n);*/
    /*else {
		  var names : Set[String] = Set()
		modelNames.map(ite => {
		  names.add(ite.name)
		})
		!names.contains(n.name)
		}*/

    true;
  }
 
}

class Bigraph(roots: Int = 1) {
  
  /*
   * yw added for verify
   */
  var linked:Bigraph = null;  //for verify
  var verifyID:String = "";
  
  
  var root: Term = null;
  var inner: Set[Name] = Set();
  var outer: Set[Name] = Set();
  var rules: Set[ReactionRule] = Set();
  
  //add by lbj
  var pattern: Term = null;//只有一个关注子模式
  var error: Term = null;//目前仅支持一个错误模式
  var bindings: Set[Binding] = Set();//每行binding是一个Binding
  var trackings: Set[Tracking] = Set();
//  var ports: Set[Port] = Set();
  var placeSorts: Set[PlaceSort] = Set();
  var linkSorts: Set[LinkSort] = Set();
  var placeSortConstraints: Set[String] = Set();
  var linkSortConstraints: Set[String] = Set(); 
  
  // add by wm
  var isInitial: Boolean = false;
  var isFinal: Boolean = false;
  var label: String = null;
  var isNegative: Boolean = false;
  
  
  def this() = this(1);

  def addOuterName(n: Name) = {
    outer.add(n);
    Bigraph.nameMap(new Pair(n.name, n.nameType)) = n;
  }

  def addInnerName(n: Name) = {
    inner.add(n);
    Bigraph.nameMap(new Pair(n.name, n.nameType)) = n;
  }

  def addRule(r: ReactionRule) = {
    rules.add(r);
  }
  
  //add by lbj
  def addBinding(b: Binding) = {
    bindings.add(b);
  }
  
  def addTracking(t: Tracking) = {
    trackings.add(t);
  }
  
//  def addPort(p: Port) = {
//    ports.add(p);
//  }
  
  def addplaceSort(s: PlaceSort) = {
    placeSorts.add(s);
  }
  
  def addlinkSort(s: LinkSort) = {
    linkSorts.add(s);
  }
  
  def addPlaceSortConstraints(t: String) = {
    placeSortConstraints.add(t);
  }
  
   def addLinkSortConstraints(t: String) = {
    linkSortConstraints.add(t);
  }


  /**
   * Find matches of one bigraph with a given reaction rule 当前bigraph与一个反应规则匹配
   */
  def findMatchesOfRR(rr: ReactionRule): Set[Match] = {
    var res: Set[Match] = Set();

    if (GlobalCfg.DEBUG)
      println("bigraph::find_matches(): redex: " + rr.redex.toString);
    /**
     * @author liangwei
     * add relation decision
     */
    var relations = true
    if (rr.data != null && GlobalCfg.checkData) {
      rr.data.map(relation => {
        relations = DataModel.relationDecision(relation)
      })
    }

    /**
     * @author liangwei
     * add RR conditions and realtions
     * relations:  Greater, Lesser and so on
     * conditions: whether check expression or not
     */

    if (relations) {
      var mp: Set[Match] = Matcher.tryMatchTermReactionRule(root, rr);
      mp.foreach(m => {
        if (rr.check(m)) {//如果cond过不去，这里rr虽然能匹配上但这里会被过滤掉不被加入res中
          res.add(m)
        }
      })
    }

    if (GlobalCfg.DEBUG) {
      println("Matches:");
      res.map(x => { println("*" + x + ":" + x.toString) });
    }
    res;
  }

  /**
   * Find matches of one bigraph with rules 当前bigraph与所有反应规则匹配
   */
  def findMatches: Set[Match] = {
    var res: Set[Match] = Set();

    rules.map(x => { //rules为所有反应规则

      if (GlobalCfg.DEBUG)
        println("bigraph::find_matches(): redex: " + x.redex.toString);
      /**
       * @author liangwei
       * add relation decision
       */
      var relations = true
      if (x.data != null && GlobalCfg.checkData) {
        x.data.map(relation => {
          relations = DataModel.relationDecision(relation)
        })
      }

      /**
       * @author liangwei
       * add HMM
       */
      /**
       * @author liangwei
       * add RR conditions and realtions
       * relations:  Greater, Lesser and so on
       * conditions: whether check expression or not
       */
      if (relations) {
        var mp: Set[Match] = Matcher.tryMatchTermReactionRule(root, x);
//        println("lbj---"+mp +"match set size:"+mp.size)
        mp.foreach(m => {
          if (x.check(m)) {//如果cond过不去，这里的x虽然能匹配上但这里会被过滤掉不被加入res中，例第一次就可以匹配r_takeoff这个规则，但因为不满足cond则不加入res中
            println("lbj---"+m +"m.rule:"+m.rule.name)
            res.add(m);//如果这条RR没有cond也能加进来，check函数能过
          }
        })
      }
    });

    if (GlobalCfg.DEBUG) {
      println("Matches:");
      res.map(x => { println("*" + x + ":" + x.toString) });
    }
    res;
  }

  def applyMatch(m: Match): Bigraph = {
    if (m == null) return this;
    if (!m.isWide) {
      var b: Bigraph = new Bigraph();
      b.root = root.applyMatch(m);
      b.inner = inner;
      b.outer = outer;
      b.rules = rules;
      b.trackings = trackings;
      b.bindings = bindings;
      b.placeSortConstraints = placeSortConstraints;
      b.placeSorts = placeSorts;
      b.linkSorts = linkSorts;
      b.linkSortConstraints = linkSortConstraints;
      b.pattern = pattern;

      Simulator.matchMarkDelete(m);
      return b;
    } else {
      if (m.rule.reactum.termType != TermType.TREGION) {
        println("bigraph::applyMatch Wide redexes must have wide reactums");
        sys.exit(1);
      }

      var reactum: Regions = m.rule.reactum.asInstanceOf[Regions];
      var wm: WideMatch = m.asInstanceOf[WideMatch];

      var b: Bigraph = new Bigraph();
      b.inner = inner;
      b.outer = outer;
      b.rules = rules;
      b.root = root;
      b.trackings = trackings;
      b.bindings = bindings;
      b.placeSortConstraints = placeSortConstraints;
      b.placeSorts = placeSorts;
      b.linkSorts = linkSorts;
      b.linkSortConstraints = linkSortConstraints;
      b.pattern = pattern;

      var mq: List[Match] = wm.submatches;
      var rc: List[Term] = List(); //reactum.getChildren;

      // 对于反应规则中包含nil根节点的, 如 A.B||nil -> A || B匹配处理, C++版本处理会出错,
      // 这里进行了修正
      var rRedex: List[Term] = m.rule.redex.asInstanceOf[Regions].getChildren;
      var rcRaw: List[Term] = reactum.getChildren;
      rRedex.map(x => {
        if (x.termType == TermType.TNIL) {
          // A.B||nil -> A||B 针对nil的处理方法：nil的生成物B放到反应物A中进行一块儿处理，在模型中，转化成A|B
          var last: Term = rc.last;
          rc = rc.init;
          rc = rc.:+(new Paraller(last.id, last, rcRaw.head))
        } else {
          rc = rc.:+(rcRaw.head);
        }
        rcRaw = rcRaw.tail;
      });
      // 对nil修正后，反应物子项数目应与子match数目一致
      if (rc.size != wm.submatches.size) {
        println("bigraph::applyMatch Wide rules must match in the number of regions in the redex and reactum");
        sys.exit(1);
      }

      mq.map(x => {
        var nr: ReactionRule = new ReactionRule(null, rc.head);
        rc = rc.tail;
        x.rule = nr;
        b.root = b.root.applyMatch(x);
      });

      Simulator.matchMarkDelete(m);
      return b;
    }
  }

  override def toString = {
    val s: StringBuffer = new StringBuffer();
    s.append("Bigraph:\n");
    s.append("nameMap:" + Bigraph.nameMap.toList + "\n");
    s.append("modelNames:" + Bigraph.modelNames + "\n");
    s.append("controlMap:" + Bigraph.controlMap.toList + "\n");
    //s.append("nRegions:" + nRegions + "\n");
    //s.append("nHoles:" + nHoles + "\n");
    s.append("inner name:" + inner + "\n");
    s.append("outer name" + outer + "\n");
    s.append("Rules:\n");
    for (rule <- rules if rule != null) {
      s.append("\t\t" + rule + "\n");
    }
    s.append("\tModel:\n\t\t" + root + "\n");
    s.append("IsStart:"+isInitial +"\n");
    s.append("IsFinal:"+isFinal+"\n");
    s.append("label:"+label+"\n");
    s.append("isNegative:"+isNegative+"\n")
    s.append("VerifyID"+ verifyID+"\n")
    s.toString();
  }
}

object testBigraph {
  def main(args: Array[String]) {
    // testControlFromString
    println("controlMap:" + Bigraph.controlMap.toList);

    // testNameFromString
    println(Bigraph.nameFromString("tanch", "innername"));
    println(Bigraph.nameFromString("zhaoxin", "innername"));
    println(Bigraph.nameFromString("chenjing", "innername"));
    println(Bigraph.nameFromString("lijingchen", "innername"));

    println("nameMap:" + Bigraph.nameMap.toList);

    // testApplyMatch
    val p = BGMParser.parse(new File("Examples/Airport_513/models/Smart.bgm"));
    println(p)
    val b: Bigraph = BGMTerm.toBigraph(p);
    println(b);
//    var mm:Set[Match]=b.findMatches();
    /**
     * 测试用例1
     * r = "%rule Acquire_a_left_fork P[lf,p,rf] || F[lf] -> P[lf,p,rf].F[lf] || Q[lf];"
     * b.root = "F[F1] | P[F1,P1,F2] | F[F2] | P[F2,P2,F3] | F[F3] | P[F3,P3,F4] | F[F4] |
     * P[F4,P4,F5] | F[F5] | P[F5,P5,F1]"
     *
     * 这里假设P[F1,P1,F2]与P[lf,p,rf]、F[F1]与F[lf] 匹配上，进行反应，检验与预期结果是否一致。
     */
    var r: ReactionRule = b.rules.filter(_.name match {
      case "Acquire_a_left_fork" => true;
      case _ => false;
    }).head;
    var m1: Match = new Match(r);
    var matchLeftTerm1: Term = b.root.asInstanceOf[Paraller].rightTerm.asInstanceOf[Paraller].leftTerm;
    var redexMatchTerm1: Term = r.redex.asInstanceOf[Regions].leftTerm;
    m1.root = matchLeftTerm1;
    m1.mapping += (matchLeftTerm1 -> redexMatchTerm1);
    m1.mapping += (matchLeftTerm1.asInstanceOf[Prefix].suffix -> redexMatchTerm1.asInstanceOf[Prefix].suffix);
    m1.hasSucceeded = true;
    m1.hasFailed = false;

    var m2: Match = new Match(r);
    var matchLeftTerm2: Term = b.root.asInstanceOf[Paraller].leftTerm;
    var redexMatchTerm2: Term = r.redex.asInstanceOf[Regions].rightTerm;
    m2.root = matchLeftTerm2;
    m2.mapping += (matchLeftTerm2 -> redexMatchTerm2);
    m2.mapping += (matchLeftTerm2.asInstanceOf[Prefix].suffix -> redexMatchTerm2.asInstanceOf[Prefix].suffix);
    m2.mapping += (matchLeftTerm1 -> redexMatchTerm1);
    m2.mapping += (matchLeftTerm1.asInstanceOf[Prefix].suffix -> redexMatchTerm1.asInstanceOf[Prefix].suffix);

    m2.hasSucceeded = true;
    m2.hasFailed = false;

    var m: WideMatch = new WideMatch(r);
    //m.names += (Bigraph.nameMap("lf") -> Bigraph.nameMap("F1"));
    //m.names += (Bigraph.nameMap("p") -> Bigraph.nameMap("P1"));
    //m.names += (Bigraph.nameMap("rf") -> Bigraph.nameMap("F2"));
    m.addSubMatch(m1);
    m.addSubMatch(m2);

    println("match, " + m);

    var b2: Bigraph = b.applyMatch(m);
    println("The original Node is:           " + b.root);
    println("After applyMatch, Node is:" + b2.root);

  }
}