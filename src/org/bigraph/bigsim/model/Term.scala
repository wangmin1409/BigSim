package org.bigraph.bigsim.model

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import org.bigraph.bigsim.BRS._

/**
 * @author tanch
 * version 0.1
 */
object TermType {
  val TPREF: Int = 1;
  val TPAR: Int = 2;
  val THOLE: Int = 4;
  val TNIL: Int = 8;
  val TREGION: Int = 16;
  val TNUM: Int = 32;

  def typeToString(termType: Int): String = {
    termType match {
      case TermType.TPREF => "Prefix";
      case TermType.TPAR => "Paraller";
      case TermType.THOLE => "Hole";
      case TermType.TNIL => "Nil";
      case TermType.TREGION => "Regions";
      case TermType.TNUM => "Num";
      case _ => "Undefined TermType";
    }
  }
}
class T{
  def A = {
    var a = Term.uTermIncrement;
    a
  }
}

object Term {
  private var uTerm: Int = 1;

  def uTermIncrement: Long = {
    uTerm += 1;
    uTerm;
  }
}

class Term {
  var termType: Int = 0;
  /**
   * use Queue in Scala to replace the deque in C++
   * Queue.enqueue() to replace deque->push_back()
   * Queue.dequeue() to replace deque->pop_front
   */
  val remaining: Queue[Term] = Queue();
  remaining.enqueue(this);
  var parent: Term = null;
  var id: Long = Term.uTermIncrement;

  override def toString = TermString.preOrderString(this);

  def size: Int = 0;

  def next: Term = {
    if (remaining.size == 0) return null;
    val t: Term = remaining.dequeue();

    t.termType match {
      case TermType.TPREF => {
        // 递归定义的，过滤掉纯nil
        var tp: Term = t.asInstanceOf[Prefix].suffix;
        if (tp.termType != TermType.TNIL) {
          remaining.enqueue(tp);
        }
      }
      case TermType.TPAR => {
        val tp: Paraller = t.asInstanceOf[Paraller];
        tp.getChildren.map(remaining.enqueue(_));
      }
      case TermType.TREGION => {
        val tr: Regions = t.asInstanceOf[Regions];
        tr.getChildren.map(remaining.enqueue(_));
      }
      case TermType.THOLE => {};
      case TermType.TNIL => {};
      case _ => {
        println("Matching encountered invalid term type " + t.termType);
        sys.exit(1);
      }
    }
    t;
  }

  def reset: Unit = {
    remaining.clear();
    remaining.enqueue(this);
  }

  def activeContext: Boolean = {
    if (parent == null) true;
    else parent.activeContext;
  }

  def overlap(other: Term): Boolean = {
    if (other == null) false;
    else if (other == this) true;
    else overlap(other.parent);
  }

  def applyMatch(m: Match): Term = {
    this;
  }

  def instantiate(m: Match): Term = {
    null;
  }

  def getAllNames: List[Name] = List();

  //for test
  val remainingTest: Queue[Int] = Queue();
}

object Paraller {
  def constuctParaller(terms: Set[Term]): Paraller = {
    if (terms == null || terms.size < 2) {
      println("Error param terms to construct Paraller");
      sys.exit(1);
    } else if (terms.size == 2) {
      new Paraller(terms.head, terms.tail.head);
    } else {
      new Paraller(terms.head, constuctParaller(terms.tail))
    }
  }
}

// leftTerm | rightTerm
class Paraller(sid: Long, lt: Term, rt: Term) extends Term {
  termType = TermType.TPAR;
  id = sid;
  var leftTerm = lt;
  var rightTerm = rt;
  leftTerm.parent = this;
  rightTerm.parent = this;

  def this(lt: Term, rt: Term) = this(Term.uTermIncrement, lt, rt);

  override def size = leftTerm.size + rightTerm.size;

  def getChildren: Set[Term] = {
    var terms: Set[Term] = Set();
    if (leftTerm.termType == TermType.TPAR) {
      terms ++= leftTerm.asInstanceOf[Paraller].getChildren;
    } else {
      terms += leftTerm;
    }
    if (rightTerm.termType == TermType.TPAR) {
      terms ++= rightTerm.asInstanceOf[Paraller].getChildren;
    } else {
      terms += rightTerm;
    }
    terms;
  }

  override def applyMatch(m: Match) = {
    // todo 递归可能存在错误，需再仔细斟酌 m.root是否可能为空？
    if (id == m.root.id) {
      var r: Term = m.rule.reactum.instantiate(m);
      if (parent == null && m.getParam(999999) != null && r != null) {
        if (r.termType != TermType.TPAR) {
          r;
        } else {
          var rp: Term = r.asInstanceOf[Paraller];
          var ht: Term = m.getParam(999999).instantiate(null);
          if (ht.termType == TermType.TNIL) {
            rp;
          } else {
            new Paraller(id, rp, ht);
          }
        }
      } else {
        r;
      }
    } else {
      var lt: Term = leftTerm.applyMatch(m);
      var rt: Term = rightTerm.applyMatch(m);
      if (lt.termType == TermType.TNIL || rt.termType == TermType.TNIL) {
        if (lt.termType == TermType.TNIL) rt;
        else lt;
      } else {
        new Paraller(id, lt, rt);
      }
    }
  }

  override def instantiate(m: Match) = {
    // 注意过滤Nil
    var lt: Term = leftTerm.instantiate(m);
    var rt: Term = rightTerm.instantiate(m);
    if (lt.termType == TermType.TNIL || rt.termType == TermType.TNIL) {
      if (lt.termType != TermType.TNIL) lt;
      else if (rt.termType != TermType.TNIL) rt;
      else null;
    } else {
      new Paraller(lt, rt);
    }
    //c++版本的一个bug：这里不能带id——对于Term t1,Rule r的reactum调用instantiate的时候，其id会传给新的term t2（t2是t1的一部分），
    //若t1的其他部分还使用r进行反应，则instantiate的时候会导致t2部分也重新被instantiate,因为其id与math的id相同
    //new Paraller(id, lt, rt);
  }

  override def getAllNames = {
    leftTerm.getAllNames ++ rightTerm.getAllNames;
  }

}

// leftTerm || rightTerm
class Regions(sid: Long, lt: Term, rt: Term) extends Term {
  termType = TermType.TREGION;
  id = sid;
  var leftTerm = lt;
  var rightTerm = rt;
  leftTerm.parent = this;
  rightTerm.parent = this;

  def this(lt: Term, rt: Term) = this(Term.uTermIncrement, lt, rt);

  override def size = leftTerm.size + rightTerm.size;

  def getChildren: List[Term] = {
    var terms: List[Term] = List();
    if (leftTerm.termType == TermType.TREGION) {
      terms ++= leftTerm.asInstanceOf[Regions].getChildren;
    } else {
      terms = terms.:+(leftTerm);
    }

    if (rightTerm != null) {
      if (rightTerm.termType == TermType.TREGION) {
        terms ++= rightTerm.asInstanceOf[Regions].getChildren;
      } else {
        terms = terms.:+(rightTerm);
      }
    }
    terms;
  }

  override def applyMatch(m: Match) = {
    if (id == m.root.id) {
      m.rule.reactum.instantiate(m);
    } else {
      var lt: Term = leftTerm.applyMatch(m);
      var rt: Term = rightTerm.applyMatch(m);
      if (lt.termType == TermType.TNIL || rt.termType == TermType.TNIL) {
        if (lt.termType == TermType.TNIL) rt;
        else lt;
      } else {
        new Regions(id, lt, rt);
      }
    }
  }

  override def instantiate(m: Match) = {
    var lt: Term = leftTerm.instantiate(m);
    var rt: Term = rightTerm.instantiate(m);
    new Regions(lt, rt);
  }

  override def getAllNames = {
    leftTerm.getAllNames ++ rightTerm.getAllNames;
  }
}

// control[ports].term
class Prefix(sid: Long, n: Node, suff: Term) extends Term {
  //class Prefix(count: Int, sid: Long, c: Control, ports: List[Name], suff: Term) extends Term {
  val node: Node = n
  /**
   * use List in Scala to replace the vector in C++
   * because Name may be place holder for Prefix
   */
  var suffix: Term = suff;
  if (suffix != null) {
    suffix.parent = this;
  }
  termType = TermType.TPREF;

  id = sid;

  // multiple constructors
  def this(n: Node, suff: Term) =
    this(Term.uTermIncrement, n, suff);

  override def size = {
    1 + suffix.size;
  }

  override def activeContext = {
    if (parent == null) node.active;
    else node.active && parent.activeContext;
  }

  override def applyMatch(m: Match) = {
    if (id == m.root.id) {
      m.rule.reactum.instantiate(m);
    } else {
      new Prefix(id, node, suffix.applyMatch(m));
    }
  }

  override def instantiate(m: Match) = {
    if (m == null) {
      new Prefix(node, suffix.instantiate(m));
    } else {
      var nport: List[Name] = List();
      for (name <- node.ports) {
        nport = nport.:+(m.getName(name));
      }

      var modelNode = node;
      /*
      if(node.id == 272)
        println
        */
      if (m.rule.nodeMap.contains(node)) {
        var redexNode = m.rule.nodeMap(node);
        if (m.nodeMap.contains(redexNode))
          modelNode = m.nodeMap(redexNode);
      }
      modelNode.ports = nport
      new Prefix(modelNode, suffix.instantiate(m));
    }
  }

  override def getAllNames = {
    var names: List[Name] = List();
    node.ports.map(x => names = names.:+(x));
    if (suffix != null)
      names = names ++ suffix.getAllNames;
    names;
  }

}

class Hole(idx: Int) extends Term {
    
  val index: Int = idx;
  termType = TermType.THOLE;
  id = Term.uTermIncrement;
  //add by lbj
  var parNode: Node=null

  override def size = 1;

  override def applyMatch(m: Match) = {
    null;
  }

  override def instantiate(m: Match) = {
    // todo
    if (m == null) {
      println("ERROR: hole::apply_match(): Invalid place graph contains a hole.");
      sys.exit(1);
    }
    var t: Term = m.getParam(index);

    if (t == null) t = new Nil();

    t.instantiate(null);
  }
}

class Num(v: Int) extends Term {
  val value: Int = v;
  termType = TermType.TNUM;
  id = Term.uTermIncrement;

  override def toString = "Int:" + value;

  override def size = 1;

  override def applyMatch(m: Match) = {
    if (id == m.root.id) {
      m.rule.reactum.instantiate(m);
    } else {
      this;
    }
  }

  override def instantiate(m: Match) = {
    this;
  }
}

class Nil extends Term {
  termType = TermType.TNIL;
  id = Term.uTermIncrement;
  override def size = 0;

  override def applyMatch(m: Match) = {
    new Nil();
  }

  override def instantiate(m: Match) = {
    new Nil();
  }
}

object testTerm {
  def main(args: Array[String]) {
    print("Hello SCALA World in Term.scala \n")

    var test = new Term();

    println("test scala Queue:");
    println(test.remainingTest.toList);

    test.remainingTest.enqueue(1);
    test.remainingTest.enqueue(2);
    test.remainingTest.enqueue(3);
    test.remainingTest.enqueue(4);
    println(test.remainingTest.toList);

    println(test.remainingTest.dequeue());

    println(test.remainingTest.toList);

    test.remainingTest.enqueue(5);
    test.remainingTest.enqueue(6);
    test.remainingTest.enqueue(7);

    println(test.remainingTest.dequeue());

    println(test.remainingTest.toList);

    println("test Singleton BGMTerm.increase():");
    println(test.id);
    println(new Term().id);
    println(new Term().id);

    var s: StringBuffer = new StringBuffer;
    s.append("(akdlad|dklakd)");
    println(s.toString());
    println(s.length);
    println(s.deleteCharAt(s.length - 1));
    println(s.length);
    println(s.delete(s.length - 2, s.length));
    println(s.length);

    type myInt = Int;
    type myFloat = Float;
    val a: myInt = 2;
    val b: myFloat = 2.3333F;
    println(a);
    println(b);

    // visit map
    val controlMap: Map[String, Int] = Map();
    /*controlMap("tanch") = 1;
		controlMap("zhaoxing") = 2;
		controlMap("chenjing") = 3;*/
    controlMap += ("tanch" -> 1);
    controlMap += ("zhaoxin" -> 2);
    controlMap += ("chenjing" -> 3);
    println("test Map:" + controlMap.toList);

    for (value <- controlMap.toList) {
      if (value._2 == 1)
        println(value._1);
    }

    // visit List
    var tl: List[Int] = List();
    println("visit List empty:" + tl.mkString(" "));
    tl = tl.:+(1);
    tl = tl.:+(2);
    tl = tl.:+(3);
    tl = tl.:+(4);
    tl = tl.:+(3);
    tl = tl.:+(5);
    tl = tl.:+(6);
    tl.:+(7);
    println("visit List:" + tl)
    println("visit List i:" + tl(2))
    println("visit List distinct:" + tl.distinct);
    println("visit List head:" + tl.head);
    println("visit List tl.remove(x=> (x == tl.head)):" + tl.filter(x => (x != tl.head)));
    println("visit List tl:" + tl)
    //tl = tl.-(3);
    println("visit List tl = tl.-(3):" + tl);
    println("visit List tl.tail:" + tl.tail)
    tl = tl.sortWith((a: Int, b: Int) => a > b);
    println("visit List tl.sort:" + tl.mkString("	"));

    var tl2: List[Int] = List(20, 30, 40)
    println("tl2:" + tl2);
    println("tl::tl2##:" + tl :: tl2)
    tl = tl2 ::: tl
    println("tl.:::(tl2)##:" + tl.:::(tl2))

    // visit set;
    var ts: Set[Int] = Set();
    ts += 1;
    ts += 2;
    ts = ts + 3;
    ts = ts + 4;
    ts = ts + 3;
    ts = ts + 5;
    ts = ts + 6;
    println("visit set:" + ts);
    ts.clear();
    println("visit set(after clear):" + ts);

    var ts2: Set[Int] = Set();
    var ts3: Set[Int] = Set();
    ts2.add(1); ts2.add(2); ts2.add(3); ts2.add(4);
    ts2.add(3); ts2.add(5); ts2.add(6);
    println("visit mutable set ts2:" + ts2);
    ts3.add(1); ts3.add(4); ts3.add(6); ts3.add(7); ts3.add(8);
    println("visit mutable set ts3:" + ts3);
    ts2 = ts2.diff(ts3);
    println("visit mutable ts2.diff(ts3):" + ts2);
    println("visit mutable ts2.intersect(ts3):" + ts2.intersect(ts3));
    ts2 = ts2 ++ ts3;
    println("visit mutable ts2 = ts2++(ts3):" + ts2);

    println("visit mutable ts2.remove(2):" + ts2.remove(2) + ", ts2:" + ts2);
    println("visit mutable ts2.remove(3):" + ts2.remove(3) + ", ts2:" + ts2);

    ts2 = ts2.drop(3);
    println("visit mutable ts2 = ts2.drop(3):" + ts2);

    // visit vector;
    var tv: Vector[Int] = Vector();
    tv = tv.:+(1);
    tv = tv.:+(2);
    tv = tv.:+(3);
    tv = tv.:+(4);
    tv = tv.:+(3);
    tv = tv.:+(5);
    tv = tv.:+(6);
    tv.:+(7);
    println("visit vector:" + tv);

    //test null
    val termMap: Map[String, Term] = Map();
    var term = new Term();
    var term1 = new Hole(1);
    var term2 = new Hole(2);
    term1.parent = term;
    term2.parent = term1;

    println("term:" + term);
    println("term1:" + term1);
    println("term2:" + term2);
    println("term.parent:" + term.parent);
    println("term1.parent:" + term1.parent);
    println("term2.parent:" + term2.parent);

    if (term.parent == null) {
      println("term.parent is null now");
    }
    if (term.parent != null) {
      println("term is not null now:" + term.parent);
    }
    termMap("tanch") = new Hole(100);
    term.parent = termMap("tanch");
    if (term.parent == null) {
      println("term.parent is null now");
    }
    if (term.parent != null) {
      println("term.parent is not null now:" + term.parent);
    }

    term = null;
    term = termMap("tanch");
    if (term == null) {
      println("term is null now");
    }
    if (term != null) {
      println("term is not null now:" + term);
    }
  }
}