package org.bigraph.bigmc

import scala.collection.mutable.Map
import scala.collection.mutable.Set

import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.BRS.Matcher

object Predicate {
  var predicates: Map[String, Predicate] = Map();

  def registerPredicate(name: String, p: Predicate): Unit = {
    predicates(name) = p;
  }
  def getPredicate(name: String): Predicate = {
    if (!predicates.contains(name)) null;
    else predicates(name);
  }

  /**
   * registerPredicates
   * five predicates: empty, size, matches, terminal, equal
   */
  def registerPredicates: Unit = {
    Predicate.registerPredicate("empty", new PredEmpty());
    Predicate.registerPredicate("size", new PredSize());
    Predicate.registerPredicate("matches", new PredMatches());
    Predicate.registerPredicate("terminal", new PredTerminal());
    Predicate.registerPredicate("equal", new PredEqual());
  }

  def PredFailNoEval(x: String): Unit = {
    println("Predicate Type error: Pred " + x + " is of type 'bool', but is being used as type 'int'");
  }

  def PredFailNoCheck(x: String): Unit = {
    println("Predicate Type error: Pred " + x + " is of type 'int', but is being used as type 'bool'");
  }

  def PredFailParam(x: String, y: String, z: String): Unit = {
    println("Predicate Type error: Pred " + x + " is of type '"
      + y + "', but is being used as type '" + z + "'");
  }
}

class Predicate {
  def check(v: Vertex, params: List[Query]): Boolean = false;
  def eval(v: Vertex, params: List[Query]): Int = 0;
}

class PredEmpty extends Predicate {
  override def check(v: Vertex, params: List[Query]) = {
    v.bigraph.root.size == 0;
  }
  override def eval(v: Vertex, params: List[Query]) = {
    Predicate.PredFailNoEval("empty");
    0;
  }
}

class PredSize extends Predicate {
  override def check(v: Vertex, params: List[Query]) = {
    Predicate.PredFailNoCheck("size");
    false;
  }
  override def eval(v: Vertex, lparams: List[Query]) = {
    v.bigraph.root.size;
  }
}

class PredMatches extends Predicate {
  override def check(v: Vertex, params: List[Query]) = {
    eval(v, params) > 0;
  }
  override def eval(v: Vertex, params: List[Query]) = {
    if (params.size == 0 || params.size > 1) {
      Predicate.PredFailParam("matches", "term -> int", "_  -> int");
      0;
    } else {
      val t: QueryTerm = params.head.asInstanceOf[QueryTerm];
      if (t == null) {
        sys.exit(1);
         0;
      } else {
        val r: ReactionRule = new ReactionRule(t.data, null);
        val mt: Match = new Match(r);
        val m: Set[Match] = Matcher.tryMatchAnywhere(v.bigraph.root, t.data, r, mt);
        var sz: Int = m.size;
        mt.failure;
        m.map(x => x.failure);
        sz;
      }
    }
  }
}

class PredTerminal extends Predicate {
  override def check(v: Vertex, params: List[Query]) = {
    v.terminal;
  }
  override def eval(v: Vertex, params: List[Query]) = {
    if (v.terminal) 1;
    else 0;
  }
}

class PredEqual extends Predicate {
  override def check(v: Vertex, params: List[Query]) = {
    // todo 
    if (params.size == 0 || params.size > 1) {
      Predicate.PredFailParam("equal", "term -> bool", "_  -> bool");
      sys.exit(1)
    } else {
      var t: QueryTerm = params.head.asInstanceOf[QueryTerm];
      if (t == null) {
        Predicate.PredFailParam("equal", "term -> bool", "??? -> bool");
        sys.exit(1)
      }
      // C++ 版本的比较方法是将Term转化成字符串比较，这里直接比较字符串
      var s1: String = v.bigraph.root.toString;
      var s2: String = t.data.toString;
      s1 == s2;
    }
  }
  override def eval(v: Vertex, params: List[Query]) = {
    Predicate.PredFailNoEval("equal");
    0;
  }
}

  
