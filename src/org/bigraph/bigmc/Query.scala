package org.bigraph.bigmc

import org.bigraph.bigsim.model.Term
import org.bigraph.bigsim.BRS.Vertex

/**
 * @author tanch
 * version 0.1
 */


class Query {

  override def toString = "<uninitialised query>";

  def check(v: Vertex): Boolean = {
    false;
  }

  def eval(v: Vertex): Double = {
    println("query.eval, Query object '" + toString + "' is of the Wrong type,  expected 'int', got 'bool'");
    0;
  }
}

class QueryNum(d: Double) extends Query {
  var value: Double = d;

  //override def toString = "<int literal>";
  //override def toString = "<int literal: " + value.toString + ">";
  override def toString = value.toString;

  override def check(v: Vertex) = {
    println("queryNum.check coercing int(" + value + ") to bool");
    value > 0;
  }

  override def eval(v: Vertex) = {
    value;
  }
}

class QueryTerm(t: Term) extends Query {
  var data: Term = t;

  override def toString = data.toString;

  override def check(v: Vertex) = {
    println("queryTerm.check  unexpected term in property");
    false;
  }

  override def eval(v: Vertex) = {
    println("queryTerm.eval Type error: expected 'int', got 'term'");
    0;
  }
}

class QueryBin(l: Query, opr: String, r: Query) extends Query {
  var lhs: Query = l;
  var oper: String = opr;
  var rhs: Query = r;

  override def toString = lhs.toString + " " + oper + " " + rhs.toString;
  override def check(v: Vertex) = {
    oper match {
      case "!=" => lhs.eval(v) != rhs.eval(v);
      case "==" => lhs.eval(v) == rhs.eval(v);
      case "<=" => lhs.eval(v) <= rhs.eval(v);
      case ">=" => lhs.eval(v) >= rhs.eval(v);
      case ">" => lhs.eval(v) > rhs.eval(v);
      case "<" => lhs.eval(v) < rhs.eval(v);
      case _ => {
        println("QueyrBin.check unhandled operator" + toString);
        false;
      }
    }
  }
}

class QueryAnd(l: Query, opr: String, r: Query) extends Query {
  var lhs: Query = l;
  var oper: String = opr;
  var rhs: Query = r;
  override def toString = lhs.toString + " " + oper + " " + rhs.toString;
  override def check(v: Vertex) = {
    lhs.check(v) && rhs.check(v);
  }
}

class QueryOr(l: Query, opr: String, r: Query) extends Query {
  var lhs: Query = l;
  var oper: String = opr;
  var rhs: Query = r;
  override def toString = lhs.toString + " " + oper + " " + rhs.toString;
  override def check(v: Vertex) = {
    lhs.check(v) || rhs.check(v);
  }
}

class QueryNot(l: Query) extends Query {
  var lhs: Query = l;

  override def toString = "!" + lhs.toString;

  override def check(v: Vertex) = !lhs.check(v);
}

class QueryScope(nm: String, q: Query) extends Query {
  var name: String = nm;
  var qry: Query = q;

  override def toString = "$" + name + "->" + qry.toString;

  override def check(v: Vertex) = {
    name match {
      case "this" => qry.check(v);
      case "terminal" => {
        if (v.terminal) qry.check(v);
        else true;
      }
      case "pred" => {
        if (v.parent == null) true;
        else qry.check(v.parent);
      }
      case "succ" => {
        var result: Boolean = true;
        for (entry <- v.targets.toList if (!qry.check(entry._1)))
          result = false;
        result;
      }
      case _ => {
        println("QueyrScope.check Unknown meta-variable $" + name);
        false;
      }
    }
  }

  override def eval(v: Vertex) = {
    name match {
      case "this" => qry.eval(v);
      case "terminal" => {
        if (v.terminal) qry.eval(v);
        else {
          println("queryScope.eval using $terminal doesn't make much sense in this context.  Returning 0");
          0;
        }
      }
      case "pred" => {
        if (v.parent == null) 0;
        else qry.eval(v.parent);
      }
      case _ => {
        println("QueyrScope.eval Unknown meta-variable $" + name);
        0;
      }
    }
  }
}

class QueryPredicate(nm: String, l: List[Query]) extends Query {
  var name: String = nm;
  var params: List[Query] = l;
  var pred: Predicate = Predicate.getPredicate(name);
  //assert(pred != null);

  override def toString = {
    if (params.size == 0) name + "()";
    else name + "(" + l.mkString(",") + ")";
  }

  override def check(v: Vertex) = {
    pred.check(v, params);
  }

  override def eval(v: Vertex) = {
    pred.eval(v, params);
  }
}

