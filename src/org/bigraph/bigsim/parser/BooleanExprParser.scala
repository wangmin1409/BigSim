package org.bigraph.bigsim.parser

import org.bigraph.bigsim._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.JavaTokenParsers
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.utils._
import org.bigraph.bigsim.model.Term
import org.bigraph.bigsim.data.Data

class Query {
  override def toString = "<uninitialised query>";
  def check(): Boolean = {
    false;
  }
  def eval(): Double = {
    println("query.eval, Query object '" + toString + "' is of the Wrong type,  expected 'double', got 'bool'");
    0;
  }
}

class QueryNum(d: Double) extends Query {
  var value: Double = d;
  override def toString = value.toString;
  override def check() = {
    println("queryNum.check coercing int(" + value + ") to bool");
    value > 0;
  }
  override def eval() = {
    value;
  }
}

class QueryTerm(t: Term) extends Query {
  var data: Term = t;
  override def toString = data.toString;
  override def check() = {
    println("queryTerm.check  unexpected term in property");
    false;
  }
  override def eval() = {
    println("queryTerm.eval Type error: expected 'int', got 'term'");
    0;
  }
}

class QueryBin(l: Query, opr: String, r: Query) extends Query {
  var lhs: Query = l;
  var oper: String = opr;
  var rhs: Query = r;
  override def toString = lhs.toString + " " + oper + " " + rhs.toString;
  override def check() = {
    oper match {
      case "!=" => lhs.eval() != rhs.eval();
      case "==" => lhs.eval() == rhs.eval();
      case "<=" => lhs.eval() <= rhs.eval();
      case ">=" => lhs.eval() >= rhs.eval();
      case ">" => lhs.eval() > rhs.eval();
      case "<" => lhs.eval() < rhs.eval();
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
  override def check() = {
    lhs.check() && rhs.check();
  }
}

class QueryOr(l: Query, opr: String, r: Query) extends Query {
  var lhs: Query = l;
  var oper: String = opr;
  var rhs: Query = r;
  override def toString = lhs.toString + " " + oper + " " + rhs.toString;
  override def check() = {
    lhs.check() || rhs.check();
  }
}

class QueryNot(l: Query) extends Query {
  var lhs: Query = l;
  override def toString = "!" + lhs.toString;
  override def check() = !lhs.check();
}

/**
 * 谓词定义存在疑问，这里的规则暂时定义如下：
 * 谓词分为三类：
 * 1)二元操作符（&&、||逻辑操作符，！=、==、>=、<=、>、<算术操作符）谓词queryBin，
 * 又分为二元逻辑谓词（5 > 3 && 3 > 1 && 1 < 2，$pred->empty() || size() > $pred->size()等）
 * 和二元算术谓词（5>3等）
 * 该类谓词由
 * 2)内置谓词Pred
 * 3)含！表达式queryNot
 *
 * 除了内置谓词外，用括号表示优先级的表达式暂时不能处理
 * ！添加处理优先级表达式 @author liangwei
 */

object BooleanExprParser extends JavaTokenParsers {
  lazy val arithOp: Parser[String] = "!=" | "==" | ">=" | "<=" | ">" | "<";
  lazy val logicAnd: Parser[String] = "&&";
  lazy val logicOr: Parser[String] = "||";
  lazy val logicOp: Parser[String] = "&&" | "||";
  lazy val numericLit: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
  lazy val stringLit: Parser[Double] = """\w*""".r ^^ { _.hashCode.toDouble }
  lazy val variable: Parser[Double] = """(\d*)?+[a-zA-Z]+(\d*)?+(\.[a-zA-Z]*)?""".r ^^ { Data.getValue(_) }
  lazy val factor: Parser[Double] = "abs(" ~> expr <~ ")" ^^ { Math.abs(_) } | numericLit | variable | "(" ~> expr <~ ")"
  lazy val term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor | "%" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
      case (x, "%" ~ y) => x % y
    }
  }
  lazy val expr: Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) { case (x, "+" ~ y) => x + y; case (x, "-" ~ y) => x - y }
  }
  lazy val queryExpr: Parser[QueryNum] = expr ^^ { case x => new QueryNum(x) }
  lazy val queryStr: Parser[QueryNum] = "'" ~> stringLit <~ "'" ^^ { case x => new QueryNum(x) }
  lazy val queryBinArithWord: Parser[QueryNum] = queryExpr | queryStr
  lazy val queryNot: Parser[Query] = "!" ~> queryBinArithWord ^^ { x => new QueryNot(x) };
  lazy val queryBinArith: Parser[Query] = queryBinArithWord ~ arithOp ~ queryBinArithWord ^^ { case l ~ op ~ r => new QueryBin(l, op, r) }
  lazy val queryBinAndWord: Parser[Query] = queryBinArith | queryBinArithWord | queryNot;
  lazy val queryBinAnd: Parser[Query] = queryBinAndWord ~ logicAnd ~ queryBinAnd ^^ { case l ~ op ~ r => new QueryAnd(l, op, r); } |
    queryBinAndWord ~ logicAnd ~ queryBinAndWord ^^ { case l ~ op ~ r => new QueryAnd(l, op, r); }
  lazy val queryBinOrWord: Parser[Query] = queryBinAnd | queryBinArith | queryBinArithWord | queryNot;
  lazy val queryBinOr: Parser[Query] = queryBinOrWord ~ logicOr ~ queryBinOr ^^ { case l ~ op ~ r => new QueryOr(l, op, r); } |
    queryBinOrWord ~ logicOr ~ queryBinOrWord ^^ { case l ~ op ~ r => new QueryOr(l, op, r); }
  lazy val query: Parser[Query] = queryBinOr | queryBinAnd | queryBinArith | queryNot;

  def parse(s: String): Query = parseAll(query, s) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }
}

object testBooleanExprParser {

  def main(args: Array[String]) {
    println("Hello Scala MetaCalcParser!")

    Data.parseData("Examples/MobileCloud/data/checker.txt")

    var q: Query = BooleanExprParser.parse("m1.power>2 ");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 >= 3 || networkType == '3G' ");
    println(q + ", q.check: " + q.check());

    q = BooleanExprParser.parse("5 >= 3");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 <= 3");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3");
    println(q + ", q.check: " + q.check());

    q = BooleanExprParser.parse("5 > 3 && 3 > 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 && 3 > 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 && 3 < 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 && 3 > 1 && 2 < 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 && 3 > 1 && 1 < 2");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 && 1 > 3 && 1 < 2");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 || 3 > 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 || 3 > 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 || 3 < 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 || 3 > 1 || 2 < 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 || 3 > 1 || 2 < 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 || 3 < 1 || 2 < 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 5 && 3 > 1 || 1 < 2");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("1 < 2 || 3 > 5 && 3 > 1");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 || 3 > 1 && 1 < 2");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 || 3 < 1 && 1 > 2");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 || 3 < 1 && 1 > 2 || 6 < 7");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 || 3 < 1 && 1 > 2 || 6 > 7");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 && 3 < 1 || 1 > 2 && 6 > 7 ");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 < 3 && 3 < 1 || 1 < 2 && 6 < 7 ");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5 > 3 && 3 > 1 || 1 > 2 && 6 > 7 ");
    println(q + ", q.check: " + q.check());
    q = BooleanExprParser.parse("5%abs(3-7)==0");
    println(q + ", q.check: " + q.check());

  }
}