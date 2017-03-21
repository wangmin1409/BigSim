package org.bigraph.bigmc
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.utils._
import org.bigraph.bigsim.data.Data

/*
class Ident(val count: Int, val instance: String, val name: String) {
  def this(instance: String, name: String) = {
    this(1, instance, name); // count default is 1
  }

  override def toString = instance + ":" + name;
}

/**
 * 接收输入模型描述Term的标准化，没有书面正式的说明，这里调整与Term.scala里定义的一致
 *
 */

object TermParser extends StandardTokenParsers {
  /** add : for new format bgm like: [name:type]*/
  lexical.delimiters ++= List(":", ".", ",", "$", "@", "*", "+", "{", "}", "[", "]", "(", ")", "||", "|");
  lexical.reserved ++= List("nil");

  /**
   * newFormatCtrl中，后5行分别为@匹配，*匹配，
   */
  lazy val newFormatCtrl = (ident ~ (":" ~> ident)) ^^ { case i ~ j => (new Ident(i, j)) } |
    ident ^^ { s => new Ident("", s) } |
    ("{" ~> "a" ~> ("}" ~> ident)) ^^ { s => (new Ident(GlobalCfg.ALL, "", s)) } |
    ("{" ~> "*" ~> ("}" ~> ident)) ^^ { s => (new Ident(GlobalCfg.STAR, "", s)) } |
    ("{" ~> "+" ~> ("}" ~> ident)) ^^ { s => (new Ident(GlobalCfg.PLUS, "", s)) } |
    ("{" ~> numericLit ~ ("}" ~> (ident ~ (":" ~> ident)))) ^^ { case c ~ (i ~ j) => (new Ident(c.toInt, i, j)) } |
    ("{" ~> numericLit ~ ("}" ~> ident)) ^^ { case i ~ j => (new Ident(i.toInt, "", j)) };

  lazy val newFormatName = (ident ~ (":" ~> ident)) ^^ { case i ~ j => (new Ident(i, "edge")) } |
    ident ^^ { s => (new Ident(s, "idle")) };
  lazy val hole = "$" ~> numericLit ^^ { b => new Hole(b.toInt) };
  lazy val nil = "nil" ^^^ { new Nil() };
  lazy val ctrl = (newFormatCtrl ~ ("[" ~> nameList <~ "]")) ^^ { case i ~ n => (i, n) } | (newFormatCtrl ^^ { s => (s, List())});
  lazy val nameList: Parser[List[Ident]] = newFormatName ~ ("," ~> nameList) ^^ { case i ~ n => (i :: n) } | newFormatName ^^ (i => List(i));

  lazy val prefix: Parser[Term] = ctrl ~ ("." ~> ("(" ~> expr <~ ")")) ^^ {
    case (c, n) ~ s => new Prefix(new Node(c.instance, true, n.map(x => Bigraph.nameFromString(x.instance, x.name)),
      Bigraph.controlFromString(c.name)), s)
  } | ctrl ~ ("." ~> prefix) ^^ {
    case (c, n) ~ s => new Prefix(new Node(c.instance, true, n.map(x => Bigraph.nameFromString(x.instance, x.name)),
      Bigraph.controlFromString(c.name)), s)
  } | ctrl ^^ {
    case (c, n) => new Prefix(new Node(c.instance, true, n.map(x => Bigraph.nameFromString(x.instance, x.name)),
      Bigraph.controlFromString(c.name)), new Nil())
  } | nil | hole;

  lazy val terminal = hole | nil | prefix;
  lazy val paraller = terminal ~ ("|" ~> expr) ^^ { case e1 ~ e2 => new Paraller(e1, e2) };
  lazy val expr: Parser[Term] = paraller | terminal;
  lazy val regions = expr ~ ("||" ~> wexpr) ^^ { case e1 ~ e2 => new Regions(e1, e2) };
  lazy val wexpr: Parser[Term] = regions | expr;

  def parse(s: String) = {
    val tokens = new lexical.Scanner(s)
    phrase(wexpr)(tokens)
  }

  def apply(s: String): Term = {
    parse(s) match {
      case Success(tree, _) => tree;
      case e: NoSuccess => {
        Console.err.println(e);
        throw new IllegalArgumentException("Bad syntax: " + s);
      }
    }
  }
}
* 
*/

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
/*
object QueryParser extends JavaTokenParsers {
  lazy val arithOp: Parser[String] = "!=" | "==" | ">=" | "<=" | ">" | "<";
  lazy val logicAnd: Parser[String] = "&&";
  lazy val logicOr: Parser[String] = "||";
  lazy val logicOp: Parser[String] = "&&" | "||";
  lazy val predWord: Parser[String] = "empty" | "size" | "terminal" | "matches" | "equal";
  lazy val predArgs: Parser[List[Query]] = "(" ~> repsep(ident, ",") <~ ")" ^^ (x => {
    var pal: List[Query] = List();
    // todo fixme 参数类别predArgs可能调整
    x.map(xx => pal = pal.+:(new QueryTerm(TermParser.apply(xx))))
    //x.map(xx => pal = pal.+:(new QueryId(xx)));
    pal;
  })

  // For calculate data models and expressions @liangwei
  lazy val number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
  lazy val string: Parser[Double] = """\w*""".r ^^ { _.hashCode.toDouble }
  lazy val variable: Parser[Double] = """(\d*)?+[a-zA-Z]+(\d*)?+(\.[a-zA-Z]*)?""".r ^^ { DataModel.getValue(_) }
  lazy val factor: Parser[Double] = number | variable | "(" ~> expr <~ ")"
  lazy val term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }
  lazy val expr: Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) { // same as before, using alternate name for /:
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }
  lazy val queryExpr: Parser[QueryNum] = expr ^^ { case x => new QueryNum(x) }
  lazy val queryStr: Parser[QueryNum] = "'" ~> string <~ "'" ^^ { case x => new QueryNum(x) }

  lazy val queryPred: Parser[Query] = predWord ~ predArgs ^^ { case n ~ q => new QueryPredicate(n.toString, q) }

  lazy val queryScopeWord: Parser[String] = "pred" | "succ" | "terminal" | "this";
  lazy val queryScope: Parser[Query] = ("$" ~> queryScopeWord <~ "->") ~ queryPred ^^ { case n ~ q => new QueryScope(n.toString, q) }
  // queryNum is in queryExpr, so just use queryExpr @liangwei
  // lazy val queryNum: Parser[QueryNum] = decimalNumber ^^ { x => new QueryNum(x.toDouble); }
  lazy val queryBinArithWord = queryScope | queryPred | queryExpr | queryStr //| queryNum
  lazy val queryNot: Parser[Query] = "!" ~> queryBinArithWord ^^ { x => new QueryNot(x) };

  lazy val queryBinArith: Parser[Query] = queryBinArithWord ~ arithOp ~ queryBinArithWord ^^ { case l ~ op ~ r => new QueryBin(l, op, r) }

  lazy val queryBinAndWord: Parser[Query] = queryBinArith | queryBinArithWord | queryNot;
  lazy val queryBinAnd: Parser[Query] = queryBinAndWord ~ logicAnd ~ queryBinAnd ^^ { case l ~ op ~ r => new QueryAnd(l, op, r); } |
    queryBinAndWord ~ logicAnd ~ queryBinAndWord ^^ { case l ~ op ~ r => new QueryAnd(l, op, r); }

  lazy val queryBinOrWord: Parser[Query] = queryBinAnd | queryBinArith | queryBinArithWord | queryNot;
  lazy val queryBinOr: Parser[Query] = queryBinOrWord ~ logicOr ~ queryBinOr ^^ { case l ~ op ~ r => new QueryOr(l, op, r); } |
    queryBinOrWord ~ logicOr ~ queryBinOrWord ^^ { case l ~ op ~ r => new QueryOr(l, op, r); }
  lazy val query: Parser[Query] = queryBinOr | queryBinAnd | queryBinArith | queryBinArith | queryPred | queryNot;

  def parse(s: String): Query = parseAll(query, s) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }

}*/

// 测试用例暂时不要删，版本完成的时候再处理
object testTermParser {


  


  def main(args: Array[String]) {
    println("Hello Scala MetaCalcParser!")

    // test parser Property
    /* println(QueryParser.parse("terminal()"));
    println(QueryParser.parse("terminal(n)"))
    println(QueryParser.parse("terminal(l,m,n)"))

    println(QueryParser.parse("empty()"))
    println(QueryParser.parse("size()"))
    println(QueryParser.parse("matches(n)"))
    println(QueryParser.parse("equal(send)"))

    println(QueryParser.parse("!terminal()"))
    println(QueryParser.parse("!4")) */
    Data.parseData("MobileCloud/data/checker.txt")

    // from doc/example/*.bgm的例子测试
    /*println(QueryParser.parse("$pred->empty() || size() > $pred->size()"))
    	println(QueryParser.parse("!terminal()"))
    	println(QueryParser.parse("size() != $pred->size()"))
    	println(QueryParser.parse("!matches(n)"))
    	println(QueryParser.parse("!equal(send)"))*/

    // println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType))

    /*term = TermParser.apply("$3");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("nil");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("P[lf,p,rf]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("Zone[w,x].Pax[y] || Zone[x,z]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType))
    	
    	term = TermParser.apply("P[lf,p,rf].F[lf]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType))
    	
    	term = TermParser.apply("P[lf,p,rf].(F[lf]|F[rf]|M|N)");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType))
    	
    	term = TermParser.apply("IN.(L.Succ.Succ.Zero|R.Succ.Succ.Zero)|IN.(L.Succ.Zero|R.Succ.Succ.Zero)");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType))
    	
    	term = TermParser.apply("Zone[w,x].Pax[y] || Zone[x,z]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("Zone[w,x] || Zone[x,z].Pax[y]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("Landside.(Zone[placeHolder,CheckIn].Pax[Gian] | Zone[CheckIn,Security])" +
    	    "| Airside.( Zone[Security,GateLounge] | Zone[GateLounge,Boarding]) " + 
    	    " | Gates.(	Gate[SK100] | Gate[placeHolder] |	Gate[placeHolder])" + " | DB.(PaxRecord[Gian,SK100])");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	    
    	term = TermParser.apply("send[a].recv[b].recv[a]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("send[x].$0 | recv[x].$1");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("LEFT.(l.s.s.z | r.s.s.z)");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	term = TermParser.apply("F[F1] | P[F1,P1,F2] | F[F2] | P[F2,P2,F3] | F[F3] | P[F3,P3,F4] | F[F4] " +
    	    "| P[F4,P4,F5] | F[F5] | P[F5,P5,F1]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("P[lf,p,rf].(F[lf] | F[rf]) || Q[lf]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("P[lf1,p1,rf1].(M | F[lf1]) || Q[lf1]");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
    	
    	term = TermParser.apply("P[lf,rf].(F[lf]|F[rf]) || nil");
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));*/
  }
}