package org.bigraph.bigsim.parser

/**
 * @author tanch
 * version 0.1
 */

/**
 * @author liangwei
 * version 0.2
 */

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.JavaTokenParsers
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.utils._
import org.bigraph.bigsim.model._

class Ident(val leftItem: String, val rightItem: String) {
  override def toString = leftItem + ":" + rightItem;
}

/**
 * 接收输入模型描述Term的标准化，没有书面正式的说明，这里调整与Term.scala里定义的一致
 *
 */
object TermParser extends StandardTokenParsers {
  lexical.delimiters ++= List(":", ".", ",", "$", "{", "}", "[", "]", "(", ")", "||", "|");
  lexical.reserved ++= List("nil");
  lazy val nodeWithCtrl = (ident ~ (":" ~> ident)) ^^ { case i ~ j => (new Ident(i, j)) } |
    ident ^^ { s => new Ident("", s) };
  lazy val nameWithType = (ident ~ (":" ~> ident)) ^^ { case i ~ j => (new Ident(i, "edge")) } |
    ident ^^ { s => (new Ident(s, "idle")) };
  lazy val hole = "$" ~> numericLit ^^ { b => new Hole(b.toInt) };
  lazy val nil = "nil" ^^^ { new Nil() };
  lazy val ctrl = (nodeWithCtrl ~ ("[" ~> nameList <~ "]")) ^^ { case i ~ n => (i, n) } | (nodeWithCtrl ^^ { s => (s, List()) });
  lazy val nameList: Parser[List[Ident]] = nameWithType ~ ("," ~> nameList) ^^ { case i ~ n => (i :: n) } | nameWithType ^^ (i => List(i));
  lazy val prefix: Parser[Term] = ctrl ~ ("." ~> ("(" ~> expr <~ ")")) ^^ {
    case (c, n) ~ s => new Prefix(new Node(c.leftItem, true, n.map(x => Bigraph.nameFromString(x.leftItem, x.rightItem)),
      Bigraph.controlFromString(c.rightItem)), s)
  } | ctrl ~ ("." ~> prefix) ^^ {
    case (c, n) ~ s => new Prefix(new Node(c.leftItem, true, n.map(x => Bigraph.nameFromString(x.leftItem, x.rightItem)),
      Bigraph.controlFromString(c.rightItem)), s)
  } | ctrl ^^ {
    case (c, n) => new Prefix(new Node(c.leftItem, true, n.map(x => Bigraph.nameFromString(x.leftItem, x.rightItem)),
      Bigraph.controlFromString(c.rightItem)), new Nil())
  } | nil | hole;
  lazy val terminal = hole | nil | prefix;
  lazy val paraller = terminal ~ ("|" ~> expr) ^^ { case leftTerm ~ rightTerm => new Paraller(leftTerm, rightTerm) };
  lazy val expr: Parser[Term] = paraller | terminal;
  lazy val regions = expr ~ ("||" ~> wexpr) ^^ { case leftTerm ~ rightTerm => new Regions(leftTerm, rightTerm) };
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

// 测试用例暂时不要删，版本完成的时候再处理
object testTermParser2 {

  def typeToString(termType: Int): String = {
    termType match {
      case TermType.TPREF => "Prefix";
      case TermType.TPAR => "Paraller";
      case TermType.THOLE => "Hole";
      case TermType.TNIL => "Nil";
      case TermType.TREGION => "Regions";
      case TermType.TNUM  => "Num";
      case _ => "Undefined TermType";
    }
  }

  def main(args: Array[String]) {
    println("Hello Scala MetaCalcParser!")
    var term = TermParser.apply("a:Hospital");
    // println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType))

      term = TermParser.apply("$3");
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
    	println("Ident:" + term + ",size=" + term.size + ", TermType:" + typeToString(term.termType));
  }
}