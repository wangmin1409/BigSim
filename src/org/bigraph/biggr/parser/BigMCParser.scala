package org.bigraph.biggr.parser

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.RegexParsers

import org.bigraph.biggr.bigraph.Control
import org.bigraph.biggr.model.Model
import org.bigraph.biggr.model.ReactionRule
import org.bigraph.biggr.simulator.SimGr

abstract class BGMTerm {
}

case class BGMControl(n: String, arity: Int, active: Boolean) extends BGMTerm
case class BGMNode(n: String, active: Boolean, ctrl: BGMControl) extends BGMTerm
case class BGMName(n: String, isouter: Boolean) extends BGMTerm
case class BGMRule(n: String, redex: String, reactum: String, exp: String) extends BGMTerm
case class BGMAgent(n: String) extends BGMTerm
case class BGMProp(n: String, p: String) extends BGMTerm
case class BGMNil() extends BGMTerm

object BGMTerm {
  def toBigraph(t: List[BGMTerm]): Model = {
   
    // BGMControl
    val controlList: List[BGMControl] = t.filter(_ match {
      case BGMControl(_, _, _) => true
      case _ => false
    }).map(_.asInstanceOf[BGMControl])
    val controls = ListBuffer[Control]()
    controlList.foreach(x =>{
      val c = new Control(x.n,x.arity)
      controls.append(c) 
      InterResForModel.nodenameToControl(x.n) = c
    })
    // BGMName
    /*
    t.filter(_ match {
      case BGMName(_, _) => true
      case _ => false
    }).map(x => {
      val xo: BGMName = x.asInstanceOf[BGMName];
      if (xo.isouter) {
        b.addOuterName(new Name(xo.n, "outername"));
      } else {
        b.addInnerName(new Name(xo.n, "innername"));
      }

    });
    */

    val agentList = t.filter(_ match {
      case BGMAgent(_) => true
      case _ => false
    }).head.asInstanceOf[BGMAgent];

    val p4 = new ModelParser1()
    //val agent = TermParser.apply(agentList.n);
    val agent = p4.parseAll(p4.agent,agentList.n).get 
 println(agentList.n)
    println(agent.toString)
    
    val rules = ListBuffer[ReactionRule]() 
    // BGMRule
    t.filter(_ match {
      case BGMRule(_, _, _, _) => true
      case _ => false
    }).map(rr => {
      val rrp = rr.asInstanceOf[BGMRule]
      val predex = new ModelParser1()
      val redex = predex.parseAll(predex.agent,rrp.redex).get 
      //println("redex"+rrp.redex)
      //val redex = TermParser.apply(rrp.redex);
      val preactum = new ModelParser1()
      //println("reactum"+rrp.reactum)
      val reactum = preactum.parseAll(preactum.agent,rrp.reactum).get 
      //val reactum = TermParser.apply(rrp.reactum);
      val reac = new ReactionRule(redex, null,reactum)
      reac.name = rrp.n
      reac.expression = rrp.exp
      rules.append(reac);
      //println("ReactionRule:" + reac.toString());
    });

    val model = new Model(controls.toList,null,rules.toList,agent,null)
    
    //Bigraph.nameMap.values.toList.map(x => b.inner.add(x));
    //b.inner = b.inner.diff(b.outer);

    //BGMProp
    /*
    t.filter(_ match {
      case BGMProp(_, _) => true
      case _ => false
    }).map(p => {
      val pro = p.asInstanceOf[BGMProp];
      //MC.addProperty(pro.n, QueryParser.parse(pro.p))
    })
    */
    model;
  }
}

object BigMCParser extends RegexParsers {
  def exp = "[^;^{^}]*".r
  def ident = "[^ \t\n\r;]+".r
  def ws = "[ \t]*".r
  
  def comment: Parser[String] = ".*".r;

  def EOL: Parser[String] = ws ~ ";" ~ ws ^^^ ""

  def stmt: Parser[BGMTerm] = "%active" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ {
      case i ~ a => BGMControl(i, a.toInt, true) } |
    "%passive" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ {
      case i ~ a => BGMControl(i, a.toInt, false) } |
    "%outername" ~> (ws ~> ident) ^^ { x => BGMName(x, true) } |
    "%outer" ~> (ws ~> ident) ^^ { x => BGMName(x, true) } |
    "%innername" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%inner" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%name" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%rule" ~> (ws ~> ident ~ (ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ {
      case i ~ s ~ k => {
        val rr = s.split("->")
        BGMRule(i, rr(0), rr(1), k) }} |
    "%rule" ~> (ws ~> ident ~ (ws ~> exp)) ^^ {
      case i ~ s => {
        val rr = s.split("->")
        BGMRule(i, rr(0), rr(1), "") }} |
    "%agent" ~> ((ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ {
      case i ~ e => {
        BGMAgent(i) }} |
    "%agent" ~> (ws ~> exp) ^^ { x => BGMAgent(x) } |
    "%property" ~> (ws ~> ident ~ (ws ~> exp)) ^^ { case i ~ p => BGMProp(i, p) } |
    "%check" ^^^ { BGMNil() } |
    "#" ~ comment ^^^ { BGMNil() }

  def stmtList: Parser[List[BGMTerm]] = stmt ~ (EOL ~> stmtList) ^^ { case x ~ xs => x :: xs } |
    stmt <~ EOL ^^ { x => x :: Nil } |
    stmt ~> stmtList ^^ { x => x }

  def parse(s: File): List[BGMTerm] = parseAll(stmtList, io.Source.fromFile(s).mkString) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }

  def parseFromString(str: String): List[BGMTerm] = parseAll(stmtList, str) match {
    case Success(res, _) => res
    case e => throw new Exception(e.toString)
  }
}

object testBigMCParser {
  def main(args: Array[String]) {
    val fileName: String = "Examples/BusinessDisable2.bgm";
    val p: List[BGMTerm] = BigMCParser.parse(new File(fileName));
    val model = BGMTerm.toBigraph(p);
    //val sim = new Simulation(model)
    //val gg = sim.generateLTS
    val sim = new SimGr(model)
    sim.simulate()
    printf(sim.toString)
  }
}


