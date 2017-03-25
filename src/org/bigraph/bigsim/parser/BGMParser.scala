package org.bigraph.bigsim.parser

import java.io.File
import scala.util.parsing.combinator.RegexParsers
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.data.Data
import org.bigraph.bigsim.utils.GlobalCfg
import scala.collection.immutable.Map
import scala.collection.mutable.Set
import scala.xml.Null

abstract class BGMTerm {
}

//case class BGMControl(n: String, arity: Int, active: Boolean) extends BGMTerm
case class BGMControl(n: String, arity: Int, active: Boolean, binding: Boolean) extends BGMTerm //add binding control
case class BGMNode(n: String, active: Boolean, ctrl: BGMControl) extends BGMTerm
case class BGMName(n: String, isouter: Boolean) extends BGMTerm
case class BGMRule(n: String, redex: String, reactum: String, exp: String) extends BGMTerm
case class BGMAgent(n: String) extends BGMTerm
case class BGMProp(n: String, p: String) extends BGMTerm
case class BGMNil() extends BGMTerm

//add by lbj
case class BGMPattern(n: String) extends BGMTerm
case class BGMError(n: String) extends BGMTerm
case class BGMTracking(n: String, t: String) extends BGMTerm
case class BGMBinding(n: String) extends BGMTerm //bindingConstraint
//case class BGMPort(n: String,cn:String) extends BGMTerm
case class BGMPlaceSort(n: String, cns: String) extends BGMTerm
case class BGMLinkSort(n: String, pns: String) extends BGMTerm
case class BGMPlaceSortConstraint(n: String) extends BGMTerm
case class BGMLinkSortConstraint(n: String) extends BGMTerm

//add by wangmin
case class BGMFormula(n: String) extends BGMTerm
case class BGMProposition(n: String,content: String) extends BGMTerm


object BGMTerm {
  
  def parseFormula(t: List[BGMTerm]): String = {
    var formula: String = "";
    t.filter(_ match {
      case BGMFormula(_) => true
      case _             => false
    }).map(x => {
      val f: BGMFormula = x.asInstanceOf[BGMFormula];
      formula = f.n
    });
    return formula; 
  }
  
  def parseProposition(t: List[BGMTerm]): Map[String, Bigraph] = {
    var res: Map[String, Bigraph] = Map();
    t.filter(_ match {
      case BGMProposition(_,_) => true
      case _             => false
    }).map(x => {
      val p: BGMProposition = x.asInstanceOf[BGMProposition];
      res += (p.n -> proToBigraph(p.content));
    });
    return res; 
  }
  
  def proToBigraph(str: String): Bigraph = {
    var b: Bigraph = new Bigraph(1);
    b.root = TermParser.apply(str);
    return b;
  }
  
  
  def toBigraph(t: List[BGMTerm]): Bigraph = {
    val b: Bigraph = new Bigraph(1);
    // BGMControl
    val controlList: List[BGMControl] = t.filter(_ match {
      case BGMControl(_, _, _,_) => true
      case _                   => false
    }).map(_.asInstanceOf[BGMControl])
    controlList.map(x => Bigraph.addControl(x.n, x.arity, x.active, x.binding));

    // BGMName
    t.filter(_ match {
      case BGMName(_, _) => true
      case _             => false
    }).map(x => {
      val xo: BGMName = x.asInstanceOf[BGMName];
      if (xo.isouter) {
        b.addOuterName(new Name(xo.n, "outername"));
      } else {
        b.addInnerName(new Name(xo.n, "innername"));
      }

    });

    // 只读取一个model
    val agentList = t.filter(_ match {
      case BGMAgent(_) => true
      case _           => false
    }).head.asInstanceOf[BGMAgent];

    b.root = TermParser.apply(agentList.n);
    //println("root:"+b.root)//lbj

    // 目前认为Model中所有的name都不是free name
    //println("Model not free names:" + b.root.getAllNames);
    Bigraph.modelNames = b.root.getAllNames;

    // BGMRule
    t.filter(_ match {
      case BGMRule(_, _, _, _) => true
      case _                   => false
    }).map(rr => {
      val rrp = rr.asInstanceOf[BGMRule]
      val redex = TermParser.apply(rrp.redex);
      val reactum = TermParser.apply(rrp.reactum);
      b.rules.add(new ReactionRule(rrp.n, redex, reactum, rrp.exp)); //toBigraph时把反应规则解析到Bigraph
      println("rrp:" + rrp) //lbj
      println("redex:" + redex) //lbj
      println("reactum:" + reactum) //lbj
    });

    // 目前bigMC中没有声明的name全默认为inner name,只有%outer 说明的才是outer name，这里作一次修正
    Bigraph.nameMap.values.toList.map(x => b.inner.add(x));
    b.inner = b.inner.diff(b.outer);

    //BGMProp
    t.filter(_ match {
      case BGMProp(_, _) => true
      case _             => false
    }).map(p => {
      val pro = p.asInstanceOf[BGMProp];
      //MC.addProperty(pro.n, QueryParser.parse(pro.p))
      //      println("pro:"+pro)//lbj
    });

    val hasPattern = t.filter(_ match { //防止bgm文件中未定义报空指针
      case BGMPattern(_) => true
      case _             => false
    }).size > 0;

    //BGMPattern，只读取一个Pattern
    if (hasPattern) {
      GlobalCfg.checkPattern = true
      val patternList = t.filter(_ match {
        case BGMPattern(_) => true
        case _             => false
      }).head.asInstanceOf[BGMPattern];
      b.pattern = TermParser.apply(patternList.n);
    }

    val hasBinding = t.filter(_ match {
      case BGMBinding(_) => true
      case _             => false
    }).size > 0;

    // BGMBinding
    if (hasBinding) {
      GlobalCfg.checkBinding = true
      t.filter(_ match {
        case BGMBinding(_) => true
        case _             => false
      }).map(bb => {
        val binding = bb.asInstanceOf[BGMBinding]
        b.addBinding(new Binding(binding.n));
      })
    }

    val hasTracking = t.filter(_ match {
      case BGMTracking(_, _) => true
      case _                 => false
    }).size > 0;

    // BGMTracking
    if (hasTracking) {
      GlobalCfg.checkTracking = true
      t.filter(_ match {
        case BGMTracking(_, _) => true
        case _                 => false
      }).map(tt => {
        val tracking = tt.asInstanceOf[BGMTracking]
        b.addTracking(new Tracking(tracking.n, tracking.t, null)); //Exception或Record类型
      })
    }

    val hasPlaceSort = t.filter(_ match {
      case BGMPlaceSort(_, _) => true
      case _                  => false
    }).size > 0;

    // BGMPlaceSort
    if (hasPlaceSort) {
      GlobalCfg.checkSorting = true
      t.filter(_ match {
        case BGMPlaceSort(_, _) => true
        case _                  => false
      }).map(ps => {
        val placeSort = ps.asInstanceOf[BGMPlaceSort]
        var placeSortName: String = placeSort.n
        var aps: Array[String] = placeSort.cns.toString().split(",")
        var placeSet: Set[String] = Set()
        aps.map { x => placeSet = placeSet + x }
        var placeS: PlaceSort = new PlaceSort(); placeS.placeSortName = placeSortName; placeS.controlList = placeSet;
        b.addplaceSort(placeS)
      })
    }

    val hasLinkSort = t.filter(_ match {
      case BGMLinkSort(_, _) => true
      case _                 => false
    }).size > 0;

    // BGMLinkSort
    if (hasLinkSort) {
      GlobalCfg.checkSorting = true
      t.filter(_ match {
        case BGMLinkSort(_, _) => true
        case _                 => false
      }).map(ps => {
        val linkSort = ps.asInstanceOf[BGMLinkSort]
        var portList: Set[Port] = Set();
        var linkSortName: String = linkSort.n
        var aps: Array[String] = linkSort.pns.toString().split(",")
        aps.map { p =>
          {
            var ps: Array[String] = p.toString().split(":")
            var port: Port = new Port(ps(0), ps(1))
            portList = portList + port
          }
          var linkS: LinkSort = new LinkSort(); linkS.linkSortName = linkSortName; linkS.portList = portList;
          b.addlinkSort(linkS)
        }
      })
    }

    val hasPlaceSortConstraint = t.filter(_ match {
      case BGMPlaceSortConstraint(_) => true
      case _                         => false
    }).size > 0;

    // BGMPlaceSortConstraint
    if (hasPlaceSortConstraint) {
      t.filter(_ match {
        case BGMPlaceSortConstraint(_) => true
        case _                         => false
      }).map(ps => {
        val placeSortConstraint = ps.asInstanceOf[BGMPlaceSortConstraint]
        var placeSortConstraintStr: String = placeSortConstraint.n
        b.addPlaceSortConstraints(placeSortConstraintStr)
      })
    }

    val hasLinkSortConstraint = t.filter(_ match {
      case BGMLinkSortConstraint(_) => true
      case _                        => false
    }).size > 0;

    // BGMLinkSortConstraint
    if (hasLinkSortConstraint) {
      t.filter(_ match {
        case BGMLinkSortConstraint(_) => true
        case _                        => false
      }).map(ps => {
        val linkSortConstraint = ps.asInstanceOf[BGMLinkSortConstraint]
        var linkSortConstraintStr: String = linkSortConstraint.n
        b.addLinkSortConstraints(linkSortConstraintStr)
      })
    }

    b;
  }
}

object BGMParser extends RegexParsers {
  def exp = "[^;^{^}]*".r
  def ident = "[^ \t\n\r;]+".r
  def ws = "[ \t]*".r
  // 单行注释内容，解析时忽略
  def comment: Parser[String] = ".*".r;

  def EOL: Parser[String] = ws ~ ";" ~ ws ^^^ ""

  def stmt: Parser[BGMTerm] = "%active" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ {
    case i ~ a => BGMControl(i, a.toInt, true, false)
  } |
    "%passive" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ {
      case i ~ a => BGMControl(i, a.toInt, false, false)
    } |
    "%binding" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ { //add by lbj
      case i ~ a => BGMControl(i, a.toInt, true, true)
    } |
    "%outername" ~> (ws ~> ident) ^^ { x => BGMName(x, true) } |
    "%outer" ~> (ws ~> ident) ^^ { x => BGMName(x, true) } |
    "%innername" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%inner" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%name" ~> (ws ~> ident) ^^ { x => BGMName(x, false) } |
    "%rule" ~> (ws ~> ident ~ (ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ { //有约束条件的反应规则
      case i ~ s ~ k => {
        val bdrr = s.split("<-")
        val rr = s.split("->")
        if (bdrr.length > 1) { //add by lbj BackDerivation Rules
          GlobalCfg.isBackDerivation = true
          BGMRule(i, bdrr(1), bdrr(0), k)
        } else { //非回朔规则就认为是正常规则
          BGMRule(i, rr(0), rr(1), k)
        }
      }
    } |
    "%rule" ~> (ws ~> ident ~ (ws ~> exp)) ^^ { //没有约束条件的反应规则
      case i ~ s => {
        val bdrr = s.split("<-")
        val rr = s.split("->")
        if (bdrr.length > 1) { //add by lbj BackDerivation Rules
          GlobalCfg.isBackDerivation = true
          BGMRule(i, bdrr(1), bdrr(0), "")
        } else { //非回朔规则就认为是正常规则
          BGMRule(i, rr(0), rr(1), "")
        }
      }
    } |
    "%agent" ~> ((ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ { //有expr的agent
      case i ~ e => {
        Data.parseAgentExpr(e)
        BGMAgent(i)
      }
    } |
    "%agent" ~> (ws ~> exp) ^^ { x => BGMAgent(x) } | //没有expr的agent
    "%pattern" ~> (ws ~> exp) ^^ { x => BGMPattern(x) } | //关注子模式
    "%error" ~> (ws ~> exp) ^^ { x => BGMError(x) } | //关注子模式
    "%bindingConstraint" ~> (ws ~> exp) ^^ { x => BGMBinding(x) } | //绑定结构
    "%tracking ExceptionAssert:" ~> (ws ~> exp) ^^ { x => BGMTracking(x, "TrackException") } | //tracking异常断言
    "%tracking RecordAssert:" ~> (ws ~> exp) ^^ { x => BGMTracking(x, "TrackRecord") } | //tracking记录断言
    //    "%port" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> ident)) ^^ {
    //      case i ~ a => BGMPort(i, a)
    //    } |
    "%placeSort" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ {
      case i ~ s ~ k => {
        BGMPlaceSort(i, k)
      }
    } |
    "%linkSort" ~> (ws ~> ident ~ (ws ~> ":" ~> ws ~> exp) ~ ("{" ~> exp <~ "}")) ^^ {
      case i ~ s ~ k => {
        BGMLinkSort(i, k)
      }
    } |
    "%placeConstraint" ~> (ws ~> exp) ^^ { x => BGMPlaceSortConstraint(x) } |
    "%linkConstraint" ~> (ws ~> exp) ^^ { x => BGMLinkSortConstraint(x) } |
    "%property" ~> (ws ~> ident ~ (ws ~> exp)) ^^ { case i ~ p => BGMProp(i, p) } |
    "%placeConstraint" ~> (ws ~> exp) ^^ { x => BGMPlaceSortConstraint(x) } |
    "%formula" ~> (ws ~> exp) ^^ { x => BGMFormula(x) } | 
    "%proposition" ~> (ws ~> ident ~ (ws ~> exp)) ^^ {  
      case i ~ s => {
        BGMProposition(i,s)
      }
    } |
    "%check" ^^^ { BGMNil() } |
    "#" ~ comment ^^^ { BGMNil() }

  def stmtList: Parser[List[BGMTerm]] = stmt ~ (EOL ~> stmtList) ^^ { case x ~ xs => x :: xs } |
    stmt <~ EOL ^^ { x => x :: Nil } |
    stmt ~> stmtList ^^ { x => x }

  def parse(s: File): List[BGMTerm] = parseAll(stmtList, io.Source.fromFile(s).mkString) match {
    case Success(res, _) => res
    case e               => throw new Exception(e.toString)
  }

  def parseFromString(str: String): List[BGMTerm] = parseAll(stmtList, str) match {
    case Success(res, _) => res
    case e               => throw new Exception(e.toString)
  }
  
}

object testBGMParser {
  // for test
  def main(args: Array[String]) {
    //    println("My BGMParser!");
    //    val fileName: String = "Examples/MobileCloud/models/hotel.bgm";
    //    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    //    println("p:" + p);
    //    var b = BGMTerm.toBigraph(p);
    //    println("Bigraph:" + b);
    //    println("getAllNames:" + b.root.getAllNames);
    //
    //    b.rules.map(r => {
    //      println(r.name);
    //      println(r.reactum.getAllNames);
    //      println(r.redex.getAllNames);
    //      println();
    //    });

  /*  val fileName: String = "Examples/Airport_513/models/SmartJigWarehouseBackDerivation.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    var b = BGMTerm.toBigraph(p);
    println("Bigraph:" + b);
    println("getAllNames:" + b.root.getAllNames);

    b.rules.map(r => {
      println(r.name);
      println(r.reactum);
      println(r.redex);
      println();
    });*/

    //    var s = "test"
    //    val rr = s.split("->")
    //    println(rr(0))
    
    
    val fileName: String = "Examples/111/models/test20170323.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    var f = BGMTerm.parseFormula(p);
    var res = BGMTerm.parseProposition(p);
    println("formula:" + f);
    println("proposition:" + res);
  }
}


