package org.bigraph.bigsim.sorting

import scala.xml._
import scala.collection.mutable.Map
import org.bigraph.bigsim.parser.TermParser
import org.bigraph.bigsim.model.Paraller
import org.bigraph.bigsim.model.Term
import org.bigraph.bigsim.model.Regions
import org.bigraph.bigsim.model.Prefix
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.model.Bigraph

abstract class Sort1 {//应该是谭朝华学长实现的，解析xml文件找到placesort、linksort然后check
}

class PlaceSort1 extends Sort1
class LinkSort1 extends Sort1

class Sorting1 {
  var placeSort: List[String] = List();
  var linkSort: List[String] = List();
  /** Map[controlName, sortName] */
  var ctrlSortMap: Map[String, String] = Map();
  var linkSortMap: Map[Pair[String, Int], String] = Map();
  var inList: List[Pair[String, String]] = List();
  var notInList: List[Pair[String, String]] = List();
  var linkList: List[Pair[String, String]] = List();
  var notLinkList: List[Pair[String, String]] = List();
  var violationInfo: String = "";

  def this(fileName: String) = {
    this;
    init(fileName);
  }

  /**
   * reading sorting from xml file.
   */
  def init(file: String): Unit = {
    try {
      var sortingInfo: Elem = XML.load(file);//解析XML文件
      (sortingInfo \\ "placesort").map(node => {
        var sortName = (node \ "@name").toString;
        placeSort = placeSort.:+(sortName);
        (node \\ "control").map(controlNode => {
          var controlName = (controlNode \ "@name").toString();
          ctrlSortMap(controlName) = sortName;
        });
      });

      (sortingInfo \\ "linksort").map(node => {
        var sortName = (node \ "@name").toString;
        linkSort = linkSort.:+(sortName);
        (node \\ "port").map(portNode => {
          var controlName = (portNode \ "@control").toString();
          var index = (portNode \ "@index").toString.toInt;
          var pair = Pair(controlName, index);
          linkSortMap(pair) = sortName;
        });
      });

      (sortingInfo \\ "placeconstraint").map(node => {
        var name = (node \ "@name").toString;
        var parent = (node \ "@parent").toString;
        var child = (node \ "@child").toString;
        var pair = Pair(parent, child);
        if ("in" == name) inList = inList.:+(pair);
        else if ("notIn" == name) notInList = notInList.:+(pair);
        else println("Unknown placeConstraint name");
      });

      (sortingInfo \\ "linkconstraint").map(node => {
        var name = (node \ "@name").toString;
        var one = (node \ "@one").toString;
        var another = (node \ "@another").toString;
        var pair = Pair(one, another);
        if ("link" == name) linkList = linkList.:+(pair);
        else if ("notLink" == name) notLinkList = notLinkList.:+(pair);
        else println("Unknown linkConstraint name");
      });
    } catch {
      case ex: Exception => ;
    }
  }

  /**
   * check if a vertex satisfied sorting logic.检测一个中间偶图是否符合Sorting约束
   */
  def check(v: Vertex): Boolean = {
    if (v == null || v.bigraph == null || v.bigraph.root == null) {
      println("Node is null");
      return true;
    } else {
      var res = true;

      var t = v.bigraph.root;
      if (inList.size > 0) res = res && inCheck(t);
      if (notInList.size > 0) res = res && notInCheck(t);
      if (linkList.size > 0) res = res && linkCheck(t);
      if (notLinkList.size > 0) res = res && notLinkCheck(t);
      res
    }
  }

  /**
   * calculate the parent2child control pair
   */
  def getParentChildPair(t: Term): List[Pair[String, String]] = {
    var pcPairs: List[Pair[String, String]] = List();

    if (t.isInstanceOf[Prefix]) { // K.T or K.(T1|T2)
      var prefix = t.asInstanceOf[Prefix];
      var suffix = prefix.suffix;
      if (suffix.isInstanceOf[Prefix]) { // K.T
        var pair = Pair(prefix.node.ctrl.name, suffix.asInstanceOf[Prefix].node.ctrl.name);
        pcPairs = pcPairs.:+(pair);
      } else if (suffix.isInstanceOf[Paraller]) { // K.(T1|T2)
        var children = suffix.asInstanceOf[Paraller].getChildren;
        children.map(child => {
          if (child.isInstanceOf[Prefix]) { // 非nil 非hole
            var pair = Pair(prefix.node.ctrl.name, child.asInstanceOf[Prefix].node.ctrl.name);
            pcPairs = pcPairs.:+(pair);
          }
        });
      }
      pcPairs = pcPairs ::: getParentChildPair(suffix);
    } else if (t.isInstanceOf[Paraller]) {
      var children = t.asInstanceOf[Paraller].getChildren;
      children.map(child => {
        pcPairs = pcPairs ::: getParentChildPair(child);
      });
    } else if (t.isInstanceOf[Regions]) {
      var children = t.asInstanceOf[Regions].getChildren;
      children.map(child => {
        pcPairs = pcPairs ::: getParentChildPair(child);
      });
    }

    pcPairs.distinct;
  }

  /**
   * 内嵌关系判断，步骤：
   * 1.计算模型中所有具有内嵌关系的控制对；
   * 2.计算控制对中控制所对应的sort；
   * 3.验证控制对中的sort是否都在in描述的约束中，都在说明模型符合其约束，否则不符合。
   */
  def inCheck(t: Term): Boolean = {
    try {
      var pcPairs = getParentChildPair(t);
      pcPairs.map(pair => {
        var parentSort = ctrlSortMap(pair._1);
        var childSort = ctrlSortMap(pair._2);
        var sortPair = Pair(parentSort, childSort);
        if (!inList.contains(sortPair)) {
          violationInfo += pair._1 + "." + pair._2 + " does not accord with Placesort Constraint\"in\"!\n";
          return false;
        }
      });
    } catch {
      case ex: Exception => ;
    }
    true;
  }

  /**
   * 非内嵌关系判断，步骤：
   * 1.计算模型中所有具有内嵌关系的控制对；
   * 2.计算控制对中控制所对应的sort；
   * 3.检查控制对中的sort是否出现了notIn描述的约束中，若出现则说明模型不符合其约束，否则符合。
   */
  def notInCheck(t: Term): Boolean = {
    try {
      var pcPairs = getParentChildPair(t);
      pcPairs.map(pair => {
        var parentSort = ctrlSortMap(pair._1);
        var childSort = ctrlSortMap(pair._2);
        var sortPair = Pair(parentSort, childSort);
        if (notInList.contains(sortPair)) {
          violationInfo += pair._1 + "." + pair._2 + " does not accord with Placesort Constraint\"notIn\"!\n";
          return false;
        }
      });
    } catch {
      case ex: Exception => ;
    }
    true;
  }

  /**
   * 计算Term中的连接对，每个连接用偶图中的控制名和端口序号表示，即[ctrlName，index]。
   * 具有相同链接名称的控制是连在一起的，如K[F1,K1,F2]|F[F2]中K与F通过F2连接在一起，
   * 即Pair[Pair[K,3],Pair[F,1]]
   */
  def getLinkPair(t: Term): List[Pair[Pair[String, Int], Pair[String, Int]]] = {
    var lpPairs: List[Pair[Pair[String, Int], Pair[String, Int]]] = List();
    //List[Pair[ctrlName, ports]]
    var ctrlInstances: List[Pair[String, List[String]]] = getCtrlInstances(t);
    //Map[linkName, List[Pair[ctrlName, index]]]
    var lnName2ctrlPairs: Map[String, List[Pair[String, Int]]] = Map();

    ctrlInstances.map(pair => {
      var ctrlName = pair._1;
      var ports = pair._2;
      var index = 0;
      while (index < ports.size) {
        var ctrlIndexPair = Pair(ctrlName, index + 1);
        var lnName = ports(index);
        if (lnName2ctrlPairs.contains(lnName)) {
          lnName2ctrlPairs(lnName) = ctrlIndexPair :: lnName2ctrlPairs(lnName)
        } else {
          lnName2ctrlPairs(lnName) = ctrlIndexPair :: List();
        }
        index = index + 1;
      }
    });

    // 将每个链接的[控制名称,端口号]对 两两任意组合配对
    lnName2ctrlPairs.values.toList.map(lp => {
      var i = 0;
      var j = 0;
      while (i < lp.size) {
        j = i + 1;
        while (j < lp.size) {
          var pair = Pair(lp(i), lp(j));
          var symPair = Pair(lp(j), lp(i));
          if (!lpPairs.contains(pair) && !lpPairs.contains(symPair)) {
            lpPairs = lpPairs.:+(pair);
          }
          j = j + 1;
        }
        i = i + 1;
      }
    });
    lpPairs.distinct;
  }

  /**
   * 连接关系“白名单”检查
   */
  def linkCheck(t: Term): Boolean = {
    try {
      var lpPairs = getLinkPair(t);
      lpPairs.map(lp => {
        var oneSort = linkSortMap(lp._1);
        var anotherSort = linkSortMap(lp._2);
        var sortPair = Pair(oneSort, anotherSort);
        var symSortPair = Pair(anotherSort, oneSort);
        if (!linkList.contains(sortPair) && !linkList.contains(symSortPair)) {
          violationInfo += lp._1.toString + "." + lp._2.toString + " does not accord with Linksort Constraint\"link\"!\n";
          return false;
        }
      });
    } catch {
      case ex: Exception => ;
    }

    true;
  }

  /**
   * 连接关系“黑名单”检查
   */
  def notLinkCheck(t: Term): Boolean = {
    var lpPairs = getLinkPair(t);
    try {
      lpPairs.map(lp => {
        var oneSort = linkSortMap(lp._1);
        var anotherSort = linkSortMap(lp._2);
        var sortPair = Pair(oneSort, anotherSort);
        var symSortPair = Pair(anotherSort, oneSort);
        if (notLinkList.contains(sortPair) || notLinkList.contains(symSortPair)) {
          violationInfo += lp._1.toString + "." + lp._2.toString + " does not accord with Linksort Constraint\"notLink\"!\n";
          return false;
        }
      });
    } catch {
      case ex: Exception => ;
    }

    true;
  }

  /**
   * 计算Term的所有控制实例：控制名称+端口名称列表
   */
  def getCtrlInstances(t: Term): List[Pair[String, List[String]]] = {
    var ctrlInstances: List[Pair[String, List[String]]] = List();
    if (t.isInstanceOf[Prefix]) {
      var prefix = t.asInstanceOf[Prefix];
      if (prefix.node.ports.size > 0) { // K[n1,n2,...]
        var pair = Pair(prefix.node.ctrl.name, prefix.node.ports.map(_.name));
        ctrlInstances = ctrlInstances.:+(pair);
      }
      ctrlInstances = ctrlInstances ::: getCtrlInstances(prefix.suffix);
    } else if (t.isInstanceOf[Paraller]) {
      var children = t.asInstanceOf[Paraller].getChildren;
      children.map(child => {
        ctrlInstances = ctrlInstances ::: getCtrlInstances(child);
      });
    } else if (t.isInstanceOf[Regions]) {
      var children = t.asInstanceOf[Regions].getChildren;
      children.map(child => {
        ctrlInstances = ctrlInstances ::: getCtrlInstances(child);
      });
    }

    ctrlInstances;
  }

  override def toString = "placeSort:" + placeSort +
    "\nctrlSortMap:" + ctrlSortMap +
    "\nlinkSort:" + linkSort +
    "\nlinkSortMap:" + linkSortMap +
    "\ninList:" + inList +
    "\nnotInList:" + notInList +
    "\nlinkList:" + linkList +
    "\nnotLinkList:" + notLinkList;
}

object testSorting {
  def main(args: Array[String]) {
/**
    // test init
    var fileName: String = "Sortings/SortingConstraint.xml";
    var s0 = new Sorting(fileName);
    println("init:" + s0.toString + "\n");

    // test getParentChildPair
    var s = new Sorting();
    var t = TermParser.apply("P[lf,p,rf].(F[lf]|F[rf]|M|N)");
    println("Term:" + t + "\ngetParentChildPair:" + s.getParentChildPair(t) + "\n");

    var b = new Bigraph;
    b.root = t;
    var v = new Vertex(b, null, null);

    // test inCheck and notInCheck
    s.placeSort = List("P", "F", "M", "N");
    s.ctrlSortMap = Map("P" -> "P", "F" -> "F", "M" -> "M", "N" -> "N");
    s.inList = List(Pair("P", "F"), Pair("P", "M"));
    println("sorting:" + s + "\ninCheck:" + s.inCheck(t) + ",notInCheck:" + s.notInCheck(t) + ",check:" + s.check(v) + "\n");
    s.inList = s.inList.:+(Pair("P", "N"));
    println("sorting:" + s + "\ninCheck:" + s.inCheck(t) + ",notInCheck:" + s.notInCheck(t) + ",check:" + s.check(v) + "\n");

    s.notInList = List(Pair("P", "N"));
    println("sorting:" + s + "\ninCheck:" + s.inCheck(t) + ",notInCheck:" + s.notInCheck(t) + ",check:" + s.check(v) + "\n");

    // test getLinkPair
    t = TermParser.apply("F[F1] | P[F1,P1,F2] | F[F2] | P[F2,P2,F3] | F[F3] | " +
      "P[F3,P3,F4] | F[F4] | P[F4,P4,F5] | F[F5] | P[F5,P5,F1]");
    println("Term:" + t + "\ngetLinkPair:" + s.getLinkPair(t) + "\n")

    // test linkCheck and notLinkCheck
    s.linkSort = List("P", "F");
    s.linkSortMap = Map(Pair("F", 1) -> "F", Pair("P", 1) -> "P", Pair("P", 3) -> "P");
    s.linkList = List(Pair("P", "P"));
    println("sorting:" + s + "\nlinkCheck:" + s.linkCheck(t) + ",notLinkCheck:" + s.notLinkCheck(t) + ",check:" + s.check(v) + "\n");
    s.linkList = s.linkList.:+(Pair("P", "F"));
    println("sorting:" + s + "\nlinkCheck:" + s.linkCheck(t) + ",notLinkCheck:" + s.notLinkCheck(t) + ",check:" + s.check(v) + "\n");
    s.notLinkList = List(Pair("P", "F"));
    println("sorting:" + s + "\nlinkCheck:" + s.linkCheck(t) + ",notLinkCheck:" + s.notLinkCheck(t) + ",check:" + s.check(v) + "\n");
*/
  }
}