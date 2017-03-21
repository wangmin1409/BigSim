package org.bigraph.bigsim.model

/**
 * @author libaijie
 */
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.bigsim.BRS.Vertex

class Sort {
}

class LinkSort extends Sort {
  var linkSortName: String = "";
  var portList: Set[Port] = Set();
}

class PlaceSort extends Sort {
  var placeSortName: String = "";
  var controlList: Set[String] = Set();
}

class Sorting {

  def this(b: Bigraph) = {
    this;
    init(b);
  }
  var placeSorts: Set[PlaceSort] = Set()
  var linkSorts: Set[LinkSort] = Set()
  var placeSortConstraints: Set[String] = Set()
  var linkSortConstraints: Set[String] = Set()

  var ctrlSortMap: Map[String, String] = Map(); //key为control，value为sort
  var linkSortMap: Map[Pair[String, String], String] = Map(); //key为(portname,control)，value为sort

  var inList: List[Pair[String, String]] = List(); //placeSort1必须在placeSort2里面
  var notInList: List[Pair[String, String]] = List();
  var linkList: List[Pair[String, String]] = List();
  var notLinkList: List[Pair[String, String]] = List();
  var violationInfo: String = "";

  def init(b: Bigraph): Unit = {
    placeSorts = b.placeSorts
    linkSorts = b.linkSorts
    placeSortConstraints = b.placeSortConstraints
    linkSortConstraints = b.linkSortConstraints

    placeSortConstraints.map { x =>
      {
        var placeStr: Array[String] = x.toString().split(" ");
        if ("in".equals(placeStr(1)) && placeStr.size == 3) {
          var pair = Pair(placeStr(0), placeStr(2));
          inList = inList.:+(pair)
        } else if ("notIn".equals(placeStr(1)) && placeStr.size == 3) {
          var pair = Pair(placeStr(0), placeStr(2));
          notInList = notInList.:+(pair);
        }
      }
    }

    linkSortConstraints.map { x =>
      {
        var linkStr: Array[String] = x.toString().split(" ");
        if ("link".equals(linkStr(1)) && linkStr.size == 3) {
          var pair = Pair(linkStr(0), linkStr(2));
          linkList = linkList.:+(pair);
        } else if ("notLink".equals(linkStr(1)) && linkStr.size == 3) {
          var pair = Pair(linkStr(0), linkStr(2));
          notLinkList = notLinkList.:+(pair);
        }
      }
    }
    placeSorts.map { x =>
      {
        var placeSortName = x.placeSortName
        x.controlList.map { ctrl => { ctrlSortMap(ctrl) = placeSortName; } }
      }
    }

    linkSorts.map { x =>
      {
        var linkSortName = x.linkSortName
        x.portList.map { port => { linkSortMap(Pair(port.name, port.control)) = linkSortName; } }
      }
    }
  }

  /**
   * check if a vertex satisfied sorting constraints
   */
  def check(v: Vertex): Boolean = {
    if (v == null || v.bigraph == null || v.bigraph.root == null) {
      println("Vertex is null");
      return true;
    } else {
      var result = true;

      var t = v.bigraph.root;
      if (inList.size > 0) result = result && inCheck(t);
      if (notInList.size > 0) result = result && notInCheck(t);
      if (linkList.size > 0) result = result && linkCheck(t);
      if (notLinkList.size > 0) result = result && notLinkCheck(t);
      return result;
    }
  }

  //(S5:Service,V), (S5:Service,O), (S5:Service,P), (P,W)...
  def getParentChildPair(t: Term): List[Pair[String, String]] = { //返回所有父子关系，包括深层的关系，即A.B.C则(A,C)也会出现在结果中
    var pcPairs: List[Pair[String, String]] = List();
    var left: String = "";
    var right: String = "";

    if (t.isInstanceOf[Prefix]) { // K.T or K.(T1|T2)
      var prefix = t.asInstanceOf[Prefix];
      var suffix = prefix.suffix;
      if (suffix.isInstanceOf[Prefix]) { // K.T
        left = prefix.node.ctrl.name
        right = suffix.asInstanceOf[Prefix].node.ctrl.name
        var pair = Pair(left, right); //left是父，right是子
        pcPairs = pcPairs.:+(pair);
      } else if (suffix.isInstanceOf[Paraller]) { // K.(T1|T2)
        var children = suffix.asInstanceOf[Paraller].getChildren;
        children.map(child => {
          if (child.isInstanceOf[Prefix]) { // 非nil 非hole
            left = prefix.node.ctrl.name
            right = child.asInstanceOf[Prefix].node.ctrl.name
            var pair = Pair(left, right);
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
   * 计算Term中的连接对，具有相同链接名称的控制表示有link
   * 如(C,List(m)), (Y,List(m))，C和Y通过m:edge连接在一起
   * 输出连接对((m,C),(m,Y))
   */
  def getLinkPair(t: Term): List[Pair[Pair[String, String], Pair[String, String]]] = {
    var linkPairs: List[Pair[Pair[String, String], Pair[String, String]]] = List();
    var ctrlInstances: List[Pair[String, List[String]]] = getCtrlInstances(t);
    var lnName2ctrlPairs: Map[String, List[String]] = Map();

    // 将每个链接的两两任意组合配对
    var i = 0;
    var j = 0;
    while (i < ctrlInstances.size) {
      j = i + 1; //自己跟自己不比
      while (j < ctrlInstances.size) {
        var A: List[String] = ctrlInstances(i)._2.distinct.filter { x => !"idle".equals(x) }; //idle不参与查找
        var B: List[String] = ctrlInstances(j)._2.distinct.filter { x => !"idle".equals(x) };
        var intersectPorts: List[String] = A.intersect(B)
        if (intersectPorts != null && intersectPorts.size > 0) { //有交集证明有相同的edge即有连接
          intersectPorts.map { x =>
            {
              var pair1 = Pair(x, ctrlInstances(i)._1)
              var pair2 = Pair(x, ctrlInstances(j)._1)
              var pair = Pair(pair1, pair2)
              linkPairs = linkPairs.+:(pair)
            }
          }
        }
        j = j + 1;
      }
      i = i + 1;
    }

    linkPairs.distinct;
  }

  /**
   * 计算Term的所有控制实例：控制名称+端口名称列表
   * (Service,List(idle, idle, idle, idle)), (C,List(m)), (Y,List(m, n)), (D,List(n))
   */
  def getCtrlInstances(t: Term): List[Pair[String, List[String]]] = {
    var ctrlInstances: List[Pair[String, List[String]]] = List();
    var nodeName: String = "";
    if (t.isInstanceOf[Prefix]) {
      var prefix = t.asInstanceOf[Prefix];
      if (prefix.node.ports.size > 0) { // K[n1,n2,...]
        nodeName = prefix.node.ctrl.name;
        var pair = Pair(nodeName, prefix.node.ports.map(_.name)); //不用加上edge了，其中不是idle的就是edge或者innername或者outername
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

    ctrlInstances; //应该可重复，因为同一个Control有不同的Node就有不同的连法
  }

  /**
   * 内嵌关系判断，in约束的必须内嵌，不是内嵌则返回false
   */
  def inCheck(t: Term): Boolean = {
    var pcPairs = getParentChildPair(t);
    var cpSortPairs: List[Pair[String, String]] = List();

    pcPairs.map(pair => {
      if (ctrlSortMap.contains(pair._1) && ctrlSortMap.contains(pair._2)) {
        var parentSort = ctrlSortMap(pair._1);
        var childSort = ctrlSortMap(pair._2);
        var sortPair = Pair(childSort, parentSort);
        cpSortPairs = cpSortPairs.+:(sortPair);
      }
    });

    inList.map(pair => {
      if (!cpSortPairs.contains(pair)) {
        violationInfo += "Sorting Violation: " + pair._1 + " must in " + pair._2 + " to accord with Placesort Constraint\"in\"!\n";
        return false;
      }

    });
    true;
  }

  /**
   * 非内嵌关系判断，notIn的不允许内嵌，出现则返回false
   */
  def notInCheck(t: Term): Boolean = {
    var pcPairs = getParentChildPair(t);

    pcPairs.map(pair => {
      if (ctrlSortMap.contains(pair._1) && ctrlSortMap.contains(pair._2)) {
        var parentSort = ctrlSortMap(pair._1);
        var childSort = ctrlSortMap(pair._2);
        var sortPair = Pair(childSort, parentSort);
        if (notInList.contains(sortPair)) { //检查控制对中的sort是否出现了notIn描述的约束中，若出现则说明模型不符合其约束，否则符合。
          violationInfo += "Sorting Violation: " + pair._1 + "." + pair._2 + " does not accord with Placesort Constraint\"notIn\"!\n";
          return false;
        }
      }
    });
    true;
  }

  /**
   * 连接关系判断，link约束的必须连接，没有连接则返回false
   */
  def linkCheck(t: Term): Boolean = {
    var linkPairs = getLinkPair(t);
    var linkSortPairs: List[Pair[String, String]] = List();

    linkPairs.map(lp => {
      if (linkSortMap.contains(lp._1) && linkSortMap.contains(lp._2)) {
        var oneSort = linkSortMap(lp._1);
        var anotherSort = linkSortMap(lp._2);
        var sortPair = Pair(oneSort, anotherSort);
        var symSortPair = Pair(anotherSort, oneSort);
        linkSortPairs = linkSortPairs.+:(sortPair);
        linkSortPairs = linkSortPairs.+:(symSortPair);
      }
    });

    linkList.map(ll => {
      if (!linkSortPairs.contains(ll)) {
        violationInfo += "Sorting Violation: " + ll.toString + " must link to accord with Linksort Constraint\"link\"!\n";
        return false;
      }
    });
    true;
  }

  /**
   * 非连接关系判断，notLink的不允许连接，出现连接则返回false
   */
  def notLinkCheck(t: Term): Boolean = {
    var linkPairs = getLinkPair(t);
    linkPairs.map(lp => {
      if (linkSortMap.contains(lp._1) && linkSortMap.contains(lp._2)) {
        var oneSort = linkSortMap(lp._1);
        var anotherSort = linkSortMap(lp._2);
        var sortPair = Pair(oneSort, anotherSort);
        var symSortPair = Pair(anotherSort, oneSort);
        if (notLinkList.contains(sortPair) && notLinkList.contains(symSortPair)) { //连接关系“黑名单”检查
          violationInfo += "Sorting Violation: " + lp._1.toString + "." + lp._2.toString + " does not accord with Linksort Constraint\"notLink\"!\n";
          return false;
        }
      }
    });
    true;
  }

  //(S5:Service,V,Q...)有Node就返回Node:Control，没有就直接是Control
  def getAllControlWithNode(t: Term): Set[String] = {
    var controlList: Set[String] = Set();

    if (t.isInstanceOf[Prefix]) { // K.T or K.(T1|T2)
      var prefix = t.asInstanceOf[Prefix];
      controlList = controlList.+(getNodeControlName(t));

      var suffix = prefix.suffix;
      if (suffix.isInstanceOf[Prefix]) { // K.T
        controlList = controlList.+(getNodeControlName(suffix));
      } else if (suffix.isInstanceOf[Paraller]) { // K.(T1|T2)
        var children = suffix.asInstanceOf[Paraller].getChildren;
        children.map(child => {
          if (child.isInstanceOf[Prefix]) {
            controlList = controlList.+(getNodeControlName(child));

          }
        });
      }
      controlList = controlList.++(getAllControlWithNode(suffix))
    } else if (t.isInstanceOf[Paraller]) {
      var children = t.asInstanceOf[Paraller].getChildren;
      children.map(child => {
        controlList = controlList.++(getAllControlWithNode(child))
      });
    } else if (t.isInstanceOf[Regions]) {
      var children = t.asInstanceOf[Regions].getChildren;
      children.map(child => {
        controlList = controlList.++(getAllControlWithNode(child))
      });
    }

    controlList;
  }

  def getNodeControlName(t: Term): String = {
    var res: String = "";
    if (t.isInstanceOf[Prefix]) {
      var p: Prefix = t.asInstanceOf[Prefix]
      if (p.node.name != null && !p.node.name.equals("")) {
        res = p.node.name + ":" + p.node.ctrl.name
      } else {
        res = p.node.ctrl.name
      }
    }
    res
  }

  def getNodeByControl(ncpair: List[Pair[String, String]], control: String): Set[String] = {
    var res: Set[String] = Set()
    ncpair.foreach { pair =>
      {
        if (pair._2.equals(control)) {
          res = res.+(pair._1)
        }
      }
    }
    res
  }

}