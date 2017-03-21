package org.bigraph.bigsim.model

import org.bigraph.bigsim.BRS.Vertex
import scala.collection.mutable.Set
import org.bigraph.bigsim.parser.TermParser

/**
 * @author libaijie
 */
class Binding { //linking(bindingControl) in placing(Control)
  var linking: String = "";
  var placing: String = "";

  override def toString = "Binding (Link:" + linking + " in Control:" + placing + ")";

  def this(n: String) = {
    this();
    init(n);
  }

  def init(n: String): Unit = {
    if (n.contains("in")) {
      var bindStr: Array[String] = n.split(" ");
      linking = bindStr(0);
      placing = bindStr(2);
    }
  }

  /**
   * 计算Term的所有控制实例：控制名称+端口名称列表
   * (Service,List(idle, idle, idle, idle)), (C,List(m)), (Y,List(m, n)), (D,List(n))
   * 有Node的话则节点也要输出
   */
  def getCtrlInstances(t: Term): List[Pair[String, List[String]]] = {
    var ctrlInstances: List[Pair[String, List[String]]] = List();
    var nodeName: String = "";
    if (t.isInstanceOf[Prefix]) {
      var prefix = t.asInstanceOf[Prefix];
      if (prefix.node.ports.size > 0) { // K[n1,n2,...]
        if (prefix.node.name != null && !prefix.node.name.equals("")) { nodeName = prefix.node.name + ":" + prefix.node.ctrl.name } else { nodeName = prefix.node.ctrl.name }
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
   * 计算Term中的连接对，具有相同链接名称的控制表示有link
   * 如(C,List(m)), (Y,List(m))，C和Y通过m:edge连接在一起
   * 返回Set集合(C,Y)
   */
  //  def getLinkPair(t: Term): Set[String] = {
  //    var linkPairs: Set[String] = Set();
  //    //List[Pair[ctrlName, ports]]
  //    var ctrlInstances: List[Pair[String, List[String]]] = getCtrlInstances(t);
  //    //Map[linkName, List[Pair[ctrlName, index]]]
  //    var lnName2ctrlPairs: Map[String, List[String]] = Map(); //linking
  //
  //    var i = 0;
  //    while (i < ctrlInstances.size) {
  //        var ctrlEdges: List[String] = ctrlInstances(i)._2.distinct.filter { x => !"idle".equals(x) }; //idle不参与查找
  //        ctrlEdges.foreach { e => {
  //          if(e.equals(linking)){
  //            linkPairs = linkPairs.+(ctrlInstances(i)._1)
  //          }
  //         } 
  //        }
  //      i = i + 1;
  //    }
  //
  //    linkPairs;
  //  }

  /**
   * 输出bindingControl和两端的两个节点，（B，X），（B，Y）都可能有node
   */
  def getLinkControl(t: Term, n: Node): Set[String] = {
    var linkPairs: Set[String] = Set();
    //List[Pair[ctrlName, ports]]
    var ctrlInstances: List[Pair[String, List[String]]] = getCtrlInstances(t);
    //Map[linkName, List[Pair[ctrlName, index]]]
    var lnName2ctrlPairs: Map[String, List[String]] = Map(); //linking
    var bindingLinks: List[String] = List();

    var i = 0;
    while (i < ctrlInstances.size) {
      if (ctrlInstances(i)._1.equals(linking) || (ctrlInstances(i)._1.split(":").size == 2 && ctrlInstances(i)._1.split(":")(1).equals(linking))) {
        bindingLinks = ctrlInstances(i)._2
      }
      i = i + 1;
    }
    if (bindingLinks.size == 2) {
      linkPairs = linkPairs.+(linking);
      ctrlInstances.foreach(x => {
        if (x._2.contains(bindingLinks(0)) || x._2.contains(bindingLinks(1))) {
          linkPairs = linkPairs.+(x._1);
        }
      })
    }
    linkPairs;
  }

  def mergeResults(result1: Set[Node], result2: Set[Node]): Set[Node] = {
    result2.map(it => {
      result1 += it
    })
    result1
  }

  //获得所有Node集合，其中每个Node的parent属性为父节点名称，如果是顶层Node则父为null
  def getNodeParent(term: Term): Set[Node] = {
    var result: Set[Node] = Set()

    if (term.termType == TermType.TPAR) {
      var paraResult: Set[Term] = term.asInstanceOf[Paraller].getChildren;
      paraResult.map(it => {
        result = mergeResults(result, getNodeParent(it))
      })
    } else if (term.termType == TermType.TPREF) {
      if (term.asInstanceOf[Prefix].suffix.termType == TermType.THOLE || term.asInstanceOf[Prefix].suffix.termType == TermType.TNIL) {
        result += term.asInstanceOf[Prefix].node;
      } else {
        result += term.asInstanceOf[Prefix].node
        var tempResult: Set[Node] = getNodeParent(term.asInstanceOf[Prefix].suffix)
        tempResult.map(it => {
          if (it.parent == null) it.parent = term.asInstanceOf[Prefix].node
        })
        result = mergeResults(result, tempResult)
      }
    }
    result;
  }

  def getPlaceStr(n: Node): String = {
    if (!"".equals(n.name) && n.name.length() > 0) {
      return n.name + ":" + n.ctrl.name
    }
    return n.ctrl.name
  }

  /**
   * 绑定关系判断，不满足则返回false
   */
  def bindingCheck(t: Term): Boolean = {
    var containBindingControl = false
    var bindingControls: Set[String] = Set()

    var nodes = getNodeParent(t);
    nodes.foreach { x =>
      {
        if (x.ctrl.name == linking) {
          containBindingControl = true
          bindingControls = getLinkControl(t, x)
        }
      }
    }

    if (!containBindingControl) { return true; } //定义了一个模型里不存在的binding control则认为验证成功直接通过了

    var parentPlace: Set[String] = Set()
    nodes.foreach { n =>
      {
        var placeName: String = getPlaceStr(n) //自动解析为Node:Control的形式，或只有Control
        bindingControls.foreach { b =>
          {
            if (placeName.equals(b)) {
              var par: String = getPlaceStr(n.parent)
              parentPlace = parentPlace.+(par)
            }
          }
        }
      }
    }

    if (parentPlace.size != 1 || !parentPlace.head.equals(placing)) { //三个control有不同的父节点或共同的父节点不是定义的placing（默认只支持一层）
      println("Binding Violation: " + "Binding (Link:" + linking + " in Control:" + placing + ")");
      return false
    }

    //    if(linkPairs.size==2){//返回linking两边的节点
    //      nodes.foreach { n => {
    //       var placeName:String = getPlaceStr(n) //自动解析为Node:Control的形式，或只有Control
    //       linkPairs.foreach { l => {
    //         if(placeName.equals(l)){
    //           var par:String = getPlaceStr(n.parent)
    //           if(!par.equals(placing)){
    //             println("Binding Violation: " + "Binding (Link:" + linking + " in Control:" + placing + ")");
    //             return false
    //           }
    //         }
    //        }
    //       }
    //      }
    //     }     
    //    }else{
    //      false
    //    }
    true;
  }

  def check(v: Vertex): Boolean = {
    if (v == null || v.bigraph == null || v.bigraph.root == null) {
      println("Vertex is null");
      return true;
    } else {
      var t = v.bigraph.root;
      return bindingCheck(t)
    }
  }

}

object TestBinding {
  def main(args: Array[String]) {
    var b: Binding = new Binding("useBlanket in Airplane")

    var prefixcompare2 = "A.(GUI.$1 | B.(w3a1:C[m:edge].(N|F.(G|Y[m:edge,n:edge])) | D[n:edge] | M)) | S4:Service[idle,idle,idle,idle] | S5:Service[idle,idle,idle,idle].(V|P.(Q|W)|O) | S6:Service[idle,idle,idle,idle] | $0"
    var testPre2 = TermParser.apply(prefixcompare2)

    var checkResult: Set[Node] = b.getNodeParent(testPre2);
    println("size: " + checkResult.size);
    checkResult.map(it => {
      println("Node: " + it.ctrl.name)
      println("Parent: " + it.parent)
    })
  }

}