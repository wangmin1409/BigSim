package org.bigraph.bigsim.model

import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.parser.TermParser
import scala.collection.mutable.Map
import org.bigraph.bigsim.parser._
import org.bigraph.bigsim.data._
import org.bigraph.bigsim.utils.GlobalCfg

/**
 * @author libaijie
 */
class Tracking(t: String, ty: String) {
  var track = t;
  var trackType = ty; //类型: TrackException，TrackRecord
  var assertType = ""; //类型: BoolAssert布尔断言，StructureAssert偶图结构断言
  var leftStructure: String = "";
  var relation: String = "";
  var rightStructure: String = "";
  var conditionExpr: String = "";
  var hasRecorded: Boolean = false; //RecordAssert在图上只记录发生点的地方一次，否则从这点开始后面的节点都会被记录
  override def toString = "Tracking(" + track + "," + trackType + "," + assertType + ")";

  def this(t: String, ty: String, at: String) = {
    this(t, ty);
    init(t);
  }

  def init(t: String): Unit = {
    var trackStr: Array[String] = t.split(" ");
    if ((t.contains("in") || t.contains("notIn") || t.contains("link") || t.contains("notLink")) && trackStr.size == 3) {
      leftStructure = trackStr(0);
      relation = trackStr(1);
      rightStructure = trackStr(2);
      assertType = "StructureAssert";
    } else { //其他都认为是布尔条件，包括都不是的
      conditionExpr = t;
      assertType = "BoolAssert";
    }
  }

  /**
   * check if a vertex satisfied structure assert
   */
  def check(v: Vertex): Boolean = {
    if (v == null || v.bigraph == null || v.bigraph.root == null) {
      println("Vertex is null");
      return true;
    } else {
      var result = true;

      var t = v.bigraph.root;
      if (assertType.equals("StructureAssert")) {
        if (relation.equals("in")) { result = inCheck(t); }
        else if (relation.equals("notIn")) { result = notInCheck(t); }
        else if (relation.equals("link")) { result = linkCheck(t); }
        else if (relation.equals("notLink")) { result = notLinkCheck(t); }
      } else if (assertType.equals("BoolAssert")) {
        result = checkBooleanExpr(t);
      } else {
        println("Unknown check type !");
        return false;
      }

      result
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
        if (prefix.node.name != null && !prefix.node.name.equals("")) { left = prefix.node.name + ":" + prefix.node.ctrl.name } else { left = prefix.node.ctrl.name }
        if (suffix.asInstanceOf[Prefix].node.name != null && !suffix.asInstanceOf[Prefix].node.name.equals("")) { right = suffix.asInstanceOf[Prefix].node.name + ":" + suffix.asInstanceOf[Prefix].node.ctrl.name }
        else { right = suffix.asInstanceOf[Prefix].node.ctrl.name }
        var pair = Pair(left, right); //left是父，right是子
        pcPairs = pcPairs.:+(pair);
      } else if (suffix.isInstanceOf[Paraller]) { // K.(T1|T2)
        var children = suffix.asInstanceOf[Paraller].getChildren;
        children.map(child => {
          if (child.isInstanceOf[Prefix]) { // 非nil 非hole
            if (prefix.node.name != null && !prefix.node.name.equals("")) { left = prefix.node.name + ":" + prefix.node.ctrl.name } else { left = prefix.node.ctrl.name }
            if (child.asInstanceOf[Prefix].node.name != null && !child.asInstanceOf[Prefix].node.name.equals("")) { right = child.asInstanceOf[Prefix].node.name + ":" + child.asInstanceOf[Prefix].node.ctrl.name }
            else { right = child.asInstanceOf[Prefix].node.ctrl.name }
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
   * 输出连接对(C,Y)
   */
  def getLinkPair(t: Term): List[Pair[String, String]] = {
    var linkPairs: List[Pair[String, String]] = List();
    //List[Pair[ctrlName, ports]]
    var ctrlInstances: List[Pair[String, List[String]]] = getCtrlInstances(t);
    //Map[linkName, List[Pair[ctrlName, index]]]
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
          var pair = Pair(ctrlInstances(i)._1, ctrlInstances(j)._1)
          linkPairs = linkPairs.+:(pair)
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
   * 内嵌关系判断，不满足in则返回false
   */
  def inCheck(t: Term): Boolean = {
    var pcPairs = getParentChildPair(t);

    pcPairs.map(pair => {
      if (pair._1.equals(leftStructure) && !pair._2.equals(rightStructure)) { //出现子结构却不在要求的父结构中
        println("Tracking in Violation: " + leftStructure + " must be in " + rightStructure + "!");
        return false;
      }
    });
    true;
  }

  /**
   * 非内嵌关系判断，不满足notIn则返回false
   */
  def notInCheck(t: Term): Boolean = {
    var pcPairs = getParentChildPair(t);

    pcPairs.map(pair => {
      if (pair._1.equals(leftStructure) && pair._2.equals(rightStructure)) { //子结构出现在必须notIn的父结构中
        println("Tracking notIn Violation: " + leftStructure + " mustn't be in " + rightStructure + "!");
        return false;
      }
    });
    true;
  }

  /**
   * 连接关系判断，不满足link则返回false
   */
  def linkCheck(t: Term): Boolean = {
    var linkPairs = getLinkPair(t);

    linkPairs.map(pair => {
      if (pair._1.equals(leftStructure) && !pair._2.equals(rightStructure)) { //出现某结构却没和要求结构相连
        println("Tracking Link Violation: " + leftStructure + " must be link with " + rightStructure + "!");
        return false;
      }
    });
    true;
  }

  /**
   * 非连接关系判断，不满足notLink则返回false
   */
  def notLinkCheck(t: Term): Boolean = {
    var linkPairs = getLinkPair(t);

    linkPairs.map(pair => {
      if (pair._1.equals(leftStructure) && pair._2.equals(rightStructure)) { //出现某结构却没和不允许相连的结构相连了
        println("Tracking notLink Violation: " + leftStructure + " mustn't be link with " + rightStructure + "!");
        return false;
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

  //校验布尔表达式
  def checkBooleanExpr(t: Term): Boolean = {
    //    Data.parseData(GlobalCfg.dataInput) //这是初始化数据而这里应该用的是Data中每步更新过的数据，应该不用了

    var res: Boolean = true
    var ncpair: List[Pair[String, String]] = List()
    var controls: Set[String] = getAllControlWithNode(t) //查找当前root中所有的node和control
    //controls中全部存放Control，有Node的放在ncpair里面
    controls.foreach { c =>
      {
        if (c.contains(":")) { //找到root中Node和Control的映射
          var s: Array[String] = c.split(":")
          var pair = Pair(s(0), s(1));
          ncpair = ncpair.:+(pair);
          controls = controls.-(c)
          controls = controls.+(s(1))
        }
      }
    }

    //如果没有control就不用替换了，直接这里都是node表达式了
    controls.foreach { c =>
      {
        if (conditionExpr.contains(c)) {
          var nc: Set[String] = getNodeByControl(ncpair, c)
          if (nc.size > 0) { //TODO: 表达式左右两边可有多个Control，每个都将被替换为具体Node再判断条件，但不支持一个Control有多个Node的情况，这需要组合，后续实现
            nc.map { x =>
              {
                
                var replacedExpr : String = conditionExpr.replaceAll(c, x)
                println("replacedExpr:" + replacedExpr)
                var q: Query = BooleanExprParser.parse(replacedExpr)
                if (!q.check()) { //data中找不到的表达式这个方法会返回false
                  println("Tracking Boolean Expression Violation: can not satisfy " + replacedExpr + "!");
                  return false
                }
              }
            }
          }
        }
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

object TestTracking {
  def main(args: Array[String]) {
    var t: Tracking = new Tracking("Trolley in Security:Zone", "TrackException", null)

    var prefixcompare2 = "A.(GUI.$1 | B.(w3a1:C[m:edge].(N|F.(G|Y[m:edge,n:edge])) | D[n:edge] | M)) | S4:Service[idle,idle,idle,idle] | S5:Service[idle,idle,idle,idle].(V|P.(Q|W)|O) | S6:Service[idle,idle,idle,idle] | $0"
    var testPre2 = TermParser.apply(prefixcompare2)
    var result1: List[Pair[String, String]] = t.getParentChildPair(testPre2)
    println(result1);

    var result2: List[Pair[String, List[String]]] = t.getCtrlInstances(testPre2)
    println(result2);

    var result3: List[Pair[String, String]] = t.getLinkPair(testPre2)
    println(result3);

    var result4: Set[String] = t.getAllControlWithNode(testPre2)
    println(result4);

    //    Data.parseData("Examples/Airport_513/data/SmartContract.data")
    //
    //    var q: Query = BooleanExprParser.parse("David.hasDanger=='true' ");
    //    println(q + ", q.check: " + q.check());

    t.conditionExpr = "S5.test=='true'"
    var result5: Boolean = t.checkBooleanExpr(testPre2)
    println(result5);
  }
}