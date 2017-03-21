package org.bigraph.bigsim.BRS

import scala.collection.mutable.Set
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.data.Data

/**
 * @author zhaoxin
 * version 0.1
 *
 * @author liangwei
 * version 0.2
 */

/**
 * Each vertex in the graph is an agent, and it related to a bigraph.
 */
class Vertex(b: Bigraph, v: Vertex, rr: ReactionRule) {
  var visited: Boolean = false
  var terminal: Boolean = false
  var parent: Vertex = v //该节点的父节点
  var pathVisited: Boolean = false
  
      //add by lbj
  var violateRecordTracking: Boolean = false //每个节点单独记录

  var sysClk: Double = 0
  var reactionRule: ReactionRule = rr//一条规则反应到这个Vertex
  var reactionRules: Set[ReactionRule] = Set()//几条并发规则反应到这个Vertex
  var variables: String = Data.getValues(",") //初始化，为输入data文件中的所有初始数据，组装成=的形式：Tim.hasChecked=false,james.hasShoppingin=false,james.hasChecked=false...
  var bigraph: Bigraph = b
  var hash: Int = {//根据当前偶图的root生成唯一的hashCode
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }

  var parents: scala.collection.immutable.Set[Vertex] = scala.collection.immutable.Set(v)

  def this(b: Bigraph, v: Vertex, rrs: Set[ReactionRule], isSet: Boolean) = {//多条并发反应的构造器！
    this(b, v, null)//这里不用重新设置variables，因为重新调用这个构造器就设置的是新的Data模型的值
//    reactionRules ++ rrs
    reactionRules = rrs
  }

  def addParents(v: Vertex) {
    parents.+(v)
//    parents += v
  }

  // use Map instead of the 'set<pair<node *,reactionrule *> > target;' of C++ version.

  var target: Map[Vertex, ReactionRule] = Map();

  def addTarget(v: Vertex, rr: ReactionRule) {
    target += (v -> rr)
  }

  // use Map instead of the 'set<pair<node *,reactionrule *> > target;' of C++ version.
  /**
   * Here we use the set pair format for multiple reactions
   */
  var targets: Map[Vertex, Set[ReactionRule]] = Map();
  //var targets: Set[Tuple2[Vertex, Set[ReactionRule]]] = Set();

  def addTargets(rrs: Set[ReactionRule], v: Vertex) { //哪几条反应规则反应到这个Vertex
    targets += v -> rrs
  }
  
  def getTargetsRRs(v:Vertex):Set[ReactionRule]={
    var rrs:Set[ReactionRule]=targets.getOrElse(v, Set())
    rrs
  }
}

