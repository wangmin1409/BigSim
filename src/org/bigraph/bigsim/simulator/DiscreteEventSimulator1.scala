package org.bigraph.bigsim.simulator

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.utils.GlobalCfg
import scala.collection.immutable.Queue
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.model.Nil
import org.bigraph.bigsim.data.Data

object DiscreteEventSimulator1 {
  var matchDiscard: Set[Match] = Set();

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }
}

class DiscreteEventSimulator1(b: Bigraph) extends Simulator {
  /** original graph */
  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);

  var forwardPath: Queue[Vertex] = Queue();//用到这个，其实就是反应队列
  //var backwardPath: Stack[Vertex] = Stack();//这个没用到，可删去

  var forwardReactNodes: Set[String] = Set();//即本模拟器定义的reactNodes
 //var backwardReactNodes: Set[String] = Set();

  /** simulate steps */
  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();

  def simulate: Unit = {
    if (b == null || b.root == null) {
      println("DiscreteEvent::simulate(): null");
      return ;
    } else {
      if (GlobalCfg.printMode) {
        printf("%s:%s\n", "N_" + Math.abs(v.hash), v.bigraph.root.toString);
        println(v.variables)
      }

      /** init the start state */
      forwardPath = forwardPath.enqueue(v);//必须存到一个变量里面，直接forwardPath是进不去的，queue不可以直接enqueue，存在返回值里，论文中的workStack
      //backwardPath.push(v);//stack可以直接push

      /** iterator the simulate step */
      while (backwordStep()) {
      }
      report;
      DiscreteEventSimulator1.matchGC;
    }
  }

  def report(): String = {
    GlobalCfg.node = false
    if (GlobalCfg.pathOutput != "")
      g.dumpPath//test new method
    GlobalCfg.node = true
    g.dumpDotForward
  }

  def backwordStep(): Boolean = {
    /** whether reach the max steps */
    if (steps >= GlobalCfg.maxSteps) {
      println("simulate::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      report();
      return false;
    }
    // 待检测模型队列为空
    if (forwardPath.size == 0) {
      println("sim::step Complete!");
      report();
      DiscreteEventSimulator1.matchGC;
      return false;
    }

    // 取出栈顶模型
    var v: Vertex = forwardPath.last;//取出队尾
    steps += 1;
    var step: Int = steps;
    var b: Bigraph = v.bigraph;

    //这段是addMatch
    var matches: Set[Match] = b.findMatches;
    //matches.foreach(f => { f.reactNodes.foreach(println) })
    checked(v.hash) = true;
    if (matches.size == 0) { //没有可以匹配的规则，该节点为终结节点
      v.terminal = true;
      return false;
    }

    var simRRMap: TreeMap[String, Match] = TreeMap();
    var isFirst = true

    if (matches.size > 0) {

      // delete conflict react node
      matches.map(it => {//就是Match的reactNodes和本模拟器定义的reactNodes比较看有没有重复的node，有就认为是冲突，相当于TimeSlicingSimulator调用的Match的conflict方法，逻辑一样
        var conflict = false
        it.reactNodes.map(rn => {
          println(rn)
          if (forwardReactNodes.contains(rn)) {
            conflict = true
          }
        })
        if (conflict) {//冲突，模拟器的reactNodes集合里面已经有该node，故删去
          matches -= it//从matches: Set[Match]中删除it
        } else if (!it.rule.random) {//不冲突且该RR不是随机反应，那就应该必定发生反应，加入到待反应集合simRRMap中，必然事件，必定发生
          var key = (GlobalCfg.SysClk + it.rule.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
          while (simRRMap.contains(key)) {//指导key随机出一个simRRMap中没有的才退出循环，key的前半部分是当前系统时间加上反应花费时间，即反应完成的时间点，这部分是一样的，后面的随机数可以不一样
            key = (GlobalCfg.SysClk + it.rule.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
          }
          simRRMap += key -> it //(key,it)放入simRRMap中
          forwardReactNodes ++ it.reactNodes //合并两个set，把match的reactNodes放入simulator的forwardReactNodes中
          matches -= it//从matches: Set[Match]中删除it
        }
      });

      // find one rule meets the condition
      if (matches.size > 0) {//这时候的matches是筛选过后的Set[Match],只剩下不冲突但是随机反应的规则
        var randIndex = scala.util.Random.nextInt(matches.size)//在这个集合中随机一个位置应用
        var curMatch = matches.toList(randIndex)//先把Set转为List然后取下标为randIndex的Match元素，这所有的随机集合中就选一条出来反应，随机事件，随机一个来发生

        var rr: ReactionRule = curMatch.rule;
        var key = (GlobalCfg.SysClk + rr.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
        while (simRRMap.contains(key)) {//指导key随机出一个simRRMap中没有的才退出循环
          key = (GlobalCfg.SysClk + rr.getRRIncr).toString + "_" + scala.util.Random.nextInt(100000)
        }

        //选出随机事件后加不加入simRRMap还具有一定随机性
        if (isFirst || !curMatch.rule.random) {//如果这是第一个或curMatch不是随机的规则（应该不可能，不冲突且不随机的上面已经处理过）
          simRRMap += key -> curMatch //(key,curMatch)放入simRRMap中
          forwardReactNodes ++ curMatch.reactNodes
        } else if (scala.util.Random.nextInt(2) == 1) {//如果这不是第一个且curMatch是随机的规则且随机数为1
          simRRMap += key -> curMatch
          forwardReactNodes ++ curMatch.reactNodes
        }
        matches -= curMatch//从matches: Set[Match]中删除curMatch
      }
      isFirst = false
    }

    //这段是applyMatch，将simRRMap中的所有match都apply了
    var curBigraph = v.bigraph
    var curRR: ReactionRule = null
    simRRMap.foreach(tm => {
      if (!GlobalCfg.checkData || tm._2.rule.check) {
        var nb: Bigraph = curBigraph.applyMatch(tm._2);//simRRMap的value是Match
        GlobalCfg.SysClk = tm._1.split("_")(0).toDouble//simRRMap的key是“反应结束时间点_随机数” 这里split里面是_，即取出反应完的时间点作为系统时钟，系统时钟直接前移，系统时钟在这里推进
        /**
         * update a reaction rule data model
         */
        tm._2.rule.update //调的是老版本的更新数据函数，没有做reactNode的映射，应该用梁薇后来的
        /**
         * update agent data with clock
         */
        Data.updateDataCalcsWithClk(tm._2.rule.getRRIncr.toString)

        if (nb.root == null)
          nb.root = new Nil();
        curBigraph = nb
        curRR = tm._2.rule
      }
    })

    if (curBigraph != null && curRR != null) {//用new vertex构造lut
      var nv = new Vertex(curBigraph, v, curRR)
      nv.sysClk = GlobalCfg.SysClk
      if (g.lut.contains(nv.hash)) {
        nv = g.lut(nv.hash);
        nv.addParents(v)
      } else {
        g.add(nv);
      }
      v.addTarget(nv, curRR);
      forwardPath.enqueue(nv) //路径上每个节点放入forwardPath中
      if (GlobalCfg.printMode) {
        printf("%s:%s\n", "N_" + Math.abs(nv.hash), nv.bigraph.root.toString);
        println(nv.variables)
      }
    }
    matches.clear();
    DiscreteEventSimulator1.matchGC;
    true;
  }

  def dumpDotForward(dot: String): String = ""

}
