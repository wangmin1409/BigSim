package org.bigraph.bigsim.simulator

/**
 * @author liangwei
 * version 0.1
 */

import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.immutable.TreeMap
import scala.util.Random
import java.io._
import org.bigraph.bigsim._
import org.bigraph.bigsim.utils.GlobalCfg
import cern.jet.random.engine.RandomEngine
import cern.jet.random.Uniform
import cern.jet.random.Exponential
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.BRS._
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.Data

object StochasticSimulator1 {

  var matchDiscard: Set[Match] = Set();

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }

  def InversionSampling(ruleActivities: Map[Int, Double]): Int = {//逆抽样,Map(规则index,规则活跃度/系统活跃度)，该方法确定下次要反应规则的索引
    var cumulate: Double = 0 //积累
    ruleActivities.map(r => {
      cumulate += r._2 //大于0的规则活跃度/系统活跃度求和
    })
    val re: RandomEngine = RandomEngine.makeDefault //生成一个随机数引擎
    val un: Uniform = new Uniform(0, cumulate, re) //Uniform(double min, double max, RandomEngine randomGenerator) 均匀分布，min和max为最小最大值，第三个参数为随机数引擎
    val r: Double = un.nextDouble() //再最小最大值之间返回一个均匀分布随机数
    cumulate = 0
  //  var cdf: TreeMap[Int, Double] = new TreeMap()
    ruleActivities.map(rule => {
      cumulate += rule._2  //大于0的规则活跃度/系统活跃度求和
      if (cumulate >= r) //即累积到刚刚>=随机数r就停止，返回这时候的规则index，例前三个规则的规则活跃度/系统活跃度和r1/r0+r2/r0+r3/r0才刚刚大于r，则返回index=3，r3即为下次要反应的规则
        return rule._1
    })
    ruleActivities.keys.toList(0) //所有大于0的规则活跃度/系统活跃度求和都小于随机数r，则返回ruleActivities中第一个元素的规则下标，ruleActivities下标是有序的，即返回index=1，r1即为下次要反应的规则
  }
}

class StochasticSimulator1(b: Bigraph) extends Simulator {
  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);
  var states: Queue[Tuple2[Double, Vertex]] = Queue();
  var reactNodes: Set[String] = Set();

  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();

  def simulate: Unit = {
    /**
     * Store the simPath and simRules
     */
    var simPath: Queue[Term] = Queue() //用于存路径上每个bigraph的root
    var simRules: Set[String] = Set()

    /**
     * 0. Initialization:
     * Initialize the simulation state
     * t = 0
     * M(a,R) = match(a,R)
     * αR = |M(a,R)|*Pr
     * α=αR1+αR2+...+αRn
     * If α==0, simulation end
     */
    // add the initial agent to the simQueue
    var curv: Vertex = v
    var curb: Bigraph = v.bigraph
    simPath += curb.root
    states += ((0, curv)) //存时间和当前vertex
    var matchMap: Map[Int, Set[Match]] = Map()
    var systemActivity: Double = 0.0
    var ruleActivities: Map[Int, Double] = Map()
    var time: Double = 0.0

    var ruleIndex: Int = 0  //规则下标，第一个规则就是1，依次类推
    println("rule size:" + b.rules.size)
    b.rules.map(r => {
      ruleIndex += 1
      println(r.name);
      var matches: Set[Match] = curb.findMatchesOfRR(r)
      matchMap += ruleIndex -> matches //规则下标、规则匹配集合

      println("ruleIndex:" + ruleIndex + " matches:" + matches.size)
      //calculate rule activities
      val ruleActivity = r.rate * matches.size //计算规则r的活跃度，若有规则与当前偶图匹配结果为空，则规则活跃度计算为0，后面会被过滤掉
      ruleActivities += ruleIndex -> ruleActivity //ruleActivities的下标一定是有序的，从1开始一直往上

      //calculate system activity
      systemActivity += ruleActivity  //计算系统活跃度33
    })

    while (systemActivity > 0) {//systemActivity=0系统模拟结束
      /**
       * 1. Monte Carlo step:
       * Sample the following random values对下面的随机变量抽样
       * R=Rand(Rule set, PPS Sampling of ruleActivity/systemActivity) 从所有匹配的反应规则中，按照每条反应规则的活跃度的概率分布生成反应规则R
       * m=Rand(M(a,R), Uniform distribution of 1/M(a,R))按照均匀分布，从反应规则R中生成要反应的匹配m
       */
      // sample rule 抽样规则InversionSampling括号里的是：遍历ruleActivities中规则活跃度>0的集合，将它们组成二元组(规则index,规则活跃度/系统活跃度)传给InversionSampling函数 
      val ru: Int = StochasticSimulator.InversionSampling(ruleActivities.filter(r => r._2 > 0).map(r => { r._1 -> r._2 / systemActivity }))
      println("which rule:" + ru + " match size:" + matchMap(ru).size) //ru为待反应规则下标，matchMap(ru)为待反应规则的匹配集合Set[Match]
      // sample match
      val re: RandomEngine = RandomEngine.makeDefault //生成一个随机数引擎
      val un: Uniform = new Uniform(0, matchMap(ru).size - 1, re)  //生成最小值为0最大值为匹配集合size-1的均匀分布,(0到size-1)
      val ma: Match = matchMap(ru).toList(un.nextInt())  //返回一个均匀分布随机数，即等概率的在Set[Match]集合中选一个Match出来反应
      val ex: Exponential = new Exponential(systemActivity, re) //Exponential(double lambda, RandomEngine randomGenerator)构造一个负指数分布，构造系统活跃度的指数分布
      val tIncr: Double = ex.nextDouble() //从指数分布中生成一个随机数作为下次发生反应的时间间隔，即现在的时间加上时间间隔是下次反应的时间
      println("time Incr:" + tIncr)

      /**
       * 2. Update:
       * Update the simulation state
       * perform reaction
       * time = time + tIncr 下次反应开始的时间点
       * update set of matches
       * update rule activities
       * update system activity
       */
      var newb: Bigraph = curb.applyMatch(ma)//实施选出的match
      simPath += newb.root //新bigraph的root进simPath
      simRules += ma.rule.name //实施反应规则列表
      var newv: Vertex = null
      time += tIncr //更新模拟时间
      if (curb != null && newb != null) {
        /** update a reaction rule data model */
        ma.rule.update //更新数据，调了没做映射的
        /** update agent data with clock */
        //DataModel.updateDataCalcsWithClk(tIncr.toString)
        if (newb.root == null)
          newb.root = new Nil();
        newv = new Vertex(newb, curv, ma.rule)
        newv.sysClk = time 
        newv.parent = curv
        if (g.lut.contains(newv.hash)) { //构造lut
          newv = g.lut(newv.hash);
          newv.addParents(curv)
        } else {
          g.add(newv);
        }
        curv.addTarget(newv, ma.rule);
        states += (time -> newv)
        if (GlobalCfg.printMode) {
          printf("%s:%s\n", "N_" + Math.abs(newv.hash), newv.bigraph.root.toString);
        }
      }
      // clean all the containers and update
      curv = newv
      curb = newb //更新为新偶图
      matchMap.empty
      systemActivity = 0.0
      ruleActivities.empty
      ruleIndex = 0
      b.rules.map(r => {
        ruleIndex += 1
        var matches: Set[Match] = curb.findMatchesOfRR(r) //更新所有匹配集合
        matchMap += ruleIndex -> matches
        println("ruleIndex:" + ruleIndex + " matches:" + matches.size)

        //calculate rule activities
        val ruleActivity = r.rate * matches.size //更新规则r的活跃度
        ruleActivities += ruleIndex -> ruleActivity 

        //calculate system activity
        systemActivity += ruleActivity//更新整个系统活跃度，准备下次循环，只要systemActivity>0就继续循环，systemActivity=0系统模拟结束
      })
    }

    dump(simPath, simRules.size)
    StochasticSimulator.matchGC;
  }

  def dump(simPath: Queue[Term], simRules: Int) {//打印到path文件
    dumpDotForward("")
    //GlobalCfg.node = false
    if (GlobalCfg.pathOutput != "") {
      var out: String = "{\n"
      while (!simPath.isEmpty)
        out += simPath.dequeue.toString + "\n"
      out += "}" + (100 * simRules.toDouble / b.rules.size).toString + "%\n"
      println(out)
      var file: File = new File(GlobalCfg.pathOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
    }
    GlobalCfg.node = true
  }

  def formatHash(hash: Int): String = {
    if (hash < 0) "_" + hash.abs;
    else hash.toString;
  }

  def dumpDotForward(dot:String): String = {//打印到dot文件
    var out: String = "";
    out += "digraph reaction_graph {\n";
    out += "   rankdir=LR;\n";
    out += "   Node [shape = circle];\n";
    out += "   BigSim_Report [shape = parallelogram color = aliceblue style=filled label=\"BigSim\nReport\"];\n"
    out += "BigSim_Report -> N_" + formatHash(g.root.hash) + "[color = aliceblue label = \"" +
      Data.getWeightExpr + "=" +
      Data.getReport + "\n" +
      Data.getValues(",") + "\"];\n";
    out += " N_" + formatHash(g.root.hash) + "\n" + " [shape=circle, color=lightblue2, style=filled];\n";
    g.lut.values.map(x => {
      var rr: String = "root";
      var dc: String = "";
      if (x.terminal) {
        dc = "shape = doublecircle, color=lightblue2, style=filled, ";
      }
      out += "N_" + formatHash(x.hash) + "[ " + dc + "label=\"N_" + formatHash(x.hash) + "\"];\n";
      x.target.map(y => {
        rr = "?";
        if (y._2 != null)
          rr = y._2.name;
        if (y._1 != null) {
          rr = rr + "\nSystem Clock: " + y._1.sysClk
          if (GlobalCfg.checkData && y._2.conds.size != 0)
            rr = rr + "\nCond:" + y._2.getConds
          if (GlobalCfg.checkHMM && y._2.hmms.size != 0)
            rr = rr + "\nHMM:" + y._2.getHMM
          out += " N_" + formatHash(x.hash) + " -> N_" + formatHash(y._1.hash) + "[ label = \"" + rr + "\n" + y._1.variables + "\"];\n"
        }
      });
    });
    out += "}\n";
    if (GlobalCfg.graphOutput != "") {
      var file: File = new File(GlobalCfg.graphOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
    }
    out;
  }

}
