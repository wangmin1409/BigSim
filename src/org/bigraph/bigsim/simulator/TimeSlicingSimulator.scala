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
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.Data
import org.bigraph.bigsim.utils.Graphviz

object TimeSlicingSimulator {

  var matchDiscard: Set[Match] = Set();

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }
}

class TimeSlicingSimulator(b: Bigraph) extends Simulator {

  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);
  var states: Queue[Tuple2[Double, Vertex]] = Queue();
  var simQueue: TreeMap[Double, Queue[Match]] = TreeMap();
  var reactNodes: Set[String] = Set();

  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();

  var path: List[String] = List();
  var variables: List[String] = List();
  var pathColor: String = Graphviz.getColor

  def simulate: Unit = {
    // add the initial agent to the simQueue
    states += ((0, v))

    if (b == null || b.root == null) {
      println("time slicing simulator::simulate(): null");
      return ;
    } else {
      // keep simulate until the end
      while (step()) {
        Simulator.matchGC
      };
      report()//打印到data和path文件
      TimeSlicingSimulator.matchGC;
    }
  }

  def report() {
    GlobalCfg.node = false
    if (GlobalCfg.pathOutput != "" && GlobalCfg.outputPath) {
      var writer: Writer = new FileWriter(GlobalCfg.pathOutput, GlobalCfg.append);
      writer.write(GlobalCfg.curLoop + "{\n");
      writer.write(path.mkString("\n"));
      writer.write("\n}\n");
      writer.close();
    }
    GlobalCfg.node = true
    if (GlobalCfg.dataOutput != "" && GlobalCfg.outputData) {
      var writer: Writer = new FileWriter(GlobalCfg.dataOutput, GlobalCfg.append);
      writer.write(GlobalCfg.curLoop + "{\n");
      writer.write(variables.mkString("\n"));
      writer.write("\n}\n");
      writer.close();
    }
    GlobalCfg.append = true
  }

  def step(): Boolean = {

    /**
     * if meet max system clock, simulation stop.
     */
    println("GlobalCfg.SysClk: " + GlobalCfg.SysClk);//lbj
    if (GlobalCfg.SysClk > GlobalCfg.maxSysClk) {
      println("GlobalCfg.SysClk: " + GlobalCfg.SysClk);//lbj
      println("sim::step Interrupted!  Reached maximum SysClk: " + GlobalCfg.maxSysClk);
      return false;
    }

    /**
     * 0: update
     * If sim queue contains reactions happen at this time,
     * try match and apply match.
     */
    update()//这就是对simQueue中reactTime=SysClk的applyMatch

    /**
     * 1: add match
     */
    if (!addMatch()) {//这里就是调match方法，正常返回true，那就不会走这里，不正常返回false，才会走这里，这就是findMatch，reactTime是反应后的时间，这里是先把反应时间推进，在apply反应
      return false
    }

    /**
     * 2: update if current match doesn't need reaction time
     * If sim queue contains reactions happen at this time,
     * try match and apply match.
     */
    update() //只有非random的且自己定义了SysClk为0的这里的update才会进去，因为addMatch的 var reactTime = GlobalCfg.SysClk + RRIncr的RRIncr为0
             //这时的reactTime=SysClk，在SysClk加1之前applyMatch

    TimeSlicingSimulator.matchGC;
    // update the system clk
    GlobalCfg.SysClk = GlobalCfg.SysClk + GlobalCfg.SysClkIncr
    Data.update("SysClk", GlobalCfg.SysClk.toString)
    true;
  }

  /**
   * update
   * update matches once the system clock meets
   */
  def update() { //第一次都不会走，第一次simQueue都为空，除非有反应时间为0的
    
     //println("simQueue: " + simQueue);//lbj

    if (simQueue.contains(GlobalCfg.SysClk)
      && !simQueue.get(GlobalCfg.SysClk).isEmpty) {//第二次进update，simQueue里包含SysCLK为1

      if (GlobalCfg.verbose)
        println("Update-----System Clock now:" + GlobalCfg.SysClk)
        
      // apply these matches
      var reactList: Queue[Match] = simQueue.getOrElse(GlobalCfg.SysClk, Queue())//返回key指定的value，如果没有则返回指定的默认值，这里如果Map里没有则返回空队列
      // match and find match and apply match!!!
      var v: Vertex = states.last._2
      var curBigraph = v.bigraph
      var curRRs: Set[ReactionRule] = Set()
      // record the cond
      var rules: Map[String, List[String]] = Map()
      var conds: List[String] = List()

      while (reactList.size > 0) {//当前可并发反应的规则
        var tm = reactList.dequeue
        var matches: Set[Match] = curBigraph.findMatchesOfRR(tm.rule)//当前bigraph与一个反应规则匹配
        if (matches != null) {
          var matched: Boolean = false//去重
          matches.map(m => {
            if (GlobalCfg.verbose) {
              println(m.rule.name + "," + m.getReactNodes + "," +
                tm.getReactNodes + "matched:" + matched)
            }

            if (!matched && m.getReactNodes.equals(tm.getReactNodes)) {//因为addMatch查找了一次放在simQueue的reactList中，那次的匹配为tm，
                                                                       //这里又查了一次，匹配为m，两次查出匹配的reactNode要一样，这里就是确保一下，就算m和tm都为空也是equals的，因为空串相等的
              var nb: Bigraph = curBigraph.applyMatch(m)
              /**
               * update a reaction rule data model
               */
              m.rule.update(tm) //更新data，看了reactNodes
              /**
               * update agent data with clock
               */
              Data.updateDataCalcsWithClk(tm.RRIncr.toString)

              curRRs += m.rule

              if (rules.contains(m.rule.name)) {//如果rules的key已经有这条反应规则name，则把新node加入value的List集合中
                rules.put(m.rule.name, rules.getOrElse(m.rule.name, List()).++(m.reactNodes.toList))
              } else {//如果rules的key没有这条反应规则name，则新建一个value
                rules.put(m.rule.name, m.reactNodes.toList) //eg. var rules: Map[String, List[String]]为Map(r_businessman_take_taxi_from_CBD -> List(David))这个规则作用了哪些nodes
              }
              if (!m.rule.getConds.equals("")) {//若这个match的反应规则有条件，则加入到conds这个集合中
                conds = conds.:+(m.rule.getConds)
              }

              if (GlobalCfg.verbose) {
                println("-----react nodes before:" + reactNodes)
              }

              reactNodes = reactNodes.filter(!m.reactNodes.contains(_))//过滤出符合filter条件的元素，即把不在集合m.reactNodes中的元素放入reactNodes，即过滤掉m.reactNodes的东西 
              if (GlobalCfg.verbose) {
                println("-----reaction nodes rm:" + m.reactNodes)//-----react nodes before:Set(jerry, David, Tim, james)
                println("-----react nodes after:" + reactNodes)//-----react nodes before:Set(jerry, Tim, james) 表示David已经反应了
              }
              
              matched = true //这个match已经发生过反应
              if (nb.root == null) {
                nb.root = new Nil();
              }
              if (GlobalCfg.verbose) {
                println("middle result match RR " + tm.rule.name + " : " + nb.root.toString)
                println("middle result of variables: " + Data.getValues(","))
              }
              curBigraph = nb
            }
          })
        }
      }

      if (curBigraph != null && curRRs != null) {
        var nv = new Vertex(curBigraph, v, curRRs, true)
        nv.sysClk = GlobalCfg.SysClk

        if (GlobalCfg.outputGraph) {//这个dot是父类定义的，子类构造好也是传给父类的方法调
          dot += "N_" + formatHash(nv.hash) +
            "[ " + "label=\"N_" + formatHash(nv.hash) + "\"];\n";
          dot += " N_" + formatHash(v.hash) +
            " -> N_" + formatHash(nv.hash) + "[ color = " + pathColor + " label = \"SysClk:" + GlobalCfg.SysClk +
            "\n" + rules.mkString(",") + "\"];\n"
        }
        if (GlobalCfg.outputPath) { //放到集合path: List[String]中
          path = path.:+(nv.bigraph.root.toString)
        }
        if (GlobalCfg.outputData) { //放到集合variables: List[String]中
          variables = variables.:+(nv.variables)
        }

        if (g.lut.contains(nv.hash)) {//建立nv和v的父子关系放到Graph的look up table中
          nv = g.lut(nv.hash);
          nv.addParents(v)
        } else {
          g.add(nv);
        }

        v.addTargets(curRRs, nv);
        
        states += (GlobalCfg.SysClk -> nv) //这个SysClk生成的vertex
        if (GlobalCfg.printMode) {
          print("SysClk:" + GlobalCfg.SysClk + "\t")
          printf("%s:%s\n", "N_" + Math.abs(nv.hash), nv.bigraph.root.toString);
          //println(nv.variables)
        }
      }

      // finally, delete it!
      simQueue = simQueue.-(GlobalCfg.SysClk) //applyMatch后反应完了就删掉，所以下次没有了
    }
  }

  def addMatch(): Boolean = { //此方法加到simQueue
    var v: Vertex = states.last._2
    steps += 1;
    var b: Bigraph = v.bigraph;
    var matches: Set[Match] = b.findMatches;

    if (steps >= GlobalCfg.maxSteps) {
      println("sim::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      return false;
    }

    /**
     * if current model can not find matches and the sim queue is empty,
     * then the simulation is over.
     * Abandon! not right
     */
    /*
    if (matches.isEmpty && simQueue.isEmpty) {
      println("sim::step Complete!");
      report(steps);
      TimeSlicingSimulator.matchGC;
      return false;
    } 
    */

    /**
     * if there is no match, but the sim queue is not emputy
     *
     * if (matches.isEmpty && !simQueue.isEmpty) {
     * println("Current Agent Can not match rules, jump to time:" + simQueue.firstKey)
     * GlobalCfg.SysClk = simQueue.firstKey
     * return true
     * }
     */

    //println("Add match-----matches size: " + matches.size)

    /**
     * If a reaction rule is not random and not conflict,
     * it must happen when it is matched.
     */
//    println("lbj---matches set size:"+matches.size)
    matches.map(m => {
//      if (GlobalCfg.verbose) {
//        println("All match:" + m.rule.name + "\tcond:" +
//          m.rule.conds + "\treactNodes:" + m.reactNodes)
//      }
          
      val conflict = m.conflict(reactNodes.toList)
      if (!conflict) {
        //if (!conflict && !m.rule.random) {
        val RRIncr = m.rule.getRRIncr //这里调用的是整数，只会是每个时间点
        var reactTime = GlobalCfg.SysClk + RRIncr//非随机这个RRIncr就是bgm文件定义的SysClk，reactTime就是反应完的时间点，如第一次反应变为1
        m.RRIncr = RRIncr
        var queue: Queue[Match] = null
        if (simQueue.contains(reactTime)) {//所有反应完为这个reactTime的都放到simQueue的key为reactTime中，如反应完时间为1的都放入simQueue(1,反应完时间都为1的Queue[Match])
          queue = simQueue(reactTime)
          queue += m
        } else {
          queue = Queue(m) //每个时间点第一个匹配会走这里
        }
        simQueue += reactTime -> queue //每个时间点所有能反应的match组成一个queue，如第一次匹配，SysClk从0开始，第一次反应，reactTime变为1
        reactNodes ++= m.reactNodes //把处理过的node放入
        if (GlobalCfg.verbose) {
          println("add match: " + m.rule.name + "\treact nodes:" +
            m.reactNodes + "\treact time:" + reactTime)
        }
        //matches -= m
      } else if (conflict) {
        //matches -= m
      }
    })
    return true;
  }

  def formatHash(hash: Int): String = {//统一后这个方法就不用再写一遍了
    if (hash < 0) "_" + hash.abs;
    else hash.toString;
  }

  def dumpDotForward(dot: String): String = {//本类没调，在父类Simulator中统一调的，子类都重写了这个方法，dot字符串也是父类定义子类写了后传进来的
    var out: String = "";
    out += "digraph reaction_graph {\n";
    out += "   rankdir=LR;\n";
    out += "   Node [shape = circle];\n";
    out += "   BigSim_Report [shape = parallelogram color = aliceblue style=filled label=\"BigSim\nReport\"];\n"
    out += "BigSim_Report -> N_" + formatHash(g.root.hash) + "[color = aliceblue label = \"" +
      Data.getWeightExpr + "=" +
      Data.getReport + "\n" + //Data.getValues(",") +
      "\"];\n";
    out += " N_" + formatHash(g.root.hash) + "\n" + " [shape=circle, color=lightblue2, style=filled];\n";
    out += dot;
    out += "}\n";
    if (GlobalCfg.graphOutput != "" && GlobalCfg.outputGraph) {
      var file: File = new File(GlobalCfg.graphOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
      writer.close();
    }
    out;
  }
}
