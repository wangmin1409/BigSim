package org.bigraph.bigsim.simulator

import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.model.Nil

/**
 * Enum Simulator is designed for enumerate all the agents
 * can be reacted into from the initial agent.
 * It will give the graph based state machines of agents.
 * Here only consider the relation of agents and rules, no time,
 * condition, and data calculation.
 */
object EnumSimulator {
  var matchDiscard: Set[Match] = Set();

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }
}

class EnumSimulator(b: Bigraph) extends Simulator {
  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);
  var workQueue: Queue[Vertex] = Queue();
  workQueue.enqueue(v);//初始节点进队列
  var steps: Int = 0;
  var reachedAgent: Map[Long, Boolean] = Map();

  def simulate: Unit = {
    if (b == null || b.root == null) {
      println("enum simulator::simulate(): null");
      return ;
    } else {
      while (step()) {};
      EnumSimulator.matchGC;
    }
  }

  def report(step: Int): String = {
    GlobalCfg.node = false
    if (GlobalCfg.outputPath)
      g.dumpPaths
    GlobalCfg.node = true
    g.dumpDotForward;//调的Graph类的dumpDotForward，本类没有重写父类抽象方法，只是简单置为空实现
  }

  def step(): Boolean = {
    /** if reach the max steps */
    if (steps >= GlobalCfg.maxSteps) {
      println("mc::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      report(steps);
      return false;
    }
    /** if the working queue is empty */
    if (workQueue.size == 0) {
      println("enum simulator::step Complete!");
      report(steps);
      EnumSimulator.matchGC;
      return false;
    }
    /** get the top element of working queue */
    var v: Vertex = workQueue.dequeue();
    /** if the current agent has been reachedAgent, then stop */
    if (reachedAgent.contains(v.hash)) {//又回到刚才到过的节点，出现环，跳出本次循环，进入下一次循环
      return true;
    }

    steps += 1;
    var step: Int = steps;
    var b: Bigraph = v.bigraph;
    
    println("ywlog:======");
    println(b);
    println("ywlog:======");
    
    var matches: Set[Match] = b.findMatches;

    /** if no match of this agent, it will be a terminal agent */
    if (matches.size == 0) {
      v.terminal = true;
    }
    reachedAgent(v.hash) = true;

    matches.map(it => {
      var rr: ReactionRule = it.rule;

      /** apply match to turn into new agent */
      var nb: Bigraph = b.applyMatch(it);
      if (nb.root == null) nb.root = new Nil();
      var nv: Vertex = new Vertex(nb, v, rr);
      if (!GlobalCfg.checkLocal) {
        if (g.lut.contains(nv.hash)) {
          nv = g.lut(nv.hash);
          nv.addParents(v)
        } else {
          /** new agent has not been reached, put into the working queue */
          workQueue.enqueue(nv);
          g.add(nv);
        }
        v.addTarget(nv, rr);
        //v.addTargets(rr, nv);
      } else {
        /** We've not reachedAgent this one! */
        if (!reachedAgent.contains(nv.hash) && nv.bigraph.root != null) {
          workQueue.enqueue(nv);
        }
      }
    });
    matches.clear();
    EnumSimulator.matchGC;

    if (GlobalCfg.reportInterval > 0 && step % GlobalCfg.reportInterval == 0) {
      println(report(step));
    }
    if (GlobalCfg.printMode) {
      printf("%s:%s\n", "N_" + Math.abs(v.hash), v.bigraph.root.toString);
    }
    if (!checkProperties(v)) { //模型性质检测
      println("mc::step Counter-example found.")
      return false;
    }
    true;
  }

  def checkProperties(v: Vertex): Boolean = {
    if (v.visited) return true;
    /** check sorting */
//    if (GlobalCfg.checkSorting) {
//      var sortingCheckRes = Bigraph.sorting.check(v);
//      if (!sortingCheckRes) {
//        println("*** Found violation of Sorting: " + Bigraph.sorting.violationInfo);
//        if (!GlobalCfg.localCheck)
//          println(g.backTrace(v));
//        else
//          println("[Backtrace unavailable in local checking mode]");
//        return false;
//      }
//    }
    v.visited = true;
    true;
  }
  
    def dumpDotForward(dot: String): String = ""

}