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
import org.bigraph.bigsim.Verify

/**
 * Enum Simulator is designed for enumerate all the agents
 * can be reacted into from the initial agent.
 * It will give the graph based state machines of agents.
 * Here only consider the relation of agents and rules, no time,
 * condition, and data calculation.
 */


class EnumFeature(b: Bigraph){
  var v: Vertex = MainSimulator.v
  var workQueue: Queue[Vertex] = Queue();
  workQueue.enqueue(v);//初始节点进队列
  var steps: Int = 0;
  var reachedAgent: Map[Long, Boolean] = Map();

  def step(): Boolean = {
    /** if reach the max steps */
    if (steps >= GlobalCfg.maxSteps) {
      println("mc::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      return false;
    }
    /** if the working queue is empty */
    if (workQueue.size == 0) {
      println("enum simulator::step Complete!");
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
    
    Verify.AddModel(b);
    
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
        if (MainSimulator.g.lut.contains(nv.hash)) {
          nv = MainSimulator.g.lut(nv.hash);
          nv.addParents(v)
        } else {
          /** new agent has not been reached, put into the working queue */
          workQueue.enqueue(nv);
          MainSimulator.g.add(nv);
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

    if (GlobalCfg.printMode) {
      printf("%s:%s\n", "N_" + Math.abs(v.hash), v.bigraph.root.toString);
    }
    true;
  }

}