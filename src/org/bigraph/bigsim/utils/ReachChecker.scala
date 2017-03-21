package org.bigraph.bigsim.utils

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.parser.BGMParser
import org.bigraph.bigsim.parser.BGMTerm
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.model.Nil

object ReachChecker {
  var matchDiscard: Set[Match] = Set();


  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }
}

class ReachChecker(bgm: String) {
  val p = BGMParser.parseFromString(bgm)
  val b: Bigraph = BGMTerm.toBigraph(p);
  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);
  var workQueue: Queue[Vertex] = Queue();
  workQueue.enqueue(v);
  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();
  var reachedRRs: TreeMap[String, Boolean] = TreeMap()

  def report: String = {
    var report: String = "Unreached Reaction Rules:\n"
    reachedRRs.map(rr => {
      if (!rr._2)
        report += rr._1 + "\n"
    })
    report
  }

  def check: String = {

    /**
     * In reach check, no time and data needed
     */
    GlobalCfg.checkData = false
    GlobalCfg.pathOutput = ""

    /**
     * Add all rules in reachedRRs
     */
    b.rules.foreach(rr => {
      reachedRRs += rr.name -> false
    })

    if (b == null || b.root == null) {
      println("reach check::check(): null");
      report
    } else {
      /**
       * keep checking the agent until all rules reach.
       */
      while (step()) {};
      report
    }
  }

  def step(): Boolean = {
    // If reach the max steps, break the check
    if (steps >= GlobalCfg.maxSteps) {
      println("reach check::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      return false;
    }
    if (workQueue.size == 0) {
      println("reach check::step Complete!");
      ReachChecker.matchGC;
      return false;
    }
    var v: Vertex = workQueue.dequeue();
    if (checked.contains(v.hash)) {
      return true;
    }

    steps += 1;
    var step: Int = steps;
    var b: Bigraph = v.bigraph;

    var matches: Set[Match] = b.findMatches;
    if (matches.size == 0) {
      v.terminal = true;
    }
    checked(v.hash) = true;

    matches.map(it => {
      var rr: ReactionRule = it.rule;
      reachedRRs += rr.name -> true;
      var nb: Bigraph = b.applyMatch(it);
      
      if (nb.root == null) nb.root = new Nil();
      var nv: Vertex = new Vertex(nb, v, rr);
      if (g.lut.contains(nv.hash)) {
        nv = g.lut(nv.hash);
        nv.addParents(v)
      } else {
        workQueue.enqueue(nv);
        g.add(nv);
      }
      v.addTarget(nv, rr);
    });
    matches.clear();
    ReachChecker.matchGC;
      if (GlobalCfg.printMode) {
      printf("%s:%s\n", "N_"+ Math.abs(v.hash), v.bigraph.root.toString);
    }
    true;
  }
}
