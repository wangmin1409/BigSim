package org.bigraph.bigmc

import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.Set
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.parser._

object MC {
  var properties: Map[String, Query] = Map();
  var matchDiscard: Set[Match] = Set();

  def addProperty(n: String, q: Query): Unit = {
    properties(n) = q;
  }

  // 暂时先实现单线程，多线程后面再加
  def threadWrapper(): Unit = {
    // todo
  }

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }
}

class MC(b: Bigraph) {
  var v: Vertex = new Vertex(b, null, null);
  var g: Graph = new Graph(v);
  var workQueue: Queue[Vertex] = Queue();
  workQueue.enqueue(v);
  var steps: Int = 0;
  var checked: Map[Long, Boolean] = Map();

  def check: Unit = {
    if (b == null || b.root == null) {
      println("mc::check(): null");
      return ;
    } else {
      // 一直检查直到遇到反例
      while (step()) {};
      MC.matchGC;
    }
  }

  def report(step: Int): String = {
    GlobalCfg.node = false
    if (GlobalCfg.pathOutput != "")
      g.dumpPaths
    GlobalCfg.node = true
    g.dumpDotForward;
  }

  def step(): Boolean = {
    // 是否达到最大检测步数
    if (steps >= GlobalCfg.maxSteps) {
      println("mc::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      report(steps);
      return false;
    }
    // 待检测模型队列是否为空
    if (workQueue.size == 0) {
      println("mc::step Complete!");
      report(steps);
      MC.matchGC;
      return false;
    }
    // 取出队首模型
    var v: Vertex = workQueue.dequeue();
    if (checked.contains(v.hash)) { //若模型已检查过则跳过
      return true;
    }

    steps += 1;
    var step: Int = steps;
    var b: Bigraph = v.bigraph;
    // 模型使用规则进行匹配
    var matches: Set[Match] = b.findMatches;

    //    println("Bigraph namemap : " + Bigraph.nameMap)
    //    println("Bigraph controlMap : " + Bigraph.controlMap)
    //    println("Bigraph modelNames : " + Bigraph.modelNames)

    //    println("b.findMatches("+ matches.size + "): ")// + matches);

    if (matches.size == 0) { //没有可以匹配的规则，该节点为终结节点
      v.terminal = true;
    }
    checked(v.hash) = true;

    matches.map(it => { // 遍历所有匹配上的规则
      var rr: ReactionRule = it.rule;

      //printf("%s:%s\n", "N_" + Math.abs(v.hash), v.bigraph.root.toString);
      // 模型转化，生成新模型
      var b2: Bigraph = b.applyMatch(it);
      //println("Match:" + it);
      //println("Rule:" + it.rule);
      //println("Before applyMatch:" + b.root);
      //println("After applyMatch: " + b2.root + "\n");
      //println("m:" + it);
      //println("m.ctrlMap:" + it.ctrlMap);
      //println("m.rule:" + it.rule);
      //println("b:" + b.root);
      //println("b2:" + b2.root);

      if (b2.root == null) b2.root = new Nil();
      var v2: Vertex = new Vertex(b2, v, rr);
      if (!GlobalCfg.checkLocal) {
        if (g.lut.contains(v2.hash)) {
          v2 = g.lut(v2.hash);
          v2.addParents(v)
        } else {
          // 若新模型尚未检测过，加入待检测队列。
          workQueue.enqueue(v2);
          g.add(v2);
        }
        v.addTarget(v2, rr);
        //v.addTargets(rr, v2);
      } else {
        // We've not checked this one!
        if (!checked.contains(v2.hash) && v2.bigraph.root != null) {
          workQueue.enqueue(v2);
        }
      }
    });
    matches.clear();
    MC.matchGC;

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

    // sorting 约束检查
//    if (GlobalCfg.checkSorting) {
//      var sortingCheckRes = Bigraph.sorting.check(v);
//      if (!sortingCheckRes) {
//        println("*** Found violation of Sorting: " + Bigraph.sorting.violationInfo);
//        if (!GlobalCfg.localCheck) println(g.backTrace(v));
//        else println("[Backtrace unavailable in local checking mode]");
//        return false;
//      }
//    }

    /*
    for (entry <- MC.properties.toList) {
      if (!entry._2.check(v)) {
        println("*** Found violation of property: " + entry._1);
        println("*** " + entry._1 + ": " + entry._2.toString);
        if (!GlobalCfg.checkLocal) println(g.backTrace(v));
        else println("[Backtrace unavailable in local checking mode]");
        return false;
      }
    }*/
    v.visited = true;
    true;
  }

}
