package org.bigraph.bigsim.modelchecker

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
import scala.collection.immutable.TreeMap
import org.bigraph.bigsim.data.Data
import org.bigraph.bigsim.strategy.PatternFlow
import org.bigraph.bigsim.model.BiNode;

/**
 * @author amy
 */
object MCMainSimulator {
  var matchDiscard: Set[Match] = Set();
  var v: Vertex = null
  var g: Graph = null
  var start: Long = System.currentTimeMillis();
  var middle: Long = 0
  var end: Long = 0

  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }  
}

class MCMainSimulator(b: Bigraph) extends MCSimulator {
  MCMainSimulator.v = new Vertex(b, null, null);
  MCMainSimulator.g = new Graph(MCMainSimulator.v);
  
  def simulate: Unit = {
    if (b == null || b.root == null) {
      println("MCMainSimulator::simulate(): initial bigraph is null");
      return;
    }
    
    var rules: Set[ReactionRule] = b.rules;
    if (rules == null || rules.size == 0) {
      println("MCMainSimulator::simulate(): no reaction rules");
    }
    
    var enumFeature = new MCEnumFeature(b);
    // set the head of the BiNode, initial Bigraph
    var biNode: BiNode = new BiNode(b,Map());
    BiNode.head = biNode;
    BiNode.addBiNode(biNode);
    
    while (enumFeature.step()) {
      MCSimulator.matchGC
    }
    println("Graph: " + MCMainSimulator.g.lut.size);
    MCMainSimulator.g.dumpPath //打印到data和path文件
    MCMainSimulator.g.dumpDotFile //打印到dot文件
    MCMainSimulator.matchGC; //Test，可能无用
    
  }
 
  def dumpDotForward(dot: String): String = ""
}
