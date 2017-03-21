package org.bigraph.bigsim.simulator

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.parser.BGMParser
import java.io.File
import org.bigraph.bigsim.parser.BGMTerm
import org.bigraph.bigsim.parser.HMM
import org.bigraph.bigsim.data.Data

object Simulator1 {

  var matchDiscard: Set[Match] = Set();
  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }

  def simulate {
    var simFactory = new SimulatorFactory();
    simFactory.simulate();
  }
}

class Simulator1 {
  var dot: String = "";
 // def simulate: Unit;//每个子类重写的是这个抽象方法
 // def dumpDotForward(dot: String): String;
    def simulate(sn: String, b: Bigraph): Unit = {
      
    var start = System.currentTimeMillis();
    var middle: Long = 0;

    // parse the BGM input file
    val t: List[BGMTerm] = BGMParser.parse(new File(GlobalCfg.filename));
    var dot: String = ""

    for (i <- 1 to GlobalCfg.simLoop) {

      val b: Bigraph = BGMTerm.toBigraph(t);

      // init variables for each loop simulation
      GlobalCfg.SysClk = 0
      GlobalCfg.curLoop = i

      if (GlobalCfg.checkData) Data.parseData(GlobalCfg.dataInput)
      if (GlobalCfg.checkHMM) HMM.parseHMM(GlobalCfg.hmmInput)
//      if (GlobalCfg.checkSorting) Bigraph.sorting.init(GlobalCfg.sortingInput)

      middle = System.currentTimeMillis();

      //simulate(GlobalCfg.SimulatorClass, b)
      //dot += simulator.dot
    }
    //simulator.dumpDotForward(dot)
    var end = System.currentTimeMillis();
    println("\n****************************************************************")
    println("  Total:\tmiddle:" + middle + ", end:" + end + ", used:" + (end - middle) + " ms");
    println("  Total:\tstart:" + start + ", end:" + end + ", used:" + (end - start) + " ms");
    println("****************************************************************")
  
}
}
