package org.bigraph.bigsim.modelchecker

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
import jdk.nashorn.internal.objects.Global
import org.bigraph.bigsim.model.BiNode;
/**
 * @author amy
 */

abstract class MCSimulator {
  var dot: String = "";//子类构造dot
  def simulate: Unit;//每个子类重写的是这个抽象方法
  def dumpDotForward(dot: String): String;
}

object MCSimulator {
  var matchDiscard: Set[Match] = Set();
  def matchMarkDelete(m: Match): Unit = {
    assert(m != null);
    matchDiscard.add(m);
  }

  def matchGC: Unit = {
    matchDiscard.clear();
  }

  def simulate {
    var simFactory = new MCSimulatorFactory();
    simFactory.simulate();
  }
}

class MCSimulatorFactory {
  var simulator: MCSimulator = null;
  
  def simulate() {

    var start = System.currentTimeMillis();
    var middle: Long = 0;
    
    var t:List[BGMTerm] = null;
    
    // parse the BGM input file
    try{
      if(GlobalCfg.IsFromNetWork){
      t = BGMParser.parseFromString(GlobalCfg.bgmContent)
    }
    else
      t = BGMParser.parse(new File(GlobalCfg.filename));
    }catch {
      case t: Throwable => {t.printStackTrace();return;} // TODO: handle error    
    }
 
    var dot: String = ""
    
    var b: Bigraph = BGMTerm.toBigraph(t); //从bgm文件构建一个偶图
    simulator = new MCMainSimulator(b)
    simulator.simulate //call
    dot += simulator.dot
    simulator.dumpDotForward(dot)//调用生成dot文件
    var end = System.currentTimeMillis();
   
    //分别是不包含读写的模拟时间和总时间（模拟时间+读写文件）
    println("\n****************************************************************")
    println("  Total:\tmiddle:" + MCMainSimulator.start + ", end:" + MCMainSimulator.middle + ", used:" + (MCMainSimulator.middle - MCMainSimulator.start) + " ms");
    println("  Total:\tstart:" + MCMainSimulator.start + ", end:" + MCMainSimulator.end + ", used:" + (MCMainSimulator.end - MCMainSimulator.start) + " ms");
    println("****************************************************************")
  }
}

