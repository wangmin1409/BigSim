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
import jdk.nashorn.internal.objects.Global

abstract class Simulator {
  var dot: String = "";//子类构造dot
  def simulate: Unit;//每个子类重写的是这个抽象方法
  def dumpDotForward(dot: String): String;
}

object Simulator {

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

class SimulatorFactory {

  var simulator: Simulator = null;

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
    
   

    for (i <- 1 to GlobalCfg.simLoop) {//每次loop都new了一个新的simulator

      val b: Bigraph = BGMTerm.toBigraph(t);//每次loop从bgm文件构建一个偶图
     
      GlobalCfg.violateSorting=false
      GlobalCfg.violateBinding=false
      GlobalCfg.violateExceptionTracking=false

      // init variables for each loop simulation
      GlobalCfg.SysClk = 0
      GlobalCfg.curLoop = i

      if (GlobalCfg.checkData) Data.parseData(GlobalCfg.dataInput)
      if (GlobalCfg.isBackDerivation) Data.parseBDData(GlobalCfg.bdDataInput)
      if (GlobalCfg.checkHMM) HMM.parseHMM(GlobalCfg.hmmInput)
//      if (GlobalCfg.checkSorting) Bigraph.sorting.init(GlobalCfg.sortingInput)

      middle = System.currentTimeMillis();

      simulate(GlobalCfg.SimulatorClass, b)
      dot += simulator.dot
    }
    simulator.dumpDotForward(dot)//调用生成dot文件
    var end = System.currentTimeMillis();
//    println("\n****************************************************************")
//    println("  Total:\tmiddle:" + middle + ", end:" + end + ", used:" + (end - middle) + " ms");
//    println("  Total:\tmiddle:" + start + ", end:" + middle + ", used:" + (middle - start) + " ms");
//    println("  Total:\tstart:" + start + ", end:" + end + ", used:" + (end - start) + " ms");
//    println("****************************************************************")
    
    //分别是不包含读写的模拟时间和总时间（模拟时间+读写文件）
    println("\n****************************************************************")
    println("  Total:\tmiddle:" + MainSimulator.start + ", end:" + MainSimulator.middle + ", used:" + (MainSimulator.middle - MainSimulator.start) + " ms");
    println("  Total:\tstart:" + MainSimulator.start + ", end:" + MainSimulator.end + ", used:" + (MainSimulator.end - MainSimulator.start) + " ms");
    println("****************************************************************")
  }

  def simulate(sn: String, b: Bigraph): Unit = {
    sn match {
      case "TimeSlicingSimulator" => {
        simulator = new TimeSlicingSimulator(b)
      }
           case "TimeSlicingSimulator1" => {
        simulator = new TimeSlicingSimulator1(b)
      }
      case "EnumSimulator" => {
        simulator = new EnumSimulator(b)
      }
      case "DiscreteEventSimulator" => {
        simulator = new DiscreteEventSimulator(b)
      }
            case "DiscreteEventSimulator1" => {
        simulator = new DiscreteEventSimulator1(b)
      }
      case "StochasticSimulator" => {
        simulator = new StochasticSimulator(b)
      }
            case "StochasticSimulator1" => {
        simulator = new StochasticSimulator1(b)
      }
            case "EnumSimulator1" => {
        simulator = new EnumSimulator1(b)
      }      
      case "MainSimulator" => {
        simulator = new MainSimulator(b)
      }
      case _ => {
        println("Error with Simulator: Class " + sn + " not found.")
        return
      }
    }
    simulator.simulate //call
  }

}
