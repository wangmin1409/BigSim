package org.bigraph.bigmc.specification


import java.util.ArrayList
import scala.collection.JavaConversions._
import rwth.i2.ltl2ba4j.model.ITransition
import rwth.i2.ltl2ba4j.model.IGraphProposition
import org.bigraph.bigmc.model.BigraphPair;
import org.bigraph.bigsim.model.Bigraph

 

/**
 * @author amy
 */
object GenBigraph {
  def main(args: Array[String]): Unit = {
    getBigraphPair();
  }
  
  def getBigraphPair(): List[BigraphPair] = {
    var automaton = new ArrayList[ITransition]();
    automaton = GenTS.genTS(); 
    var bigraphPairLIst: List[BigraphPair] = List()
    var bigraphPair: BigraphPair = null;
    var trueB: Bigraph = null;
    var init: Bigraph = null;
    automaton.foreach { x => 
      var labelSet: scala.collection.mutable.Set[IGraphProposition] = x.getLabels;
      println(labelSet.size)
      if(labelSet.size == 1) {
        if (isSIGMA(labelSet)) {
          println("label是siegma");
          if (x.getSourceState.isFinal()) {
            trueB.isFinal = true;
            bigraphPair.init(trueB, trueB); 
            bigraphPairLIst.add(bigraphPair);
          }else if(x.getSourceState.isInitial()) {
            init.isInitial = true;
            bigraphPair.init(init, init);
            bigraphPairLIst.add(bigraphPair);
          } else {
            
          }
        }
      }
      println(x) }
    var res: List[BigraphPair] = List()
    return res;
  }
  
 /**
  * 判断label是否为<SIGMA>
  */
  def isSIGMA(labelSet: scala.collection.mutable.Set[IGraphProposition]): Boolean = {
    var res: Boolean = false
    labelSet.foreach { x => 
     if (("<SIGMA>").equals(x.getLabel())) {
       res = true;
     }
    }
    return res;
  }
 
}