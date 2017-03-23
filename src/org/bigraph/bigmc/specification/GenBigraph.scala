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
  
  var b: Bigraph = null;
  var proBigMap: Map[String, Bigraph] = Map("a" -> b);
  
  def main(args: Array[String]): Unit = {
    getBigraphPair();
  }
  
  /**
   * 获取BA中每个state对应的Bigraph
   * 将label内容赋予其targetState
   */
  
  def getstateBigraph(): Map[String, Bigraph]  = {
    var res: Map[String, Bigraph] = Map();
    var automaton = GenTS.genTS(); 
    automaton.foreach { x => 
      var labelSet: scala.collection.mutable.Set[IGraphProposition] = x.getLabels;
      if(labelSet.size == 1) {
        if (!isSIGMA(labelSet)) {
          var b: Bigraph = getBigraph(getLabel(labelSet));
          var str = x.getTargetState.getLabel;
          println(str);
          res += (str -> b);
        }
      }
     }
    println(res.keys);
    println(res.values);
    return res;
  }
  
  /**
   * 获得Bigraph迁移关系
   * 以迁移对的方式返回
   */
  def getBigraphPair(): List[BigraphPair] = {
    var automaton = GenTS.genTS(); 
    var stateBigraph: Map[String, Bigraph] = getstateBigraph();
    var bigraphPairList: List[BigraphPair] = List()
    var bigraphPair: BigraphPair = null;
    var trueB: Bigraph = null;
    var init: Bigraph = null;
    automaton.foreach { x => 
      var labelSet: scala.collection.mutable.Set[IGraphProposition] = x.getLabels;
      println(labelSet.size)
      if(labelSet.size == 1) {
        if (isSIGMA(labelSet)) { //标签为siegma
          println("label是siegma");
          if (x.getSourceState.isFinal()) { //状态为终止状态
            trueB.isFinal = true;
            bigraphPair.init(trueB, trueB); 
            bigraphPairList.add(bigraphPair);
          }else if(x.getSourceState.isInitial()) { //状态为起始状态
            init.isInitial = true;
            bigraphPair.init(init, init);
            bigraphPairList.add(bigraphPair);
          } else { //label为siegma，但是前后非起始非终止状态
            bigraphPair.init(init, init);
            bigraphPairList.add(bigraphPair);
          }
        }else { //label为普通命题
          var source = stateBigraph(x.getSourceState.getLabel);
          var target = stateBigraph(x.getTargetState.getLabel);
          bigraphPair.init(source, target);
          bigraphPairList.add(bigraphPair);
        }
      }
     }
    return bigraphPairList;
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
  
  
  /**
   *  获取label标签内容 
   */
  def getLabel(labelSet: scala.collection.mutable.Set[IGraphProposition]): String = {
    var res: String = "";
    labelSet.foreach { x => 
      res = x.getLabel;
    }
    return res;
  }
  
   /**
   *  命题对应的Bigraph
   */
  def getBigraph(str: String): Bigraph = {
    //var b: Bigraph = null;
    var b: Bigraph = proBigMap(str);
    return b;
  }
 
}