package org.bigraph.bigsim.specification


import java.io.File
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.collection.mutable.Set

import rwth.i2.ltl2ba4j.model.ITransition
import rwth.i2.ltl2ba4j.model.IGraphProposition
import org.bigraph.bigsim.model.Specification;
import org.bigraph.bigsim.model.BigraphPair;
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.parser.BGMTerm;
import org.bigraph.bigsim.parser.BGMParser;
import org.bigraph.bigsim.Verify


/**
 * @author amy
 */
object GenBigraph {
  var initBi: Bigraph = getInit();
  var trueBi: Bigraph = getTrueBigraph();
  
  var spec: Specification = Specification.processSpec();
  //result of the ltl2ba， list of ITransition
  var automaton = GenTS.genTS(); 
  //由automaton转换后的所有Bigraph pair
  var bigraphPairList: ListBuffer[BigraphPair] = new ListBuffer[BigraphPair]();
  
  def main(args: Array[String]): Unit = {
    //getBigraphPair();
    getAllBigraph();
    //getNextBigraph(b);
    println(isNegative("fffa"));
  }
   
  /**
   * 获取BA中每个state对应的Bigraph
   * 将label内容赋予其targetState
   */
  
  def getstateBigraph(): Map[String, Bigraph]  = {
    var res: Map[String, Bigraph] = Map();
    automaton.foreach { x => 
      var labelSet: scala.collection.mutable.Set[IGraphProposition] = x.getLabels;
      if(labelSet.size == 1) {
        //如果label不是sigma
        if (!isSIGMA(labelSet)) {
          if(x.getSourceState.isInitial()) {
            var sourceStr = x.getSourceState.getLabel;
            res += (sourceStr -> initBi);
            var b: Bigraph = getBigraph(getLabel(labelSet));
            b.label = getLabel(labelSet);
            if (isNegative(b.label)) {
              b.isNegative = true;
            }
            var targetStr = x.getTargetState.getLabel;
            res += (targetStr -> b);
          } else {
            var b: Bigraph = getBigraph(getLabel(labelSet));
            var str = x.getTargetState.getLabel;
            b.label = getLabel(labelSet);
            if (isNegative(b.label)) {
              b.isNegative = true;
            }
            res += (str -> b);
          }
        } 
      }
     }
    return res;
  }
  
  /**
   * 获得Bigraph迁移关系
   * 以迁移对的方式返回
   */
  def getBigraphPair(): Unit = {
    bigraphPairList.clear();
    var stateBigraph: Map[String, Bigraph] = getstateBigraph();
    var bigraphPair: BigraphPair = new BigraphPair();
    automaton.foreach { x => 
      var labelSet: scala.collection.mutable.Set[IGraphProposition] = x.getLabels;
      if(labelSet.size == 1) {
        if (isSIGMA(labelSet)) { //标签为siegma
          if (x.getSourceState.isFinal()) { //状态为终止状态
            var bigraphPair1: BigraphPair = new BigraphPair(); 
            bigraphPair1.init(trueBi, trueBi);
            bigraphPairList.add(bigraphPair1);
          }else if(x.getSourceState.isInitial()) { //状态为起始状态
            var bigraphPair2: BigraphPair = new BigraphPair(); 
            bigraphPair2.init(initBi, initBi);
            bigraphPairList.add(bigraphPair2);
          }  
        }else { //label为普通命题
          var bigraphPair3: BigraphPair = new BigraphPair(); 
          var source = stateBigraph(x.getSourceState.getLabel);
          var target = stateBigraph(x.getTargetState.getLabel);
          bigraphPair3.init(source, target);
          bigraphPairList.add(bigraphPair3);
        }
      }
     }
  }
  
  
  /**
   * 根据输入的Bigraph获取所有它的下一状态
   */
  
  def getNextBigraph(b: Bigraph): Set[Bigraph] = {
    getBigraphPair();
    var res: Set[Bigraph] = Set();
    bigraphPairList.foreach { x => 
      if(x.sourceBigraph == b) {
        res.add(x.targetBigraph);
      }
     }
    return res;
  }
  
  /**
   * 根据所有的Bigraph
   */
  
  def getAllBigraph():Set[Bigraph] = {
    getBigraphPair();
    var res: Set[Bigraph] = Set();
    bigraphPairList.foreach { x => 
      res.add(x.sourceBigraph);
      res.add(x.targetBigraph); 
     }
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
  
   /**
  * 获取labelSet内容
  */
  private def getLabel(labelSet: scala.collection.mutable.Set[IGraphProposition]): String = {
    var res: String = "";
    labelSet.foreach { x => 
      res = x.getLabel;
    }
    return res;
  }
  /**
   * 判断label内容是否为否定形式
   */
   private def isNegative(str: String): Boolean = {
    var res: Boolean = false;
    if (str.startsWith("!")) {
      res = true;
    }
    return res;
  }
  
   /**
   *  命题对应的Bigraph
   */
   def getBigraph(str: String): Bigraph = {
    var pair: Map[String, Bigraph] = spec.proposition;
    var b: Bigraph = pair(str);
    return b;
  }
  
  /**
   * 获取初始Bigraph
   */
  private def getInit(): Bigraph = {
    var init: Bigraph = new Bigraph(1);
    init.isInitial = true;
    init.label = "init";
    return init;
  }
  
  /**
   * 获取最终自循环的Bigraph
   */
  def getTrueBigraph(): Bigraph = {
    var trueB: Bigraph = new Bigraph(1);
    trueB.isFinal = true;
    trueB.label = "true";
    return trueB;
  }
}