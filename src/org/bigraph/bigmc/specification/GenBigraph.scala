package org.bigraph.bigmc.specification

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import scala.collection.mutable.Set

import rwth.i2.ltl2ba4j.model.ITransition
import rwth.i2.ltl2ba4j.model.IGraphProposition
import org.bigraph.bigmc.model.BigraphPair;
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
  
  //result of the ltl2ba， list of ITransition
  var automaton = GenTS.genTS(); 
  //由automaton转换后的所有Bigraph pair
  var bigraphPairList: ListBuffer[BigraphPair] = new ListBuffer[BigraphPair]();
  
  def main(args: Array[String]): Unit = {
    getBigraphPair();
    var b = getInit();
    //getNextBigraph(b);
  }
  
  
  
  
  
  /**
   * 从bgm文件获取formula和proposition
   */
  def getFormula(): String = {
    val fileName: String = "Examples/111/models/test20170323.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    return BGMTerm.parseFormula(p);
  }
  
  
  def getProposition: Map[String,Bigraph] = {
    val fileName: String = "Examples/111/models/test20170323.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    return BGMTerm.parseProposition(p);
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
        if (!isSIGMA(labelSet)) {
          if(x.getSourceState.isInitial()) {
            var sourceStr = x.getSourceState.getLabel;
            res += (sourceStr -> initBi);
            var b: Bigraph = getBigraph(getLabel(labelSet));
            var targetStr = x.getTargetState.getLabel;
            res += (targetStr -> b);
          } else {
            var b: Bigraph = getBigraph(getLabel(labelSet));
            var str = x.getTargetState.getLabel;
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
            bigraphPair.init(trueBi, trueBi);
            bigraphPairList.append(bigraphPair);
          }else if(x.getSourceState.isInitial()) { //状态为起始状态
            bigraphPair.init(initBi, initBi);
            bigraphPairList.append(bigraphPair);
          }  
        }else { //label为普通命题
          var source = stateBigraph(x.getSourceState.getLabel);
          var target = stateBigraph(x.getTargetState.getLabel);
          bigraphPair.init(source, target);
          bigraphPairList.append(bigraphPair);
        }
      }
     }
    println(bigraphPairList)
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
    println(res.size())
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
    println(res.size())
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
    val fileName: String = "Examples/111/models/test20170323.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    var pair: Map[String, Bigraph] = BGMTerm.parseProposition(p);
    var b: Bigraph = pair(str);
    return b;
  }
  
  /**
   * 获取初始Bigraph
   */
  def getInit(): Bigraph = {
    var init: Bigraph = new Bigraph(1);
    init.isInitial = true;
    init.lable = "init";
    return init;
  }
  
  /**
   * 获取最终自循环的Bigraph
   */
  def getTrueBigraph(): Bigraph = {
    var trueB: Bigraph = new Bigraph(1);
    trueB.isFinal = true;
    trueB.lable = "true";
    return trueB;
  }
}