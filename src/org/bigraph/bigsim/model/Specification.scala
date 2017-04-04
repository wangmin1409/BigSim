package org.bigraph.bigsim.model

import org.bigraph.bigsim.parser.BGMTerm
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.parser.BGMParser
import java.io.File

/**
 * @author amy
 */
object Specification {
  
  def main(args: Array[String]): Unit = {
    println(processSpec());
  }
  def processSpec(): Specification = {
    var t:List[BGMTerm] = null;
    // parse the BGM input file
    try{
      if (GlobalCfg.IsFromNetWork) {
      t = BGMParser.parseFromString(GlobalCfg.bgmContent);
      } else {
      t = BGMParser.parse(new File(GlobalCfg.filename));
      }
    } catch {
       case t: Throwable => {t.printStackTrace();} // TODO: handle error
    }
    var spec: Specification = BGMTerm.toSpecification(t) 
    
    return spec;
  }
}

class Specification(){
  
  var formula: String = "";  //LTL公式
  var proposition: Map[String, Bigraph] = Map(); //公式中命题对应的Bigraph
  
  override def toString = {
    val s: StringBuffer = new StringBuffer();
    s.append("Specfication:\n");
    s.append("formula:" + formula + "\n"); 
    proposition.keySet.foreach { x => 
      s.append("\tProName:" + x);
      s.append("\tProContent:" +  proposition(x));
    }
    s.toString();
  }
}