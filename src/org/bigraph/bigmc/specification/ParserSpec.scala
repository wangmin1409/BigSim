package org.bigraph.bigmc.specification

import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.parser.BGMTerm
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.parser.BGMParser
import java.io.File


/**
 * @author amy
 */
object ParserSpec {
  
  def processSpec(): Spec = {
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
    var str: String = BGMTerm.parseFormula(t);
    var pro: Map[String, Bigraph] = BGMTerm.parseProposition(t);
    var spec: Spec = new Spec(str, pro);
    
    return spec;
  }
  
   /**
   * 从bgm文件获取formula和proposition
   */
  def getFormula(): String = {
    val fileName: String = "Examples/111/models/test20170323.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    return BGMTerm.parseFormula(p);
  }
  
  /**
   * 从bgm文件获取proposition
   */
  def getProposition: Map[String,Bigraph] = {
    val fileName: String = "Examples/111/models/test20170323.bgm";
    val p: List[BGMTerm] = BGMParser.parse(new File(fileName));
    return BGMTerm.parseProposition(p);
  }
  
}

class Spec(f: String, pro: Map[String, Bigraph]){
  
  var formula: String = f;
  var proposition: Map[String, Bigraph] = pro;
  
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


