package org.bigraph.bigmc.specification

import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.parser.BGMTerm
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.parser.BGMParser
import org.bigraph.bigsim.model.Specification;
import java.io.File


/**
 * @author amy
 */
object ParserSpec {
  
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
  
   def main(args: Array[String]): Unit = {
     processSpec();
   }
 
  
}


