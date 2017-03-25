package org.bigraph.bigmc.model

import org.bigraph.bigsim.model.Bigraph;
/**
 * @author amy
 */
object BigraphPair {
  
}

class BigraphPair {
   var sourceBigraph: Bigraph = null;
   var targetBigraph: Bigraph = null;
  
  def this(surceBigraph: Bigraph, targetBigraph: Bigraph) = {
    this();
    init(surceBigraph, targetBigraph);
  }
  def init(sourceBigraph: Bigraph, targetBigraph: Bigraph): Unit = {
    this.sourceBigraph = sourceBigraph;
    this.targetBigraph = targetBigraph;
  }
  
  override def toString = "sourceBigraph：" + sourceBigraph + "--->" + "\n" + "targetBigraph：" + targetBigraph + ";";
  
}
