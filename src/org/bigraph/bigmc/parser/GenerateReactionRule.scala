package org.bigraph.bigmc.parser 

/**
 * @author amy
 */
object GenerateReactionRule {
  def main(args: Array[String]): Unit = { 
    val lTLtoBigraph = new LTLtoBigraph()
    lTLtoBigraph.entry()
  }
}