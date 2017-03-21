package org.bigraph.biggr.model

import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.simulator._
import org.bigraph.biggr.sorting._
import scala.collection.mutable.Map



class Model(
    val controls:List[Control],
    val nodeNameToControl:Map[String,Control],
    val reactionRules:List[ReactionRule],
    val beginningState:Bigraph,
    val sortings:List[Sorting]) {
  
  

}