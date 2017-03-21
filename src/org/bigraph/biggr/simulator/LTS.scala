package org.bigraph.biggr.simulator

import org.bigraph.biggr.model._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedList

class Transition(val startState:Int,
    val endState:Int,
    val rule:ReactionRule){
}
class LTS {
  var stateSequence:ArrayBuffer[State]  = ArrayBuffer()
  val labels:ListBuffer[Transition] = ListBuffer()
  def toGraphviz:String = {
    var str = ""
    this.labels.foreach(l=>{
      str += "N_"+l.startState
      str += "->"
      str += "N_"+l.endState
      str += " [label=\"" + l.rule.name +"\"];\n"
    })
    this.stateSequence.foreach(s=>{
      str += "N_"+s.index+": "+s+"\n"
    })
    str
  }

}