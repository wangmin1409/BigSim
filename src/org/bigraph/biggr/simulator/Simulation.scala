package org.bigraph.biggr.simulator

import org.bigraph.biggr.model._
import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.simulator._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.LinkedList


class State{
  var index:Int = -1
  var bigraph:Bigraph = null
  
  
  def this(bigraph:Bigraph)
  {
    this()
    this.bigraph = bigraph
    
  }
  def this(state:State){
    this()
    this.bigraph=new Bigraph(state.bigraph)
  }
  override def toString():String ={
    bigraph.toString()
  }
  def isSame(state:State):Boolean = {
    val matcher = new Matcher()
    val s1 = matcher.topDown_SearchOne_MultiRoot(this.bigraph,state.bigraph)
    if(s1==null)
      return false   
    val s2 = matcher.topDown_SearchOne_MultiRoot(state.bigraph,this.bigraph)
    if(s2==null)
      return false 
    true
  }
  def isIdentical(state:State):Boolean = {
    this.bigraph.isIdentical(state.bigraph)
  }
  def inStates(states:List[State]):Int = {
    states.foreach(s=>{
      if(this.isIdentical(s))
      //if(this.isSame(s))
        return s.index
    })
    -1
  }
}

class NamingNodes(model:Model){
  var NumOfEachControl:Map[Control,Int] = Map()
  model.beginningState.nodeNameToNode.values.toList.foreach(n=>
      if(!NumOfEachControl.contains(n.control) || NumOfEachControl(n.control)<n.id)
      {
        NumOfEachControl(n.control)=n.id
      }
        )
  model.controls.foreach(c=>{
    if(!NumOfEachControl.contains(c))
      NumOfEachControl(c)=0
  })
}

class Simulation(model:Model) {
  //val stateSequence:List[State] = null
  var timeOfSorting:Long = 0
  val beginningState:State = new State(model.beginningState)
  val naming = new NamingNodes(model)
  
  def generateLTS():LTS = {
    val lts = new LTS()
    var idxState = 0
    lts.stateSequence.append(new State(beginningState))
    //var cur = lts.stateSequence
    lts.stateSequence(0).index = idxState
    //cur.elem.index = idxState
    idxState += 1
    var ij = 0
    while(ij<lts.stateSequence.length)
    {
      val sss = lts.stateSequence(ij)
      model.reactionRules.foreach(r=>{
        var after = LinkedList[State]()
        val next_states = matchingOneRuleToNextState(sss,r)
        next_states.foreach(s=>{
          var i = s.inStates(lts.stateSequence.toList)
          if(i== -1)
          {
            after=s+:after
            s.index = idxState
            idxState += 1
            i = s.index 
          }
          lts.labels+=new Transition(sss.index,i,r)
        })
        after.foreach(af=>lts.stateSequence.append(af))
      })
      ij+=1
    }
    
    
    println(lts.toGraphviz)//Ϊ��ʡʱ��ע�͵�
    println("״̬��"+lts.stateSequence.size+"  Ǩ����"+lts.labels.size)
    lts
  }
  
  //����һ�������ҳ����пɴ����һ״̬
  def matchingOneRuleToNextState(state:State,rule:ReactionRule):List[State] = {
    val ret = ListBuffer[State]()
    val graph = state.bigraph
    if(graph == null)
    {
      println("Invalid Input")
      return null
    }
    val matcher = new Matcher()
    val ss = matcher.topDown_SearchAll_MultiRoot(graph,rule.redex)
    if(ss==null)
      return null
    if(!ss.isEmpty)
    {
      println("THIS: " + state)//Ϊ��ʡʱ��ע�͵�
      println("RULE: " + rule)//Ϊ��ʡʱ��ע�͵�
    }
    ss.foreach(s=>{
      ///
      
      ///
      val state_copy = new State(state)
      val reaction = new Reaction(state_copy,rule,s,this.naming)
      val next = reaction.next_state()
      
       val endTime1=System.currentTimeMillis();
      
      if(model.sortings==null ||( model.sortings.head.checkAll(next.bigraph.nodeNameToNode.values.toArray)
        && model.sortings(1).checkAll(next.bigraph.nodeNameToNode.values.toArray)
        && model.sortings(2).checkAll(next.bigraph.nodeNameToNode.values.toArray)
        ))
      {ret.append(next)
       println("NEXT: "+next)
        //println("RIGHT")
      }
       val endTime2=System.currentTimeMillis();
       this.timeOfSorting  += endTime2-endTime1
      //else
        //println("WRONG")
    })
    if(!ss.isEmpty)
      println("-----")
    ret.toList
  }
 
 

}