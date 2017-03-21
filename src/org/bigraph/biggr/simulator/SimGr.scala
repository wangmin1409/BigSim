package org.bigraph.biggr.simulator

import org.bigraph.biggr.model._
import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.simulator._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.LinkedList

class SimGr(model:Model) {
	val lts:LTS = new LTS()
	val naming = new NamingNodes(model)
	val s1 = new State(model.beginningState)
	s1.index = 0
	//lts�ĵ�һ��״̬��ʼ��Ϊagent����״̬����Ϊ0
	lts.stateSequence.append(s1)
	
	//��һ�ε��ò���ʱ������0����ʾ��agent��ʼ
	def GetMatches(stateIdx:Int):Map[ReactionRule,Array[NLMapping]] = {
	  val m = Map[ReactionRule,Array[NLMapping]]()
	  model.reactionRules.foreach(r=>{
	    val matcher = new Matcher()
	    val state = lts.stateSequence(stateIdx)
	    val ss = matcher.topDown_SearchAll_MultiRoot(state.bigraph,r.redex)
	    if(ss!=null && !ss.isEmpty)
	    {
		    println("THIS: " + state)//Ϊ��ʡʱ��ע�͵�
		    println("RULE: " + r)//Ϊ��ʡʱ��ע�͵�
		    m(r) = ss.toArray
	    }
	  })
	  m
	}
	
	/*********************************
	 * 	������ɡ�
	 *  stateIdx��ʾ��״̬������
	 *  choices�Ǹ�״̬�����еĹ����Լ��ù����¿��õ�ƥ��
	 *  ��Ҫʱ�ɵ���ReactionRule���name,expression����(index�ݲ�����)
	 *  ����֪��NLMapping������ʵ�֡�	
	*********************************/
	def ChooseMatch(stateIdx:Int,choices:Map[ReactionRule,Array[NLMapping]]):(Int,ReactionRule,NLMapping) = {
	  if(choices.isEmpty)
	    return null
	  //...
	  //...
	  (stateIdx,choices.toList(0)._1,choices.toList(0)._2(0))
	}
	
	//��һ��ƥ�䣬���з�Ӧ�������һ��״̬����״̬�ʹ�Ǩ�Ƽ���lts�У�������һ��״̬������
	def Reaction(stateIdx:Int,rule:ReactionRule, mapping:NLMapping):Int = {
	  val state = lts.stateSequence(stateIdx)
	  val state_copy = new State(state)
      val reaction = new Reaction(state_copy,rule,mapping,naming)
      val next = reaction.next_state()
      lts.stateSequence.append(next)
      next.index = lts.stateSequence.length-1
      lts.labels+=new Transition(stateIdx,next.index,rule)
	  next.index
	}
	
	override def toString:String = lts.toGraphviz
	
	def simulate():Unit = {
	  var idx=0
	  var matches = GetMatches(idx)
	  while(!matches.isEmpty)
	  {
	    val nextStateIdx = Reaction(ChooseMatch(idx,matches)._1,
	        ChooseMatch(idx,matches)._2,ChooseMatch(idx,matches)._3)
	    matches = GetMatches(nextStateIdx)
	    idx = nextStateIdx
	  }

	}
 }