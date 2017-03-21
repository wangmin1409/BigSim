package org.bigraph.biggr.simulator

import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.model._


class Reaction(val ori_state:State,val rule:ReactionRule,val ori_nl_mapping:NLMapping,val naming:NamingNodes) {
  //����һ��ƥ��ӳ������decompose��buildcopy��compose���裬������һ��״̬
  val nl_mapping:NLMapping = new NLMapping()
  val state:State = state_copy()
  
  
  def state_copy():State={
    val state = new State(ori_state)
    val bigraph = state.bigraph
    ori_nl_mapping.matchedNodes.foreach(n=>{
      nl_mapping.matchedNodes+= n._1->bigraph.nodeNameToNode(n._2.name)
    })
    ori_nl_mapping.matchedLinks.foreach(l=>{
      nl_mapping.matchedLinks+= l._1->bigraph.linknameToLink(l._2.name)
    })
    state
  }
  
  def next_state():State ={
    
    val dec = new Decompose(nl_mapping,rule.redex.regions.length,rule.redex.sites.length)
    //decompose��ɺ�,�õ���matched���ֿ��������ˣ���Ҫ��ֻ��Components�е�context��parameter
    dec.decompose(state.bigraph,rule.redex)
    //println(dec.toString())
    //ruleֻҪ����mapping�Ϳ������turnedMatched
    val turnedMatched = rule.buildCopyOfReactum(nl_mapping,naming)
    //println(turnedMatched.toString())
    val comp = new Compose(dec.context,turnedMatched,dec.parameter)
    val after = comp.compose()
    new State(after)
  }

}