package org.bigraph.biggr.simulator

import org.bigraph.biggr.model._
import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.utils._
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer

class NLMapping{
  val matchedNodes:Map[Node,Node] = Map()
  val matchedLinks:Map[Link,Link] = Map()
  def this(other:NLMapping){
    this()
    this.matchedLinks ++= other.matchedLinks
    this.matchedNodes ++= other.matchedNodes
  }
  def add(other:NLMapping){
    this.matchedLinks ++= other.matchedLinks
    this.matchedNodes ++= other.matchedNodes
  }
  override def toString():String = {
    matchedNodes.toString + matchedLinks.toString
  }
}




/*Match�ฺ��ƥ��
 *�������������ԣ��Զ�����/�Ե׶���
 * */
class Matcher() {

   /* ���ڵ�region
    * �ҳ�����match
    *  
   def topDown_SearchAll_OneRoot(agent:Bigraph,redex:Bigraph):ArrayBuffer[NLMapping]={
    val m_roots = redex.regions
    val roots = agent.regions
    val nl_mapping:NLMapping = new NLMapping()
    //redex��㵽matched����ӳ��matchedNodes,�ڵ���matchOneRegionOfRedex����г�ʼ�����
    //redex���ӵ�matched���ӵ�ӳ��matchedLinks,�ڵ���matchOneRegionOfRedex����г�ʼ����
    //�ڵ���decomposeʱ��Ҫ���£���Ϊ�������ӱ��滻�������֣�
    //����redex����Ӧ����ӳ�䵽matched����Ӧ������
    //matchedNodes��matchedLinks��decompose��compose�ж���������
    val nl_mappings = ArrayBuffer[NLMapping]()
    val matchOneRule = new MatchOneRule()
    var b:Boolean = false
    for(i<-Range(0,m_roots.length))
    {
      matchOneRule.aRegionOfRedexMatch_All(m_roots(i), roots.toList,nl_mapping,nl_mappings)
    }
    println("all mappings:")
    for(a<-nl_mappings)
     println("** "+a)
    nl_mappings
   }*/
   /* ���ڶ�region
    * �ҳ�����match
    * */
   def topDown_SearchAll_MultiRoot(agent:Bigraph,redex:Bigraph):ArrayBuffer[NLMapping]={
      val m_roots = redex.regions
      val roots = agent.regions
      val matchOneRule = new FindAllMatches()
      val nl_mappings = matchOneRule.multiRegionsOfRedexMatch_All(m_roots.toList, roots.toList,roots.toList)
      //println("AGENT: "+agent)
      //println("REDEX: "+redex)
      //println("all mappings:")
      //for(a<-nl_mappings)
         //println("** "+a)
      nl_mappings
   }
   /* ���ڶ�region
    * �ҵ�һ��matchΪֹ
    * �ݹ�汾
    */
   def topDown_SearchOne_MultiRoot(agent:Bigraph,redex:Bigraph):NLMapping={
    val m_roots = redex.regions
    val roots = agent.regions
    val matchOneRule = new FindOneMatch()
    //matchOneRule.isForSame = true//��ѧ��������Ҫ��Ϊtrue,��ݿ������Ӳ���Ҫ
    val b = matchOneRule.multiRegionsOfRedexMatch_One(
        m_roots.toList,roots.toList,roots.toList)    
    if(b)
    {
      matchOneRule.nl_mapping_multi_roots
    }
    else
    {
      //println("No Match")
      null
    }
   }
   
  
}
