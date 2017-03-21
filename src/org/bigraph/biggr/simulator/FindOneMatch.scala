package org.bigraph.biggr.simulator

import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.utils._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.biggr.utils.Algorithm


class  FindOneMatch {
  var alreadyMatchedNodes = Set[Node]()
  //������ĵط�
  var isForSame:Boolean = false
  var nl_mapping_multi_roots = new NLMapping()

 
  /* �ҵ�һ����region��ƥ��Ϊֹ
   * regionsOfRedex����ÿ�δ���ȡһ�����㣬�ݹ��������
   * regionsOfAgent���ڼ���Agent�Ķ��ˣ�������ʧ��ʱ��Ҫ�Ӵ˴�/��ͷ����
   */
  def multiRegionsOfRedexMatch_One(
      regionsOfRedex:List[Place],
      regionsOfAgent:List[Place],
      B:List[Place]):Boolean = {
    if(regionsOfRedex==null || regionsOfRedex.size==0)
      return true
    val regionOfRedex = regionsOfRedex.head
    val tmp_nl_mapping:NLMapping = new NLMapping(this.nl_mapping_multi_roots) 
    val tmp_alreadyMatchedNodes = Set()++this.alreadyMatchedNodes
    for(b<-B)
    {
      //println("TRY: "+b)
      if(isSubOf(regionOfRedex,b,this.nl_mapping_multi_roots))
      {
        //this.alreadyMatchedNodes = Set[Node]()
        for((key,value)<-this.nl_mapping_multi_roots.matchedNodes)
        {
           this.alreadyMatchedNodes+=value
        }
        if(multiRegionsOfRedexMatch_One(
            regionsOfRedex.drop(1),
            regionsOfAgent,
            regionsOfAgent))
          //��ߵĲ�����B����Ϊ�Ǵ�ͷ��ʼ��
        {
          //��ʱһ����region��ƥ���ҵ��ˣ�
          return true
        }
        else
        {
          //����ȫ�ֱ���������һ��
          this.nl_mapping_multi_roots = tmp_nl_mapping
          this.alreadyMatchedNodes = tmp_alreadyMatchedNodes
        }
      }
      if(b.children != null)
      {
          if(multiRegionsOfRedexMatch_One(
              regionsOfRedex,
              regionsOfAgent,
              b.children.values.toList.flatten))
            return true
      }
    }
    false
  }
  /* ������matchedNodes��matchedLinks�������£����ж�
   * �жϽ��A�������Ƿ��ǽ��B��������Ӽ�������ǣ���ƥ�䵽��B�Ľڵ����matchedNodes
   */
  def isSubOf(A:Place,B:Place,cond_Mapping:NLMapping):Boolean = {
    if(A.children==null)
      return true
    else if(B.children==null)
      return false
    //����A�ĺ��ӵ�control������A��ÿ��control�ĺ��ӣ�������controlΪ�淽Ϊ�棩
    CheckSubForRemainingControl(Map()++A.children,B.children,cond_Mapping)
  }
  
  //������matchedNodes��matchedLinks������[����mapping]
  //��ʣ���control�Ƿ�����Sub������ϵ
  //ֻ����һ��Control,Ȼ���A_children��ȥ���Control���ݹ���ñ�����
  //������һ��Control�Ĺ���У����tmp�����ɹ�ƥ�䣬�Ͱ�tmp��������mapping��
  def CheckSubForRemainingControl(A_children:Map[Control,ArrayBuffer[Node]],
      B_children:Map[Control,ArrayBuffer[Node]],
      cond_Mapping:NLMapping):Boolean = {
    
    if(A_children.isEmpty)
      return true
    val (control,nodesA)=A_children.head   
    if(!B_children.contains(control))
      return false
    var b:Boolean = false//�ҵ�һ�����о�����  
    val nodesBB = B_children(control).filterNot(n=>alreadyMatchedNodes.contains(n))
    if(nodesA.length>nodesBB.length)
      return false
    //�ҵ�B����Ӧcontrol�ĺ��ӣ�����ȳ��ȵ��Ӽ���������controlƥ�䲻�ϣ�����false
    for(nodesB<-nodesBB.toSet.subsets(nodesA.length) if!b)
    {
      //println("TRY: "+nodesB)
      val permu = Range(0,nodesA.length).toArray
      do{
        var tmp:NLMapping=new NLMapping(cond_Mapping)//����
        val A = nodesA.toArray
        val B = nodesB.toArray 
        if(checkMatchedLinks(A,B,tmp,permu))
        {
          val A_remainingChildren:Map[Control,ArrayBuffer[Node]]=A_children-control
          if(CheckSubForRemainingControl(A_remainingChildren,B_children,tmp))
          {
            b=true  
            permu.foreach(i => cond_Mapping.matchedNodes+=(A(i)->B(permu(i))) )
            cond_Mapping.add(tmp)
          }
        }
      }while(!b && Algorithm.next_permutation(permu))
    }
    b//���������control�������Ӽ������ж������˻�Ϊ�٣��������ؼ�
  }


  def checkMatchedLinks(A:Array[Node],B:Array[Node],tmp:NLMapping,permu:Array[Int]):Boolean = {
    //����A��B�ĳ�����ͬ��Ԫ��controlҲ��ͬ��ȷ������������Ԫ�ص�����ӳ�䲻��ͻ��ȷ������������Ԫ�ص��������Ӽ�
    for(i<-Range(0,A.length))
    {
      //ȷ������Ԫ�ص�����ӳ�䲻��ͻ��ȷ������Ԫ�ص��������Ӽ�
      if(!checkMatchedLinksForNode(A(i),B(permu(i)),tmp))
        return false
    }
    true
  }
  def checkMatchedLinksForNode(A:Node,B:Node,tmp:NLMapping):Boolean = {
    if(isForSame)
    {
      if(!A.name.equals(B.name))
        return false
    }
    val map:Map[Link,Link] = Map()
    if(A.ports.length!=B.ports.length)
      return false
    for(i<-0 until A.ports.length)
    {
      val aLink = A.ports(i).link
      val bLink = B.ports(i).link
      aLink match{
        case aLink:Edge => 
          if(!bLink.isInstanceOf[Edge])
            return false
          if(aLink.points.size>bLink.points.size)
            return false
        case aLink:OuterName =>
          if(aLink.points.size>bLink.points.size)
            return false
      }
      if(tmp.matchedLinks.contains(aLink))
      {
        if(tmp.matchedLinks(aLink)!=bLink)
          return false
      }
      else
      {
        map(aLink)=bLink
      }
    }
    tmp.matchedLinks ++= map
    isSubOf(A,B,tmp)
  }
}
