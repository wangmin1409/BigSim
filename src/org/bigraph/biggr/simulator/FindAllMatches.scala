package org.bigraph.biggr.simulator

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.biggr.bigraph.IdleEdge
import org.bigraph.biggr.bigraph.OuterName
import org.bigraph.biggr.bigraph.Edge
import org.bigraph.biggr.bigraph.Place
import org.bigraph.biggr.bigraph.Link
import org.bigraph.biggr.bigraph.Control
import org.bigraph.biggr.bigraph.Node
import org.bigraph.biggr.bigraph.Root
import org.bigraph.biggr.utils.Algorithm


class  FindAllMatches {
  var alreadyMatchedNodes = Set[Node]()

  /* �ҵ�����ƥ��Ϊֹ
   * regionsOfRedex����ÿ�δ���ȡһ�����㣬�ݹ��������
   * regionsOfAgent���ڼ���Agent�Ķ��ˣ�������ʧ��ʱ��Ҫ�Ӵ˴�/��ͷ����
   */
  def multiRegionsOfRedexMatch_All(
      regionsOfRedex:List[Place],
      regionsOfAgent:List[Place],
      B:List[Place]):ArrayBuffer[NLMapping] = {
    if(regionsOfRedex==null || regionsOfRedex.size==0)
    {
      return ArrayBuffer[NLMapping]()
    }
    val regionOfRedex = regionsOfRedex.head
    val tmp_alreadyMatchedNodes = Set()++this.alreadyMatchedNodes
    val resddd = ArrayBuffer[NLMapping]()
    for(b<-B)
    {
      //println("TRY: "+b)
      val res = isSubOf(regionOfRedex,b,Map[Link,Link]())
      for(one<-res)
      {
        for((key,value)<-one.matchedNodes)
        {
           this.alreadyMatchedNodes+=value
        }
        val remains = multiRegionsOfRedexMatch_All(
            regionsOfRedex.drop(1),
            regionsOfAgent,
            regionsOfAgent)

       
        //remains��one�ĵѿ����
        resddd++=Algorithm.dikaer(ArrayBuffer(one), remains)
        
        this.alreadyMatchedNodes = tmp_alreadyMatchedNodes        
      }
      if(b.children != null)
      {
          resddd++= multiRegionsOfRedexMatch_All(
              regionsOfRedex,
              regionsOfAgent,
              b.children.values.toList.flatten)
            
      }
    }
    resddd
  }
 
  /* ������ matchedLinks�������£����ж�
   * �жϽ��A�������Ƿ��ǽ��B��������Ӽ�������ǣ���ƥ�䵽��B�Ľڵ����matchedNodes
   */
  def isSubOf(A:Place,B:Place,cond_matchedLinks:Map[Link,Link]):ArrayBuffer[NLMapping] = {
    val cond_Mappings = ArrayBuffer[NLMapping]()
    if(A.children==null || A.children.isEmpty)
    {
      val one_result = new NLMapping()
      one_result.matchedLinks++=cond_matchedLinks
      cond_Mappings.append(one_result)
      return cond_Mappings 
    }
    else if(B.children==null|| B.children.isEmpty)
      return cond_Mappings
    //����A�ĺ��ӵ�control������A��ÿ��control�ĺ��ӣ�������controlΪ�淽Ϊ�棩
    CheckSubForRemainingControl(Map()++A.children,B.children,cond_matchedLinks)
  }
  
  //������matchedNodes��matchedLinks������[����mapping]
  //��ʣ���control�Ƿ�����Sub������ϵ
  //ֻ����һ��Control,Ȼ���A_children��ȥ���Control���ݹ���ñ�����
  //������һ��Control�Ĺ���У����tmp�����ɹ�ƥ�䣬�Ͱ�tmp��������mapping��
  def CheckSubForRemainingControl(A_children:Map[Control,ArrayBuffer[Node]],
      B_children:Map[Control,ArrayBuffer[Node]],
      cond_matchedLinks:Map[Link,Link]):ArrayBuffer[NLMapping] = {
    //val cond_Mappings = ArrayBuffer[NLMapping]()
    val resddd = ArrayBuffer[NLMapping]()
    if(A_children.isEmpty)
    {
      val one_result = new NLMapping()
      one_result.matchedLinks++=cond_matchedLinks
      resddd.append(one_result)
      return resddd 
    }
    val (control,nodesA)=A_children.head   
    if(!B_children.contains(control))
      return resddd
    //var b:Boolean = false//�ҵ�һ�����о�����  
    val nodesBB = B_children(control).filterNot(n=>alreadyMatchedNodes.contains(n))
    if(nodesA.length>nodesBB.length)
      return resddd
    if(nodesA.head.parent.sites.isEmpty && !nodesA.head.parent.isInstanceOf[Root])
    {
      if(nodesA.length!=nodesBB.length)
        return resddd
    }
    //�ҵ�B����Ӧcontrol�ĺ��ӣ�����ȳ��ȵ��Ӽ���������controlƥ�䲻�ϣ�����false
    for(nodesB<-nodesBB.toSet.subsets(nodesA.length))
    {
      //println("TRY: "+nodesB)
      val permu = Range(0,nodesA.length).toArray
      do{
        var tmp = Map()++cond_matchedLinks//��������Ϊ����checkMatchedLinks�ĵ��ûὫ��ı�
        val A = nodesA.toArray
        val B = nodesB.toArray 
        val afterCheckLinks = checkMatchedLinks(A,B,tmp,permu)
        if(afterCheckLinks!=null)//���Ϊ�棬�ҸĶ���tmp
        {
          val A_remainingChildren:Map[Control,ArrayBuffer[Node]]=A_children-control
          val remains = CheckSubForRemainingControl(A_remainingChildren,B_children,tmp)
          if(remains.size!=0)
          {
            for(one<-afterCheckLinks)
            {
              permu.foreach(i => one.matchedNodes+=(A(i)->B(permu(i))) )
              one.matchedLinks ++= tmp
            }
            //��afterCheckLinks��remains�ĵѿ����
            resddd ++= Algorithm.dikaer(afterCheckLinks, remains)
          }
        }//����cond_matchedLinksά��ԭò
      }while(Algorithm.next_permutation(permu))
    }
    resddd//���������control�������Ӽ������ж������˻�Ϊ�٣��������ؼ�
  }

  def CheckSub_subRoutine(){
    
  }

  def checkMatchedLinks(A:Array[Node],B:Array[Node],cond_matchedLinks:Map[Link,Link],permu:Array[Int]):ArrayBuffer[NLMapping] = {
    //����A��B�ĳ�����ͬ��Ԫ��controlҲ��ͬ��ȷ������������Ԫ�ص�����ӳ�䲻��ͻ��ȷ������������Ԫ�ص��������Ӽ�
    var ress = ArrayBuffer[NLMapping]()
    for(i<-Range(0,A.length))
    {
      //ȷ������Ԫ�ص�����ӳ�䲻��ͻ��ȷ������Ԫ�ص��������Ӽ�
      val res = checkMatchedLinksForNode(A(i),B(permu(i)),cond_matchedLinks)
      if(res==null)
         return null
      else
         ress = Algorithm.dikaer(ress, res)
    }
    //��ress�ĵѿ����
    //{1,3,4}{H}{r,t}
    //����(1Hr,1Ht,3Hr,3Ht,4Hr,4Ht)
    ress
  }
  //AB���ǽ�㣬������ǵ�link����ͻ��������ͻ����Ҫ��A������B������Ӽ������ܷ����档
  //
  def checkMatchedLinksForNode(A:Node,B:Node,cond_matchedLinks:Map[Link,Link]):ArrayBuffer[NLMapping] = {
    val map:Map[Link,Link] = Map()
    if(A.ports.length!=B.ports.length)
      return null
    for(i<-0 until A.ports.length)
    {
      val aLink = A.ports(i).link
      val bLink = B.ports(i).link
      aLink match{
        case aLink:Edge => 
          if(!bLink.isInstanceOf[Edge])
            return null
          if(aLink.points.size>bLink.points.size)
            return null
        case aLink:OuterName =>
          if(aLink.points.size>bLink.points.size)
            return null
        case aLink:IdleEdge =>
          if(!bLink.isInstanceOf[IdleEdge])
            return null
      }
      if(cond_matchedLinks.contains(aLink))
      {
        if(cond_matchedLinks(aLink)!=bLink)
          return null
      }
      else
      {
        map(aLink)=bLink
      }
    }
    cond_matchedLinks  ++= map
    val res = isSubOf(A,B,cond_matchedLinks)
    if(res.isEmpty)
      return null
    else
        res
  }
}
