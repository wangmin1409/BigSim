package org.bigraph.biggr.simulator

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.biggr.bigraph.Root
import org.bigraph.biggr.bigraph.OuterName
import org.bigraph.biggr.bigraph.Site
import org.bigraph.biggr.bigraph.Edge
import org.bigraph.biggr.bigraph.Control
import org.bigraph.biggr.bigraph.Bigraph
import org.bigraph.biggr.bigraph.Node
import org.bigraph.biggr.bigraph.Place
import org.bigraph.biggr.bigraph.InnerName
import org.bigraph.biggr.bigraph.Port
import org.bigraph.biggr.bigraph.Point

class Decompose(nl_mapping:NLMapping,
    nRegionsOfMatched:Int,
    nSitesOfMatched:Int) {
  var context:Bigraph = null
  var matched:Bigraph = null
  var parameter:Bigraph = {
    val r=new Array[Root](nSitesOfMatched)
    val b = new Bigraph()
    b.regions = r
    b.outerNames = ArrayBuffer()
    b}
  override def toString():String ={
    var s = "NL_Mapping: "+nl_mapping.toString()+"\n"
    s += "CONTEXT: "+context.toString()+"\n"
    s+=context.nodeNameToNode.toString()+"\n"
    s+=context.linknameToLink.toString()+"\n"
    s+= "MATCHED: "+matched.toString()+"\n"
    s+=matched.nodeNameToNode.toString()+"\n"
    s+=matched.linknameToLink.toString()+"\n"
    s+="PARA: "+parameter.toString()+"\n"
    s+=parameter.nodeNameToNode.toString()+"\n"
    s+=parameter.linknameToLink.toString()+"\n"
    s
  }
  
  private def newMatched():Unit = {
    val regions_matched = new Array[Root](nRegionsOfMatched)
    for(i <- 0 until nRegionsOfMatched)
    {
      regions_matched(i) = new Root()
    }
    regions_matched.foreach(r=>r.children = Map[Control,ArrayBuffer[Node]]())
    this.matched = new Bigraph(regions_matched)
    for(i <- 0 until nRegionsOfMatched)
    {
      regions_matched(i).bigrah = matched
    }
    for(i<-0 until nSitesOfMatched)
       matched.addSite(null)
  }
  
  
  def decompose(toBeDec:Bigraph,redex:Bigraph):Unit = {
    //�½�matched:Bigraph
    newMatched()
    //context�����½���������ԭtoBeDex
    this.context = toBeDec
    //this.context.linknameToLink = Map()
    //�Ȱѽ�㳶�ϣ��ٷ�link����ʼ����żͼ��linknametolink����ԭ��linkû�ж�Ҳû����
    //��toBeDec�еݹ�������traverseAndDec����ɳ���
    for(i<-0 until toBeDec.regions.length)
    {
      traverseAndDec(context.regions(i).children,nl_mapping.matchedNodes)
    }
    //��Ҫ����һ��matched��������еĽڵ㲻��matchedNodes�У���Щ�ڵ���d��
    //����matched��para�ĳ���
    //match�Ա���para���֣�û�й�ϵ����Ϊmatched��������õ�
    for(i<-0 until this.matched.regions.length)
    {
      traverseAndDec3(this.matched.regions(i).children,nl_mapping.matchedNodes)
    }
    

    //��redex��outername������matched,����ʱ������points
    //����redex�����ֵĳ���
    //ͬʱ����Matched�������ֺ�context��������
    for(i<- 0 until redex.outerNames.size)
    {
      matched.outerNames.append(new OuterName(i, Set[Point]()))
      context.innerNames.append(new InnerName(i,null))
    }
    //����redex�����ֵĳ���
    //ͬʱ����Matched��������: ��redex��innername������matched
    //��para��������
    for(i<- 0 until redex.innerNames.size)
    {
      if(redex.innerNames(i).link.isInstanceOf[OuterName])
        matched.innerNames.append(new InnerName(i,null))
      else if(redex.innerNames(i).link.isInstanceOf[Edge])
      {
        matched.innerNames.append(new InnerName(
            i,nl_mapping.matchedLinks(redex.innerNames(i).link)))
      }
      parameter.outerNames.append(new OuterName(i, Set[Point]()))
    }
    //��redex��outername���֣�
    //����NL_mapping:
    //�ߵ��߲���
    //����/��Ҫ����
    for(i<- 0 until redex.outerNames.size)
    {
      val re_outer = redex.outerNames(i)
      val agent_link = nl_mapping.matchedLinks(redex.outerNames(i))
      val outer = matched.outerNames(i)
      re_outer.points.foreach(p=>{
        val agent_point = p match{
          case p:InnerName =>
            matched.innerNames(p.index)
          case p:Port =>
            val N = nl_mapping.matchedNodes(p.node)//��λ����Ӧ��matched�еĽ��
            N.ports(p.index)
        }
        agent_point.link = outer
        agent_link.points -= agent_point
        agent_link.points += context.innerNames(i)
        outer.points += agent_point
        context.innerNames(i).link = agent_link
      })
      nl_mapping.matchedLinks(redex.outerNames(i))=matched.outerNames(i)
    }
    //��ʼ����żͼ��linknametolink��nodeNameToNode
    context.initialize()
    matched.initialize()
    parameter.initialize()
    //��matched�������ֵ�link���֣�
    for(i<- 0 until matched.innerNames.size)
    {
      val link = matched.innerNames(i).link
      if(link.isInstanceOf[Edge])
      {
        parameter.linknameToLink -= link.name
        val points = link.points
        points.foreach(p=>{
          if(p.isInstanceOf[Port])
          {
            val port = p.asInstanceOf[Port]
            if(parameter.nodeNameToNode.contains(port.node.name))
            {
              port.link = parameter.outerNames(i)
              parameter.outerNames(i).points += port
              points -= p
            }
          }
        })
      }
    }
    
  }
  private def traverseAndDec3(nodes:Map[Control,ArrayBuffer[Node]],
      matchedNodes:Map[Node,Node]):Unit = {
    //�ܱ�֤���ǣ����뵽�����nodesȫ�Ǳ�matched���Ľڵ�
    if(nodes==null)
      return
    for((control,nodeArray)<-nodes)
    {
      //����ÿ��node,��Ҫ����Ӧ��controlNode��û��site���ӡ�����У�Ҫ�Ѳ�ƥ��ڵ����site
      nodeArray.foreach(node=>{
        var candidate_d:Site = null
        val unmAtch = Map[Control,ArrayBuffer[Node]]()
        val mAtch = Map[Control,ArrayBuffer[Node]]()
        //pap���ڴ��治ƥ��Ľڵ㣬Ҫ�Ž�para�е�
        var controlNode = matchedNodes.find(p=>p._2 == node) match
		{
		  case Some(pair) => pair._1
		  case None =>  null
		}
        //���controlNode���ֵ���d����ô��ƥ��node��Ҫ�Ž�d��
        //���ֵ�site���һ��candidate_d��
        if(controlNode.sites.size!=0)
        {
          candidate_d=controlNode.sites.head
        }
        for((control1,nodeArray1)<-node.children)
        {
          nodeArray1.foreach(node1=>{
            if(matchedNodes.values.toSet.contains(node1))
            {
              if(!mAtch.contains(control1))
                mAtch(control1)=ArrayBuffer[Node]()
              mAtch(control1)+=node1         
            }
            //��ƥ��ڵ�Ҫ�Ž�pap��
            else
            {
              if(!unmAtch.contains(control1))
                unmAtch(control1)=ArrayBuffer[Node]()
              unmAtch(control1)+=node1
            }
          })
        }  
        node.children = mAtch
        traverseAndDec3(mAtch,matchedNodes)
        //�����site����
        if(candidate_d!=null)
        {
          val s = new Site(candidate_d.index,node)
          node.sites = ArrayBuffer(s)
          matched.sites(candidate_d.index)=s
		    //�½�һ��parameter��ÿ��para��Ȼ�ǵ�region.
		    //�ѱ�����������ƥ��ڵ�������para�ĵ�region��     
		    val r = new Root()
		    r.children=unmAtch
		    r.bigrah = parameter
		    parameter.regions(candidate_d.index)=r
         }
        
       })
    }
    
  }
  

  //����toBeDec��ÿ���ڵ㴦���������ڵ���matchedNodes��ֵ�У�������matched_bigraph��
  //����toBeDecԭ������site��ͬʱ����matched_bigraph��context_bigraph��links
  private def traverseAndDec(nodes:Map[Control,ArrayBuffer[Node]],
      matchedNodes:Map[Node,Node]):Unit = {
    if(nodes==null)
      return
    //hit���ĳ��site�Ƿ�����
    val hit = Set[Int]()
    //���d1|A��d2|B��ͬ��ƥ�䣬AB���ֵܽڵ�Ҫôȫ��d1Ҫôȫ��d2
    var valid_d:Site = null
    val invalid_d:ListBuffer[Site] = ListBuffer()
    for((control,nodeArray)<-nodes)
    {
      val newNodeArray = ArrayBuffer[Node]()
      nodeArray.foreach(node=>
        //���ڵ㱻ƥ��:1,2,3
        if(matchedNodes.values.toSet.contains(node))
        {
          //1.�ҵ���Ӧ��control��region��
          var controlNode = matchedNodes.find(p=>p._2 == node) match
          {
            case Some(pair) => pair._1
            case None =>  null
          }
          var idxRegion = controlNode.region.index
          //2.��region������site��û�н��������siteû�н�����ҪΪcontext����site
          if(!hit.contains(idxRegion))
          {
            //site��index����ƥ��������region��index����idxRegion������
            val parent:Place = node.parent
            val site = new Site(idxRegion,parent)
            parent.addSite(site)
            while(context.sites.size<=idxRegion)
              context.sites += null
            context.sites(idxRegion) = site
          }
          //hit������¼��ЩidxRegion�Ѿ���context������Site
          hit += idxRegion
          //3.�ѽ��ӵ�matched��region����
          if(!matched.regions(idxRegion).children.contains(control))
            matched.regions(idxRegion).children(control) = ArrayBuffer[Node]()
          matched.regions(idxRegion).children(control) += node
          node.parent = matched.regions(idxRegion)
          //node���¼���matched�Ľ�㣬Ҫ��node�Ķ˿�������link������linknameToLink��
          //node.ports.foreach(p=>{
            //matched.linknameToLink(p.link.name)=p.link}) 
            //4.���redexƥ��ڵ���ֵ���site,��Ҫ��node���ֵ�
          if(controlNode.parent.sites.size!=0)
          {
            if(valid_d!=null)
              invalid_d += valid_d
            valid_d = controlNode.parent.sites.head
          }
        }
        //���ڵ㲻��ƥ��
        else
        {
          newNodeArray += node
          //node.ports.foreach(p=>context.linknameToLink(p.link.name)=p.link)
          traverseAndDec(node.children,matchedNodes)             
        } 
      )
      //context���ǲ�ڵ���ȥ���Ž�matched�Ľڵ�
      if(newNodeArray.size==0)
        nodes-=control
      else
        nodes(control) = newNodeArray
    }
    
    if(valid_d!=null)
    {
	    //�½�һ��parameter��ÿ��para��Ȼ�ǵ�region.
	    //�ѱ�����������ƥ��ڵ�������para�ĵ�region��     
	    val r = new Root()
	    r.children=Map()++nodes
	    for((control,nodeArray)<-nodes)
	      nodes-=control	    
		r.bigrah = parameter
		parameter.regions(valid_d.index)=r
	    //���invalid_d�л���Ԫ�أ���ЩparaҪ�ÿ�
	    for(in_d<-invalid_d)
	    {
	      val r = new Root()
	      r.children=Map()
	      r.bigrah = parameter
	      parameter.regions(in_d.index)=r
	    }
        
    }
  }

}