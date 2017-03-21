package org.bigraph.biggr.bigraph

import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.bigsim.model._
import scala.collection.mutable.ArrayBuffer


class Bigraph{
  var regions:Array[Root] = null
  var sites:ArrayBuffer[Site] = ArrayBuffer()
  var linknameToLink:Map[String,Link] = null
  var nodeNameToNode:Map[String,Node] = null
  var innerNames:ArrayBuffer[InnerName] = ArrayBuffer()
  var outerNames:ArrayBuffer[OuterName] = ArrayBuffer()

  
  var allNodes:List[Node] = List()
  //初始化：设置每个节点的root。把所有link放进linknameToLink集合中,如果link是outername，还要放入outernames中
  //初始化：把每个节点加入nodeNameToNode
  //这个initialize函数不仅在bigraph的构造函数中有用，在其他bigraph产生变化的时候也要用到
  //decompose过程中，context的nodeNameToNode要去掉所有matched的结点，通过initialize完成
  //compose过程中，context的nodeNameToNode要加上所有turnedMatched的结点，也通过initialize完成
  def initialize():Unit = {
    this.linknameToLink = Map()
    this.nodeNameToNode = Map()
    for(i<-0 until this.regions.length)
    {
      traverseNodes(this.regions(i).children,this.regions(i))
    }
  }
  private def traverseNodes(nodes:Map[Control,ArrayBuffer[Node]],Region:Root):Unit = {
    if(nodes==null)
      return
    for((control,nodeArray)<-nodes)
    {
      for(node<-nodeArray)
      {
        nodeNameToNode(node.name) = node
        node.ports.foreach(p=>{
          linknameToLink(p.link.name)=p.link
          if(p.link.isInstanceOf[OuterName])
          {
            val outer = p.link.asInstanceOf[OuterName]
            while(outerNames.size<=outer.index)
              outerNames += null
            outerNames(outer.index) = outer
          }
        })
        node.region = Region
        traverseNodes(node.children,Region)     
      }
    }
  }
  def addSite(s:Site) = {sites+=s}
  //构造函数
  def this(regions:Array[Root]){
    this()
    this.regions = regions
    //this.linknameToLink = Map()
    //this.nodeNameToNode = Map()
    this.innerNames = ArrayBuffer()
    this.outerNames = ArrayBuffer()
    initialize()
  }
  //拷贝构造函数：深拷贝
  def this(bigraph:Bigraph){
    this()
    this.regions = new Array[Root](bigraph.regions.size)
    this.linknameToLink = Map()
    this.nodeNameToNode = Map()
    this.sites = ArrayBuffer[Site]()
    for(i<-0 until bigraph.sites.size)
    {this.sites+= null
    }
    //linknameToLink,nodeNameToNode会在initialize中被初始化
    //linknameToLink在调用recur_copy时也会初始化一次，在initialize中再次初始化是冗余的
    
    for(i<-0 until bigraph.regions.length)
    {
      this.regions(i) = new Root()
      var region = this.regions(i)
      region.index = i
      region.bigrah = this
      recur_copy(this,region,bigraph.regions(i),this.linknameToLink)
    }
    this.innerNames = ArrayBuffer()
    for(innername<-bigraph.innerNames)
    {
      this.innerNames += new InnerName(innername.index,linknameToLink(innername.link.name))
    }
    //innerNames需要linknameToLink，要在initialize之后再初始化
    this.outerNames = ArrayBuffer()
    for(i<- 0 until bigraph.outerNames.length)
    {
      this.outerNames += linknameToLink("y"+i.toString).asInstanceOf[OuterName]
    }
    //outerNames属于link,和其他link一样在initialize过程中初始化
    initialize()//用于初始化nodeNameToNode
  }
  private def recur_copy(this_bigraph:Bigraph,p1:Place,p2:Place,linknameToLink:Map[String,Link]):Unit = {
    for(site<-p2.sites)
    {
      val s = new Site(site.index,p1)
      p1.sites += s
      this_bigraph.sites(site.index)=s
    }
    if(p2.children==null)
      return
    p1.children = Map()
    for((control,nodeArray)<-p2.children)
    {
      p1.children(control) = ArrayBuffer()
      for(node<-nodeArray)
      {
        val n = new Node(node,linknameToLink)
        n.parent = p1
        p1.children(control) += n
        recur_copy(this_bigraph,n,node,linknameToLink)
      }
    }
  }
  /*
  traverseNodes(topNodes)
  var NodesOfControl:Map[Control,List[Node]] = {
    val map:Map[Control,List[Node]] = Map()
    allNodes.foreach(node => {
      if(map.contains(node.control))
        map(node.control) = node::map(node.control)
      else
        map += (node.control->List(node))
    })
    map
  }
  def traverseNodes(nodes:List[Place]):List[Node] = {
    if(nodes!=null)
	    for(node<-nodes)
	    {
	      node match
	      {
	        case node:Node => allNodes = node::allNodes
	        traverseNodes(node.children)
	      }    
	    }
    allNodes
  }
	*/
  override def toString():String ={
    var str = ""
    if(innerNames!=null && innerNames.length >0)
      str+= innerNames.mkString("{",",","}")
    str += this.regions.mkString("||")
    str
  } 

  def isIdentical(bigraph:Bigraph):Boolean = {
    var b = true
    if(this.regions.size!=bigraph.regions.size ||
        this.innerNames.size != bigraph.innerNames.size ||
        this.outerNames.size != bigraph.outerNames.size)
      return false
    for(i<-Range(0,this.innerNames.length))
    {
      if(!this.innerNames(i).link.name.equals(bigraph.innerNames(i).link.name))
        return false
    }
    for(i<-Range(0,this.regions.length))
    {
      b = isIdentical_For(this.regions(i).children,bigraph.regions(i).children)
    }
    return b    
  }
  def isIdentical_For(A:Map[Control,ArrayBuffer[Node]],
     B:Map[Control,ArrayBuffer[Node]]):Boolean = {
    if((A==null || A.size==0) &&(B==null ||B.size==0))
      return true
    else if((A==null || A.size==0))
      return false
    else if(B==null ||B.size==0)
      return false
    for((control,nodesA)<-A)
    {
      if(!B.contains(control))
        return false
      val nodesB = B(control)
      if(nodesA.size!=nodesB.size)
        return false
      for(nodeA <-nodesA)
      {
        nodesB.find(b=>b.name.equals(nodeA.name)) match{
          case None => return false
          case Some(nodeB) => 
            if(!isIdentical_For(nodeA.children,nodeB.children))
              return false
            else
            {
              for(pi<-Range(0,nodeA.ports.length))
              {
                if(!nodeA.ports(pi).link.name.equals(nodeB.ports(pi).link.name))
                  return false
              } 
            }
          }
      }
    }
    true
  }
}