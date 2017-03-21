package org.bigraph.biggr.model

import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.simulator._
import org.bigraph.biggr.sorting._
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import org.bigraph.biggr.sorting.RedexSorting

class ReactionRule(   
     val redex:Bigraph,var sorting:RedexSorting,
     val reactum:Bigraph) {
  var name:String=""
  var expression:String=""
  var index:Int = -1
  //var sorting:Formula = null
  override def toString():String = {
    name+": "+redex.toString + "->" + reactum.toString
  }
  def buildMap():(Map[Node,Node],Map[Link,Link])={
    val nodesMap = Map[Node,Node]()
    val linksMap = Map[Link,Link]()
    (nodesMap,linksMap)
  }
  def buildCopyOfReactum(nl_mapping:NLMapping,naming:NamingNodes):Bigraph = {
    val nodes_redexToMatched = nl_mapping.matchedNodes
    val links_redexToMatched = nl_mapping.matchedLinks
      
    val bigraph = new Bigraph(reactum)
    var m1 = Map[String,Node]()
    bigraph.nodeNameToNode.foreach(pair=>{
      var nodename = pair._1
      val node = pair._2
      if(redex.nodeNameToNode.contains(nodename))
      {
        val redexNode = redex.nodeNameToNode(nodename)
        if(nodes_redexToMatched.contains(redexNode))
        {
          nodename = nodes_redexToMatched(redexNode).name
          node.name = nodename
        }
      }
      else
      {
        
        naming.NumOfEachControl(node.control)+=1
        nodename = node.control.name.toLowerCase+
         naming.NumOfEachControl(node.control).toString
        
        node.name = nodename
      }
      m1+=(nodename->node)
      }
    )
    bigraph.nodeNameToNode = m1
    var m2 = Map[String,Link]()
    bigraph.linknameToLink.foreach(pair =>{
      var linkname = pair._1
      val link = pair._2
      if(redex.linknameToLink.contains(linkname))
      {
        val redexLink = redex.linknameToLink(linkname)
        if(links_redexToMatched.contains(redexLink))
        {
          val matchLink = links_redexToMatched(redexLink)
          if(matchLink.isInstanceOf[OuterName])
          {
            val outername = matchLink.asInstanceOf[OuterName]
            while(bigraph.outerNames.size<=outername.index)
              bigraph.outerNames+=null
            bigraph.outerNames(outername.index) = link.asInstanceOf[OuterName]//outername
          }
          linkname =  matchLink.name
          link.name = linkname
        }
      }
      m2+=(linkname->link)}
    )
    bigraph.linknameToLink = m2
    bigraph
  }
   
}