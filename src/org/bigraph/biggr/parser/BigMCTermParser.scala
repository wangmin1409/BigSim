package org.bigraph.biggr.parser


import org.bigraph.biggr.model._
import org.bigraph.biggr.bigraph._
import org.bigraph.biggr.utils._
import org.bigraph.biggr.simulator._
import org.bigraph.biggr.sorting._
import scala.util.parsing.combinator.RegexParsers 
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import java.lang.System

trait ParsersUtil1{
  val identifier = "[a-zA-Z][a-zA-Z0-9]*".r
  val number = "[0-9]+".r
}
object InterResForModel{
  var nodenameToControl:Map[String,Control] = Map()
  
}
object InterResForBigraph{
  var sites:ArrayBuffer[Site] = ArrayBuffer()
  var linknameToLink:Map[String,Link] = Map()
}


class ModelParser1 extends RegexParsers with ParsersUtil1{
  def addAndGetLinkName(linkName:String):Link={
    if(!InterResForBigraph.linknameToLink.contains(linkName))
      {
        InterResForBigraph.linknameToLink(linkName) = linkName match {
          case _ => new Edge(linkName,Set())
        }
      }
    InterResForBigraph.linknameToLink(linkName)
  }
  def name:Parser[Link] = (identifier~":"~"edge" ^^
  {case linkName~":"~"edge" => 
    addAndGetLinkName(linkName)
    } 
  | "idle" ^^{
    case "idle" => 
      new IdleEdge("",Set())
    }
   )
  def nodeOrSite:Parser[Place] = ( identifier~opt(":"~identifier)~opt("["~repsep(name,",")~"]")^^
  {case nodeName~Some(":"~controlName)~Some("["~namelist~"]") => 
	    val ports = new Array[Port](namelist.length)
	    val node = InterResForModel.nodenameToControl match{
	      case null => new Node(nodeName,null,NodeState.active,ports)
	      case _ => new Node(nodeName,InterResForModel.nodenameToControl(controlName),NodeState.active,ports)
	    }
	    //����һ���ڵ������link��Ҳ���Ƕ˿�
	    var i = 0
	    for(link<-namelist)
	    {
	      val port = new Port(null,link,node,i)
	      ports(i) = port
	      link.points += port
	      i+=1
	    }
	    node
     case nodeName~Some(":"~controlName)~None =>
        new Node(nodeName,InterResForModel.nodenameToControl(controlName),NodeState.active,new Array[Port](0))

     case controlName~None~Some("["~namelist~"]") =>
        val ports = new Array[Port](namelist.length)
	    val node = InterResForModel.nodenameToControl match{
	      case null => new Node(controlName,null,NodeState.active,ports)
	      case _ => new Node(controlName,InterResForModel.nodenameToControl(controlName),NodeState.active,ports)
	    }
	    //����һ���ڵ������link��Ҳ���Ƕ˿�
	    var i = 0
	    for(link<-namelist)
	    {
	      val port = new Port(null,link,node,i)
	      ports(i) = port
	      link.points += port
	      i+=1
	    }
	    node
     case controlName~None~None=>
       new Node(controlName,InterResForModel.nodenameToControl(controlName),NodeState.active,new Array[Port](0))
       
  }|
    "$"~number^^{
     case "$"~siteIdx =>
       new Site(siteIdx.toInt,null)
   }
  )
  
   def parallel:Parser[List[Place]] = (//"nil"^^(x=>null)
    rep1sep(nesting,"|")^^{
      case placelist =>
        placelist
      }
   )  
   
    def nesting:Parser[Place] = 
       nodeOrSite~opt("."~("("~parallel~")"|nesting))^^{
         case parent~Some("."~("("~placeList~")")) =>
           val map:Map[Control,ArrayBuffer[Node]]= Map()
           placeList match{
             case lll:List[Place] =>{
               lll.foreach(place=>{
	             place match {
	               case site:Site=>
	                 parent.addSite(site)
	                 while(InterResForBigraph.sites.size<=site.index)
	                   InterResForBigraph.sites+=null
	                 InterResForBigraph.sites(site.index)=site
	                 site.parent = parent
	               case node:Node=>
	                 if(map.contains(node.control))
		               map(node.control) += node
		             else
		               map += (node.control->ArrayBuffer(node))
		             node.parent = parent
	             }
	           })
             }
           }
           parent.children = map
           parent

         case parent~None  => //parent.children=null
           parent
         case parent~Some("."~child) =>
           child match{
             case child:Node =>
               parent.children = Map(child.control->ArrayBuffer(child))
               child.parent = parent
               parent
             case child:Site =>
               parent.addSite(child)
               child.parent = parent
                while(InterResForBigraph.sites.size<=child.index)
	                   InterResForBigraph.sites+=null
	                 InterResForBigraph.sites(child.index)=child
               parent
           } 
       }
       
 
   def region:Parser[Root] = (parallel^^ {
     case p => val r = new Root()
     val map:Map[Control,ArrayBuffer[Node]]= Map()
   
               p.foreach(place=>{
	             place match {
	               case site:Site=>
	                 r.addSite(site)
	                 site.parent = r
	                 while(InterResForBigraph.sites.size<=site.index)
	                   InterResForBigraph.sites+=null
	                 InterResForBigraph.sites(site.index)=site
	               case node:Node=>
	                 if(map.contains(node.control))
		               map(node.control) += node
		             else
		               map += (node.control->ArrayBuffer(node))
		             node.parent = r
	             }
	           })
           
           r.children = map
     r}
     |"#Nil"^^{
       case "#Nil"=> val r = new Root()
       //r.children = null
       r
     }
   )

   def agent:Parser[Bigraph] = rep1sep(region,"||") ^^ {
     case r =>
       val region = r.toArray
       val b = new Bigraph(region)
       b.sites = InterResForBigraph.sites
       for(i <- 0 until region.length)
       {
         region(i).index = i
         region(i).bigrah = b
       }
        InterResForBigraph.sites = ArrayBuffer()
        InterResForBigraph.linknameToLink = Map()
       b
   }

}


