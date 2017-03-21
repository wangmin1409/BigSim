package org.bigraph.biggr.bigraph

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
abstract class Place() {
  var name:String 
  var children:Map[Control,ArrayBuffer[Node]] = Map()
  var sites:ArrayBuffer[Site] = ArrayBuffer()
  def addSite(s:Site) = {
    if(sites==null)
      sites=ArrayBuffer()
    else
      sites+=s}
  //输出从本节点起包括其所有嵌套节点的字符串
  override def toString():String = {
    val childrenPlace = ListBuffer[Place]()++this.children.values.toList.flatten
    childrenPlace ++= this.sites
    var str =
    childrenPlace.size match
	    {
	      case 0 =>
	          this.name
	      case 1 => 	        
	          this.name+"."+childrenPlace.head.toString()
	      case _ =>
	        this.name+"."+childrenPlace.mkString("(","|",")")
	    }
    str
  }
 

}

object NodeState extends Enumeration{
  type NodeState=Value
  val active,passive=Value
}
import NodeState._

class Node(
    n:String,
    val control:Control,
    val state:NodeState,
    val ports:Array[Port]) extends Place(){
  
  
  var name:String = n
  val id:Int = {
    val id_str = name.filter(p=>Set('0','1','2','3','4','5','6','7','8','9').contains(p))
    if(id_str.length()==0)
      0
    else
      id_str.toInt
  }
  var region:Root = null
  var parent:Place = null
  //拷贝构造函数：深拷贝;需要linknameToLink为参数传给Port的拷贝构造函数
  def this(node:Node,linknameToLink:Map[String,Link]){
    this(node.name,node.control,node.state,new Array[Port](node.ports.length))
    for(i<- 0 until this.ports.length)
    {
      this.ports(i) = new Port(node.ports(i),linknameToLink)
      this.ports(i).node = this
    }
  }
  def hasSameControl(other:Node):Boolean={control.controlType==other.control.controlType}

  override def toString():String = {
    val linkString =this.name +  ports.map(s=>s.link).mkString("[",",","]")
    val childrenPlace = ListBuffer[Place]()++this.children.values.toList.flatten
    childrenPlace ++= this.sites
    var str =
    childrenPlace.size match
	    {
	      case 0 =>
	          linkString
	      case 1 => 	        
	          linkString+"."+childrenPlace.head.toString()
	      case _ =>
	        linkString+"."+childrenPlace.mkString("(","|",")")
	    }
    str
  }
 
}
class Root(
    ) extends Place(){
  var name:String = "root"
  var index:Int = 0
  var bigrah:Bigraph = null
  //override def toString():String = super.toString.substring(6,super.toString.length()-1)

  override def toString():String = {
    val childrenPlace = ListBuffer[Place]()++this.children.values.toList.flatten
    childrenPlace ++= this.sites
    var str =
    childrenPlace.size match
	    {
	      case 0 =>
	          "nil"
	      case 1 => 	        
	          childrenPlace.head.toString()
	      case _ =>
	        childrenPlace.mkString("|")
	    }
    str
  }
}
class Site(
    val index:Int,
    var parent:Place
    ) extends Place(){
  var name:String = "$"+index
  override def toString():String={this.name}
}