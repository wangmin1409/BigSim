package org.bigraph.biggr.bigraph
import scala.collection.mutable.Set
import scala.collection.mutable.Map
abstract class Point(
    val name:String) {
  var link:Link
}
class Port(
    override val name:String,
    var link:Link,
    var node:Node,
    val index:Int) extends Point(name){
  
  def this(port:Port,linknameToLink:Map[String,Link]){
    this(port.name,null,null,port.index)
    if(linknameToLink.contains(port.link.name))
      this.link = linknameToLink(port.link.name)
    else
    {
      if(port.link.name.length==0)
        this.link = new IdleEdge("",Set())
      else if(port.link.name.charAt(0)=='y')
        this.link = new OuterName(port.link.name.substring(1).toInt,Set())
      else 
        this.link = new Edge(port.link.name,Set())
      linknameToLink(port.link.name) = this.link
    }
    this.link.points += this
  }
  override def toString:String = {
    node.name+":"+index
  }
}
class InnerName(
    val index:Int,
    var link:Link) extends Point("x"+index){
  override def toString:String = {
    name+"/"+link.name
  }
}