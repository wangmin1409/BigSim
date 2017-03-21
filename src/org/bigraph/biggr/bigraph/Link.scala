package org.bigraph.biggr.bigraph
import scala.collection.mutable.Set

abstract class Link (
    var name:String,
    val points:Set[Point]
    ){
  override def toString():String = name
}
class Edge(
    name:String,
    override val points:Set[Point]) extends Link(name,points){
  
}
class IdleEdge(
    name:String,
    override val points:Set[Point]) extends Link(name,points){
  
}
class OuterName(
    val index:Int,
    override val points:Set[Point]) extends Link("y"+index,points){

  //def this(name:String,points:Set[Point]){
   // this(name.substring(1).toInt,points)
    
  //}
}