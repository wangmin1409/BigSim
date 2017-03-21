package org.bigraph.biggr.bigraph

object ControlType extends Enumeration{
  type ControlType=Value
  val ccm,uccm=Value//ccm:conditionally concurrently matchable; 
  //uccm:unconditionally concurrently matchable
}
import ControlType._

class Control(val name:String,val arity:Int) {
  val controlType:ControlType = uccm
  override def toString():String = name
}