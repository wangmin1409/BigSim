package org.bigraph.biggr.simulator

import org.bigraph.biggr.bigraph._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import org.bigraph.biggr.bigraph.Bigraph

class Compose {
  var context:Bigraph = null
  var matched:Bigraph = null
  var parameter:Bigraph = null
  def this(c:Bigraph,m:Bigraph,p:Bigraph){
    this()
    this.context = c
    this.matched =  m
    this.parameter = p
  }
  def composeAB(A:Bigraph,B:Bigraph):Bigraph = {
    for(site<-A.sites)
    {
      //println("A:"+A.toString)
      //println("sites:"+A.sites.toString)
      val map = site.parent.children
      
        val B_region = B.regions(site.index)
        if(B_region.children!=null)//nil
        {
	      for((control,nodeArray)<-B_region.children)
	      {
	        if(map.contains(control))
	          map(control) ++=nodeArray
	        else
	          map(control) = ArrayBuffer()++nodeArray
	        nodeArray.foreach(n=>n.parent = site.parent)
	      }
        }
      site.parent.sites = ArrayBuffer()
    }
    A.sites=B.sites
    for(i<-0 until A.innerNames.length)
    {
      val innername = A.innerNames(i)
      val outername = B.outerNames(i)
      val link = innername.link
      outername.points.foreach(p=>{
        p.link = link
        link.points -= innername
        link.points += p
        })
      
    }
    A.innerNames = B.innerNames
    A.initialize()
    A
  }
  def compose():Bigraph = {
    val b1 = composeAB(this.context,this.matched)
    //println("b1: "+b1.toString())
    val b2 = composeAB(b1,this.parameter)
    //println("b2: "+b2.toString())
    b2
  }

}