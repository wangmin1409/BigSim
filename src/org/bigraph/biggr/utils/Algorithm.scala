package org.bigraph.biggr.utils

import scala.collection.mutable.ArrayBuffer
import org.bigraph.biggr.simulator._

object Algorithm {
  
  def dikaer(A:ArrayBuffer[NLMapping],B:ArrayBuffer[NLMapping]):ArrayBuffer[NLMapping]={
    if(A.isEmpty)
      return B
    else if(B.isEmpty)
      return A
    val res = ArrayBuffer[NLMapping]()
     for(i<- 0 until A.length)
     {
        for(j<-0 until B.length)
        {
           val one = new NLMapping(A(i))
           one.add(B(j))
           res += one
        }
     }
     res
  }
  def swap(a:Int, b:Int, arr:Array[Int])= {
    val swap = arr(a);arr(a)=arr(b);arr(b)=swap
  }
  def next_permutation(a:Array[Int]):Boolean={
    if(a.length<2)
      return false
	var i = a.length-2
	var ii = i+1
	while(i>=0 && a(i)>=a(ii) )
	{i-=1;ii-=1}
    if(i<0)
      return false
	var j = a.length-1
	while(a(j)<=a(i))
	{j-=1}
	swap(i,j,a)
	var revI = i+1
	var revJ = a.length-1
	for(iter<-Range(0,(a.length-i-1)/2))
	{
	    swap(revI,revJ,a)
	    revI+=1
	    revJ-=1
	}
	true
  }

}