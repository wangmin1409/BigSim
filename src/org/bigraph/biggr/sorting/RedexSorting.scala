package org.bigraph.biggr.sorting


import scala.collection.mutable.Map
import scala.collection.mutable.Set
import org.bigraph.biggr.bigraph.Control
import org.bigraph.biggr.bigraph.Node
import org.bigraph.biggr.utils.Algorithm


class RedexSorting(val sorting:Formula){
  val variables:List[String] = null
  val assignedV:List[String] = null
  def checkAll(values:Array[Node],variables:Array[String]):Boolean = {
    for(SubValues<-values.toSet.subsets(variables.length))
    {
      val sva = SubValues.toArray
      val permu = Range(0,variables.length).toArray
      do{
        val assignment:Map[String,Node] = Map()
        permu.foreach(i => assignment+=(variables(i)->sva(permu(i))))
        if(!sorting.isMatchSorted(assignment))
          return false
      }while(Algorithm.next_permutation(permu))
    }
    true
  }
}
class Sorting(val sorting:Formula){
  val variables:Array[String] = {
    val v = Set[String]()
    traverse(sorting,v)
    v.toArray
  }
  private def traverse(f:Formula,vs:Set[String]):Unit = {
    if(f.isInstanceOf[AtomicFormula])
    {
      vs.add(f.asInstanceOf[AtomicFormula].name1)
      if(f.asInstanceOf[AtomicFormula].name2.length()!=0)
        vs.add(f.asInstanceOf[AtomicFormula].name2)
    }
    else if(f.isInstanceOf[NegativeFormula])
    {
      traverse(f.asInstanceOf[NegativeFormula].t,vs)
    }
    else if(f.isInstanceOf[ConjunctiveFormula])
    {
      f.asInstanceOf[ConjunctiveFormula].ts.foreach(x=>traverse(x,vs))
    }
    else if(f.isInstanceOf[DisjunctiveFormula])
    {
      f.asInstanceOf[DisjunctiveFormula].ts.foreach(x=>traverse(x,vs))
    }
  }
  val assignedV:List[String] = null
  def checkAll(values:Array[Node]):Boolean = {
    for(SubValues<-values.toSet.subsets(variables.length))
    {
      val sva = SubValues.toArray
      val permu = Range(0,variables.length).toArray
      do{
        val assignment:Map[String,Node] = Map()
        permu.foreach(i => assignment+=(variables(i)->sva(permu(i))))
        //println(assignment.toString)
        //assignment.foreach(a=>print(a._1+"->"+a._2.name+", "))
        //println()
        if(!sorting.isMatchSorted(assignment))
          return false
      }while(Algorithm.next_permutation(permu))
    }
    true
  }
}

class Formula{
  def isMatchSorted(assignment:Map[String,Node]):Boolean = {
    var b:Boolean = false
    if(this.isInstanceOf[NegativeFormula])
    {
      b = !this.asInstanceOf[NegativeFormula].t.isMatchSorted(assignment)
    }
    else if (this.isInstanceOf[ConjunctiveFormula])
    {
      val f_list = this.asInstanceOf[ConjunctiveFormula].ts
      //f_list.forall(f=>f.isMatchSorted(redex, matching))
      f_list.foreach(f=>{
        if(!f.isMatchSorted(assignment))
          return false
      })
      b = true
    }
    else if(this.isInstanceOf[DisjunctiveFormula])
    {
      val f_list = this.asInstanceOf[DisjunctiveFormula].ts
      f_list.foreach(f=>{
        if(f.isMatchSorted(assignment))
          return true
      })
      b = false
    }
    else if(this.isInstanceOf[AtomicFormula])
    {
      b = this.asInstanceOf[AtomicFormula].isMatchSortedForAtomic(assignment)
    }
    b 
  }
}
class NegativeFormula(val t:Formula) extends Formula()
{}
class ConjunctiveFormula(val ts:List[Formula]) extends Formula()
{}
class DisjunctiveFormula(val ts:List[Formula]) extends Formula()
{}
class AtomicFormula(val name1:String,val name2:String, var control:Control, var cat:Int) extends Formula()
{
  def this(name1:String,name2:String,cat:Int) = this(name1,name2,null,cat)
  def this(name1:String,control:Control,cat:Int) = this(name1,"",control,cat)
  var u:Node = null
  var v:Node = null
  var i:Int = 0
  var j:Int = 0
  //val cat:Int = 0//1:/;2://;3:@//4:C(u)
  def isChild(node1:Node,node2:Node):Boolean = {
    node2.parent==node1}
  def isDescendant(node1:Node,node2:Node):Boolean = {
    var p = node2.parent
    while(p!=null)
    {
      if(p==node1)
        return true
      if(p.isInstanceOf[Node])
        p = p.asInstanceOf[Node].parent
      else
        p = null
    }
    false}
  def isLinked(node1:Node,node2:Node):Boolean = {
    if(node1.ports.length<=i || node2.ports.length<=j )
      return false
    node1.ports(i).link==node2.ports(j).link
  }
  def isControl(node:Node,control:Control):Boolean = {
    node.control==control
  }
  def isMatchSortedForAtomic(assignment:Map[String,Node]):Boolean = {
    var b = true
    if(this.cat!=4)//��Ԫ
    {
	    val node1 = assignment(name1)
	    val node2 = assignment(name2)
	    b = this.cat match{
	      case 1 => {//print(name1+"/"+name2)
	      isChild(node1,node2)}
	      case 2 => {//print(name1+"//"+name2)
	        isDescendant(node1,node2)}
	      case 3 => {//print(name1+"@"+name2)
	        isLinked(node1,node2)}
	    }
	    
    }
    else//һԪ
    {
      val node1 = assignment(name1)
      b = isControl(node1,control)
      //print(name1+"("+control+")")
    }
    //println(b.toString)
    b
  }
}

