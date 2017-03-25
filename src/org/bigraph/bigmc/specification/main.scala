package org.bigraph.bigmc.specification
import scala.collection.mutable.Set;
import scala.collection.mutable.Map;

/**
 * @author amy

 */

class pair(a:A,b:A){
  var p1:A = a;
  var p2:A = b;
}

class A{
  var a:Int = 0;
}


object main extends App{
  
  override def main(args: Array[String]): Unit = {
    
    
    
        var s = Set();
        var s0 = new A();
        var s1 = new A();
        var s2 = new A();
        var l:Set[pair] = Set();
        var p1 = new pair(s0,s1);
        var p2 = new pair(s0,s2);
        l.add(p1);
        l.add(p2);
      
      
      def getBi():Set[A] = {
        var s:Set[A] = Set();
        
        l.foreach { p => 
          s.add(p.p1);
          s.add(p.p2);
        }
        return s;
      }
      
      
      
      def getNextFromBI(io:A):A = {
        var res:A = null;
        l.foreach { pair =>  
          if(pair.p1 ==io) {
            res = pair.p1;
          } 
        }
        return res;
      }
        
      
  }
}