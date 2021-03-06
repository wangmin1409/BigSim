package org.bigraph.bigsim

import org.bigraph.bigsim.model.Bigraph

import scala.collection.mutable.Set;
import java.lang.Boolean
import scala.collection.mutable.ListBuffer;
import scala.collection.Parallel
import org.bigraph.bigsim.specification.GenBigraph;

/*
 * author: yanwei
 * this mod is for auto verify 
 */

class GraphNode(ss:Bigraph,qq:Bigraph){
  var s:Bigraph = ss;  //model bigraph
  var q:Bigraph = qq;  //spec bigraph
  var next:Set[GraphNode] = Set();
  
  def AddNext(n:GraphNode) = {
    next.add(n);
  }
}

object Verify{
   
   var modelIndex:Int = 0; 
  
   private var models: Set[Bigraph] = Set();
   
   private var specification: Set[Bigraph] = Set();
   
   var graph:Set[GraphNode] = Set() ;
   
   var curBI:Bigraph = null;
   
   def AddModel(b:Bigraph):Unit = {
     models.foreach { model =>  
       if(VerifyMatcher.BigraphIsEqual(b.root,model.root)){
         return;
       }
     }
     if(curBI!=null)
        curBI.linked = b;
      curBI = b;
      models.add(b);
   }
   
   def Calculate(){
     /*
      *  step1: 构建同步积
      */
     
     var sIndex = 0;
     var mIndex = 0;
     
     specification = GenBigraph.getAllBigraph();
     
      specification.foreach { x => 
         models.foreach { y =>
             if(y.verifyID.equals("")){
               y.verifyID = "S" +mIndex;
               mIndex = mIndex +1;
             }
              if(x.verifyID.equals("")){
               x.verifyID = "P" +mIndex;
               sIndex = sIndex +1;
             }
             
             var g = new GraphNode(y,x);
             graph.add(g);   
         }    
      }
      
       /*
      *  step2: 去false节点 
      */
          
      var g:Set[GraphNode] = Set();
      
      graph.foreach { node => 
          if(VerifyMatcher.Match(node.s, node.q)){
            g.add(node);
          }
      }
      
      graph = g;
      
      /*
       * step3: 建立链接关系
       */
      graph.foreach { node => 
            graph.foreach { otherNode => 
              if(node!=otherNode){
                if(node.s.linked == otherNode.s){
                  var sets = GenBigraph.getNextBigraph(node.q);
       
                  sets.foreach { allLinked =>  
                      if(allLinked ==otherNode.q){
                        node.AddNext(otherNode);
                      }  
                  }
                }
              }
              }
      }        
       /*
       * step4: 找连通分量，判断满足性
       */
      var al = new TarJanAlgorithm(graph);
      al.run();
   }
   
}

class AAA{
  
}


object test extends App{
   override def main(args: Array[String]) = {     
        var a:Set[AAA] = Set();
        var bbb = new AAA()
       
        a.add(bbb)
        
        bbb = new AAA();
        a.add(bbb);
        a.foreach { x => println(x) }
   }
}
