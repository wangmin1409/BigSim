package org.bigraph.bigsim

import org.bigraph.bigsim.model.Bigraph

import scala.collection.mutable.Set;
import java.lang.Boolean
import scala.collection.mutable.ListBuffer;
import org.bigraph.bigmc.specification.GenBigraph;


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
   
   def AddModel(b:Bigraph) = {
     if(curBI!=null)
        curBI.linked = b;
      curBI = b;
      models.add(b);
   }
   
   
   
   def isSame(bl:Bigraph,br:Bigraph):Boolean = {
    
     //if(bl==null||br==null||bl.root==null||br.root==null)return false;
      println("Verify_isSame_bl_root:"+bl.root);
     println("Verify_isSame_br_root:"+br.root);
     
     if(bl.isInitial ||br.isInitial) return true;
     if(bl.isFinal&&bl.lable!=null&&bl.lable.equals("true"))return true;
     if(br.isFinal&&br.lable!=null&&br.lable.equals("true"))return true;
     
     if(bl.root.toString().equals(br.root.toString())){
       return true;
     }
     
     return false;
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
          if(isSame(node.s,node.q)){
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
   
   def getNextFromSpec(spec:Bigraph):Set[Bigraph] = {
     return null; 
   }
}

object test extends App{
   override def main(args: Array[String]) = {     
   }
}
