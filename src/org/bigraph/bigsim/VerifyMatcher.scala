package org.bigraph.bigsim

import org.bigraph.bigsim.model.Term
import java.lang.Boolean
import org.bigraph.bigsim.model.Bigraph
import org.bigraph.bigsim.model.TermType
import org.bigraph.bigsim.model.Prefix
import org.bigraph.bigsim.model.Paraller
import scala.collection.Parallel


/*
 * this mod is for match in verify (curState with proposition match)
 * author: yanwei
 * */

object VerifyMatcher {
  
  def Match(bl:Bigraph,br:Bigraph):Boolean = {
     if(bl.isInitial ||br.isInitial) return true; 
     if(bl.isFinal&&bl.label!=null&&bl.label.equals("true"))return true;
     if(br.isFinal&&br.label!=null&&br.label.equals("true"))return true;
     return BigraphIsEqual(bl.root,br.root)
  }
  
  def BigraphIsEqual(bl:Term,br:Term):Boolean = {
    if(bl.termType != br.termType)return false;
    if(bl.termType == TermType.TPREF){
      var p1 = bl.asInstanceOf[Prefix];
      var p2 = br.asInstanceOf[Prefix]
        if(p1.suffix.termType ==TermType.TNIL&&p2.suffix.termType ==TermType.TNIL){
          if(!bl.termType.toString().equals(br.termType.toString())){
            return false;
          }
        }
        if(p1.node.name.equals(p2.node.name)&&p1.node.ctrl.name.equals(p2.node.ctrl.name)){
          return BigraphIsEqual(p1.suffix,p2.suffix)
        }
    }
    else if(bl.termType==TermType.TPAR){
      var p1 = bl.asInstanceOf[Paraller];
      var p2 = br.asInstanceOf[Paraller];
      return (BigraphIsEqual(p1.leftTerm,p2.rightTerm)&&
        BigraphIsEqual(p1.rightTerm,p2.leftTerm))||
         (BigraphIsEqual(p1.leftTerm,p2.leftTerm)&&
        BigraphIsEqual(p1.rightTerm,p2.rightTerm))
    }
    return true;
  }
}