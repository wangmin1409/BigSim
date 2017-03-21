package org.bigraph.bigsim.strategy
import scala.xml._
import scala.collection.mutable.Map
import scala.collection.mutable.Set

/**
 * @author zhaoxin
 * version 0.1
 */
class FilterRules {
	def getNamesOfFocusRules(filterRules : scala.xml.Elem) : Set[String] = {
	   var focusRules =
	    (Set[String]() /: (filterRules \\ "FocusRule")){
	    (set, fruleNode) =>
	      var fruleName = (fruleNode \ "@name").toString()
	      set += fruleName
	      set
	   }
	   focusRules
	}
	
	def getNamesMapOfReactionRuleRelationship(filterRules : scala.xml.Elem) : Map[String, String] = {
	  var reactionRuleRelationship = 
	    (Map[String, String]() /: (filterRules \\ "ReactionRule")){
	     (map, rrnode) =>
	       var rrname = (rrnode \ "@name").toString()
	       var nextName = (rrnode \ "NextRule" \ "@name").toString()
	       map += (rrname -> nextName)
	       map
	  }
	  reactionRuleRelationship
	}
	
}

object testXml {
  def main(args: Array[String]) {
	  var filterRules = XML.load("filterrules/FilterRules.xml")

	  var reactionRuleRelationship = 
	    (Map[String, String]() /: (filterRules \\ "ReactionRule")){
	     (map, rrnode) =>
	       var rrname = (rrnode \ "@name").toString()
	       var nextName = (rrnode \ "NextRule" \ "@name").toString()
	       map += (rrname -> nextName)
	       map
	  }
	  
	  var focusRules =
	    (Set[String]() /: (filterRules \\ "FocusRule")){
	    (set, fruleNode) =>
	      var fruleName = (fruleNode \ "@name").toString()
	      set += fruleName
	      set
	  }
      
	  println("The reactionRuleRelationship is: " + reactionRuleRelationship)
	  println("The focusRules is: " + focusRules)
  }
}