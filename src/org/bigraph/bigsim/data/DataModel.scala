package org.bigraph.bigsim.data

import org.bigraph.bigsim.model.Name
import org.bigraph.bigsim.model.Nil
import org.bigraph.bigsim.model.Paraller
import org.bigraph.bigsim.model.Prefix
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.model.Regions
import org.bigraph.bigsim.model.Term
import org.bigraph.bigsim.model.TermType
import org.bigraph.bigsim.parser.TermParser

object DataModel {
  var dataCtrls: List[String] = null
  var dataPorts: List[String] = null
  var relations: List[String] = List("Greater", "Less", "Equal", "NotEqual",
    "LessOrEqual", "GreaterOrEqual")
  var dataValues: Map[String, String] = null

  dataCtrls = List()
  dataPorts = List()
  dataValues = Map("leftValue" -> "10", "rightValue" -> "20",
    "patient_leftValue" -> "20", "patient_rightValue" -> "20",
    "d" -> "20", "g" -> "15", "h" -> "13", "e" -> "16", "age_value_is" -> "14")

  def relationDecision(relation: Prefix): Boolean = {
    if (relation.node.ports.size != 2) {
      println("relation prefix must have 2 ports, not " + relation.toString)
      false
    } else {
      var leftValue = dataValues(relation.node.ports(0).name)
      var rightValue = dataValues(relation.node.ports(1).name)

      if (leftValue != null && rightValue != null) {
        relation.node.ctrl.name match {
          case "Greater" => leftValue > rightValue
          case "Less" => leftValue < rightValue
          case "Equal" => leftValue == rightValue
          case "NotEqual" => leftValue != rightValue
          case "LessOrEqual" => leftValue <= rightValue
          case "GreaterOrEqual" => leftValue >= rightValue
          case _ => true
        }
      } else {
        true
      }
    }
  }
}

object DataSpliter {

  /**
   * pre order data structure, and split data info.
   */
  def preOrderData(t: Term, position: String, rr: ReactionRule) {
    if (t == null) return ;
    else t.termType match {
      case TermType.TPREF => preOrderData(t.asInstanceOf[Prefix], position, rr);
      case TermType.TPAR => preOrderData(t.asInstanceOf[Paraller], position, rr);
      case TermType.TREGION => preOrderData(t.asInstanceOf[Regions], position, rr);
      case _ => return ;
    }
  }

  def preOrderData(t: Prefix, position: String, rr: ReactionRule) {
    updatePorts(t)
    if (DataModel.relations.contains(t.node.ctrl.name)) {
      rr.data += t
      updateParent(t, position)
    } else if (DataModel.dataCtrls.contains(t.node.ctrl.name))
      updateParent(t, position)
    else
      preOrderData(t.suffix, "suffix", rr)
  }

  def preOrderData(t: Paraller, position: String, rr: ReactionRule) {
    var rightTerm = t.rightTerm
    var grandfather = t.parent
    preOrderData(t.leftTerm, "leftTerm", rr)

    if (rightTerm.parent == grandfather) {
      updateSub(t.parent, rightTerm, position)
      preOrderData(rightTerm, position, rr)
    } else {
      preOrderData(t.rightTerm, "rightTerm", rr)
    }
  }

  def preOrderData(t: Regions, position: String, rr: ReactionRule) {
    var rightTerm = t.rightTerm
    var grandfather = t.parent
    preOrderData(t.leftTerm, "leftTerm", rr)
    if (rightTerm.parent == grandfather) {
      updateSub(t.parent, rightTerm, position)
      preOrderData(rightTerm, position, rr)
    } else
      preOrderData(t.rightTerm, "rightTerm", rr)
  }

  /**
   * update ports who contains data link
   */
  def updatePorts(t: Prefix) {
    var ports: List[Name] = List[Name]()
    ports.map(p => {
      if (DataModel.dataPorts.contains(p.name)) {
        p.name = "idle"
        p.nameType = "idle"
      }
    })
  }

  /**
   * update parent node
   */
  def updateParent(t: Term, position: String) {
    val positions = List("root", "leftTerm", "rightTerm",
      "suffix")

    position match {
      case "root" => println("bad format")
      case "suffix" => {
        t.parent.asInstanceOf[Prefix].suffix = new Nil()
      }
      case "leftTerm" => {
        if (t.parent.isInstanceOf[Paraller])
          t.parent.asInstanceOf[Paraller].rightTerm.parent = t.parent.parent
        else if (t.parent.isInstanceOf[Regions])
          t.parent.asInstanceOf[Regions].rightTerm.parent = t.parent.parent
      }
      case "rightTerm" => {
        if (t.parent.isInstanceOf[Paraller])
          t.parent.asInstanceOf[Paraller].leftTerm.parent = t.parent.parent
        else if (t.parent.isInstanceOf[Regions])
          t.parent.asInstanceOf[Regions].leftTerm.parent = t.parent.parent
      }
    }
  }

  /**
   * Parameters: parent, child, position: the position of child in its parent
   */
  def updateSub(parent: Term, child: Term, position: String) {
    parent.termType match {
      case TermType.TREGION => {
        if (position == "leftTerm" && parent != null)
          parent.asInstanceOf[Regions].leftTerm = child
        else
          parent.asInstanceOf[Regions].rightTerm = child
      }
      case TermType.TPAR => {
        if (position == "leftTerm" && parent != null)
          parent.asInstanceOf[Paraller].leftTerm = child
        else
          parent.asInstanceOf[Paraller].rightTerm = child
      }
      case TermType.TPREF => {
        parent.asInstanceOf[Prefix].suffix = child
      }
      case _ => return
    }
  }
}

object testDataModel {
  def main(args: Array[String]) {
    println(DataModel.relationDecision(
      TermParser.apply("Greater[leftValue, rightValue]").asInstanceOf[Prefix]))
  }
}
  