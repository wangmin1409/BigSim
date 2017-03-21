package org.bigraph.bigsim.model

import java.util.Random

import scala.collection.mutable.Set

import org.bigraph.bigsim.parser.TermParser
import org.bigraph.bigsim.utils.GlobalCfg

/**
 * @author zhaoxin
 * version 0.1
 */
object TermString {

  def preOrderString(t: Term): String = {
    if (t == null) "";
    else t.termType match {
      case TermType.TPREF => preOrderString(t.asInstanceOf[Prefix]);
      case TermType.TPAR => preOrderString(t.asInstanceOf[Paraller]);
      case TermType.TREGION => preOrderString(t.asInstanceOf[Regions]);
      case TermType.THOLE => preOrderString(t.asInstanceOf[Hole]);
      case TermType.TNIL => preOrderString(t.asInstanceOf[Nil]);
      case _ => "<untyped term>";
    }
  }

  def preOrderString(t: Prefix): String = {
    if ("" == t.node.name) {
      t.node.ctrl.name + fports(t.node.ports) + "." + preOrderString(t.suffix);
    } else {
      if (GlobalCfg.node)
        t.node.name + ":" + t.node.ctrl.name + fports(t.node.ports) + "." + preOrderString(t.suffix);
      else
        t.node.ctrl.name + fports(t.node.ports) + "." + preOrderString(t.suffix);
    }
  }

  def preOrderString(t: Paraller): String = {
    var children: Set[Term] = t.getChildren;
    var childrenStr: List[String] = List();
    var res: String = "";

    children.map(x => {
      childrenStr = childrenStr.:+(preOrderString(x));
    });
    childrenStr = childrenStr.sortWith((a: String, b: String) => a < b);
    "(" + childrenStr.mkString("|") + ")";
  }

  def preOrderString(t: Regions): String = {
    var children: List[Term] = t.getChildren;
    var childrenStr: List[String] = List();
    var res: String = "";

    children.map(x => {
      childrenStr = childrenStr.:+(preOrderString(x));
    });
    childrenStr = childrenStr.sortWith((a: String, b: String) => a < b);
    "(" + childrenStr.mkString("||") + ")";
  }

  def preOrderString(t: Nil): String = {
    "nil";
  }

  def preOrderString(t: Hole): String = {
    "$" + t.index;
  }

  def fports(prt: List[Name]): String = {
    if (prt.size == 0) "";
    else {
      var nameList: List[String] = List();
      prt.map(x => {
        if ("idle" == x.name)
          nameList = nameList.:+(x.name);
        else {
          if (GlobalCfg.node)
            nameList = nameList.:+(x.name + ":" + x.nameType);
          // for xuxu
          else
            nameList = nameList.:+(x.name);
        }
      });
      "[" + nameList.mkString(",") + "]";
    }
  }
}

object testSubtree {
  def main(args: Array[String]) {

    var t2 = TermParser.apply("a:Airport.(d:Security.(Computer[connect:edge,idle].SecurityInfo[idle].nil|Ticket[hasTicket:edge].nil|bb:hangLuggage[hasHangLuggage:edge,idle].nil|w:Passenger[idle,idle,idle,hasHangLuggage:edge,idle,idle,idle,hasTicket:edge,idle,idle].nil)|database[connect:edge].(dataDangerousList[idle].nil|dataDangerousList[idle].nil|dataDangerousList[idle].nil|dataDangerousList[idle].nil|database[connect:edge].database[connect:edge].(b:Checkin.personinfo[idle].nil|c:PassagewayToSecurity.m:Light[idle].nil|e:PassagewayToShop.n:Light[idle].nil|f:ShoppingZone.(p:BillBoard[idle].nil|q:Store.v:Computer[connect:edge,idle].nil)|g:PassagewayToGate.o:Light[idle].nil|h:GateLounge.l:Computer[connect:edge,idle].nil|i:database[connect:edge].(r:dataPassenger[idle,idle,idle,idle,idle,idle,hasTicket:edge,idle,hasHangLuggage:edge,hasCheckinLuggage:edge].nil|s:dataHangLuggage[hasHangLuggage:edge,idle,idle].nil|t:dataTicket[idle,idle,idle,idle,hasTicket:edge].nil|u:datacheckinLuggage[idle,hasCheckinLuggage:edge,idle,idle].nil|x:dataAD[idle,idle].nil|y:dataShopPrefer[idle,type:edge].nil|z:dataProduct[idle,type:edge,idle].nil))))");
    var t1 = TermParser.apply("Security.(Passenger[idle,idle,idle,hasHangLuggage:edge,idle,idle,passCom:edge,hasTicket:edge,idle,idle] | Ticket[hasTicket:edge] | hangLuggage[hasHangLuggage:edge,hasHangOrNot:edge] | Computer[connect:edge, passCom:edge].$2) | database[connect:edge].$3 | Equal[hasHangOrNo:edge, hasHang:edge] | True[hasHang:edge] | $0");
    var rule = new ReactionRule("liangwei", t1, t2, "");

    println(t1.toString)
    println(t2.toString)
    println(rule)
    val rand = new Random(System.currentTimeMillis())
    val x = rand.nextInt(1000)
    println(x)

    /*  t1 = TermParser.apply("Pharmacy.Computer[connected:edge] | ChargingRoom.(Patient[patient_prescription:edge,patient_bill_payed:edge,isDecoction:edge].IsDecoction[isDecoction:edge,value_is:edge,leftValue:edge].Value[value_is:edge] | $1) | Equal[leftValue:edge,rightValue:edge] | False[rightValue:edge] | $0")
    rule = new ReactionRule("liangwei", t1, t2);
    println(rule.toString)

    println("t1:" + t1 + ", orderedString:" + Subtree.preOrderString(t1));
    println("t2:" + t2 + ", orderedString:" + Subtree.preOrderString(t2));

    var t3 = TermParser.apply("recv[a].recv[a]|send[a].send[a]|send[a].recv[a].recv[a]");
    println("t3:" + t3 + ", orderedString:" + Subtree.preOrderString(t3));
    var t4 = TermParser.apply("recv[a].recv[a]|send[a].recv[a].recv[a]|send[a].send[a]");
    println("t4:" + t4 + ", orderedString:" + Subtree.preOrderString(t4));
*/
  }
}