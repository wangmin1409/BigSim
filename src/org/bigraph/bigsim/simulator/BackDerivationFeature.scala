package org.bigraph.bigsim.simulator

import scala.collection.mutable.Queue
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.immutable.TreeMap
import scala.collection.mutable.TreeSet
import scala.util.Random
import java.io._
import org.bigraph.bigsim._
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.model._
import org.bigraph.bigsim.BRS.Match
import org.bigraph.bigsim.BRS.Graph
import org.bigraph.bigsim.BRS.Vertex
import org.bigraph.bigsim.data.Data
import org.bigraph.bigsim.utils.Graphviz
import org.bigraph.bigsim.parser.BooleanExprParser

class BackDerivationFeature() {

  var states: Queue[Tuple2[Double, Vertex]] = Queue();
  var simQueue: TreeMap[Double, Queue[Match]] = TreeMap();
  var reactNodes: Set[String] = Set();
  var reactTimeSet: TreeSet[Double] = TreeSet();
  var checkCondition: Boolean = true; //每条回溯出的节点检验是否符合条件，不符合则停止反应

  var steps: Int = 0;

  var v: Vertex = MainSimulator.v
  // add the initial agent to the simQueue
  states += ((0, v))

  def step(): Boolean = {

    /**
     * if meet max system clock, simulation stop.
     */
    println("GlobalCfg.SysClk: " + GlobalCfg.SysClk); //lbj
    if (GlobalCfg.SysClk >= GlobalCfg.maxSysClk) {
      var v: Vertex = states.last._2
      v.terminal = true //add by lbj
      println("sim::step Interrupted!  Reached maximum SysClk: " + GlobalCfg.maxSysClk);

      return false;
    }

    /**
     * 0: update
     * If sim queue contains reactions happen at this time,
     * try match and apply match.
     */
    update() //这就是对simQueue中reactTime=SysClk的applyMatch，第一次simQueue为0，执行了也进不去

    /**
     * 1: add match
     */
    if (!addMatch()) { //这里就是调match方法，正常返回true，那就不会走这里，不正常返回false，才会走这里，这就是findMatch，reactTime是反应后的时间，这里是先把反应时间推进，在apply反应
      return false
    }

    /**
     * 2: update if current match doesn't need reaction time
     * If sim queue contains reactions happen at this time,
     * try match and apply match.
     */
    update() //只有非random的且自己定义了SysClk为0的这里的update才会进去，因为addMatch的 var reactTime = GlobalCfg.SysClk + RRIncr的RRIncr为0
    //这时的reactTime=SysClk，在SysClk加1之前applyMatch

    TimeSlicingSimulator1.matchGC;
    if (reactTimeSet.size > 0) {
      GlobalCfg.SysClk = reactTimeSet.head //reactTimeSet最后一轮会被清空，所以这里.head空指针
    } else
      GlobalCfg.SysClk = GlobalCfg.maxSysClk //若reactTime为空证明反应结束没有新的反应了，直接set到系统最大时钟
    println("React Time:" + GlobalCfg.SysClk)
    Data.update("SysClk", GlobalCfg.SysClk.toString) //更新data中的系统时间
    true;
  }

  /**
   * update
   * update matches once the system clock meets
   */
  def update() { //第一次都不会走，第一次simQueue都为空，除非有反应时间为0的

    if (simQueue.contains(GlobalCfg.SysClk)
      && !simQueue.get(GlobalCfg.SysClk).isEmpty) { //第二次进update，simQueue里包含SysCLK为1

      if (GlobalCfg.DEBUG)
        println("Update-----System Clock now:" + GlobalCfg.SysClk)

      // apply these matches
      var reactList: Queue[Match] = simQueue.getOrElse(GlobalCfg.SysClk, Queue()) //返回key指定的value，如果没有则返回指定的默认值，这里如果Map里没有则返回空队列
      // match and find match and apply match!!!
      var v: Vertex = states.last._2
      var curBigraph = v.bigraph
      var curRRs: Set[ReactionRule] = Set()
      // record the cond
      var rules: Map[String, List[String]] = Map()
      var conds: List[String] = List()

      while (reactList.size > 0) { //当前可并发反应的规则
        var tm = reactList.dequeue
        var matches: Set[Match] = curBigraph.findMatchesOfRR(tm.rule) //当前bigraph与一个反应规则匹配
        if (matches != null) {
          var matched: Boolean = false //去重

          matches.map(m => {

            if (!matched && m.getReactNodes.equals(tm.getReactNodes)) {

              var nb: Bigraph = curBigraph.applyMatch(m)
              /**
               * update a reaction rule data model
               */
              m.rule.updateBD(tm) //更新data
              /**
               * update agent data with clock
               */
              Data.updateDataCalcsWithClk(tm.RRIncr.toString) //更新Data中的时针

              curRRs += m.rule

              if (rules.contains(m.rule.name)) { //如果rules的key已经有这条反应规则name，则把新node加入value的List集合中
                rules.put(m.rule.name, rules.getOrElse(m.rule.name, List()).++(m.reactNodes.toList))
              } else { //如果rules的key没有这条反应规则name，则新建一个value
                rules.put(m.rule.name, m.reactNodes.toList) //eg. var rules: Map[String, List[String]]为Map(r_businessman_take_taxi_from_CBD -> List(David))这个规则作用了哪些nodes
              }
              if (!m.rule.getConds.equals("")) { //若这个match的反应规则有条件，则加入到conds这个集合中
                conds = conds.:+(m.rule.getConds)
              }

              reactNodes = reactNodes.filter(!m.reactNodes.contains(_)) //过滤出符合filter条件的元素，即把不在集合m.reactNodes中的元素放入reactNodes，即过滤掉m.reactNodes的东西 

              matched = true //这个match已经发生过反应
              if (nb.root == null) {
                nb.root = new Nil();
              }
              curBigraph = nb

              m.rule.bdconds.foreach(c => { //反应完的偶图是否满足这条规则的条件
                var cond = c
                reactNodes.foreach(rn => {
                  if (cond.contains(rn)) {
                    var rns = m.reactNodesMap.getOrElse(rn, Set())
                    if (rns.size > 0) {
                      cond = cond.replaceAll(rn, rns.head)
                      println(cond)
                    }
                  }
                })
                
                val q = BooleanExprParser.parse(cond)

                if (!q.check()) {
                  checkCondition = false
                }
              })
            }

          })
        }
      }

      if (curBigraph != null && curRRs != null) {
        var nv = new Vertex(curBigraph, v, curRRs, true)
        nv.sysClk = GlobalCfg.SysClk

        v.addTargets(curRRs, nv);
        nv.addParents(v);

        if (MainSimulator.g.lut.contains(nv.hash)) { //建立nv和v的父子关系放到Graph的look up table中
          nv = MainSimulator.g.lut(nv.hash);
        } else {
          MainSimulator.g.add(nv);

        }
        states += (GlobalCfg.SysClk -> nv) //这个SysClk生成的vertex
      }

      reactTimeSet.-=(GlobalCfg.SysClk)
      // finally, delete it!
      simQueue = simQueue.-(GlobalCfg.SysClk) //applyMatch后反应完了就删掉，所以下次没有了
    }
  }

  def addMatch(): Boolean = { //此方法加到simQueue
    var v: Vertex = states.last._2 //最新生成的Vertex
    steps += 1;
    var b: Bigraph = v.bigraph;
    var matches: Set[Match] = b.findMatches; // check conds
    var bdConds: Set[String] = Set() //由本条Expr转换而来

    if (steps >= GlobalCfg.maxSteps) {
      v.terminal = true
      println("Terminal bigraph: " + b.toString());
      println("sim::step Interrupted!  Reached maximum steps: " + GlobalCfg.maxSteps);
      return false;
    }

    if(!checkCondition){v.terminal = true; return false;}
    /**
     * If a reaction rule is not random and not conflict,
     * it must happen when it is matched.
     */
    println("lbj---matches set size:" + matches.size)
    matches.map(m => {
      if (GlobalCfg.verbose) {
        println("All match:" + m.rule.name + "\tcond:" +
          m.rule.conds + "\treactNodes:" + m.reactNodes)
      }

      val conflict = m.conflict(reactNodes.toList)
      val RRIncr = m.rule.getRRIncr //这里调用的是整数，只会是每个时间点
      var reactTime = GlobalCfg.SysClk + RRIncr //非随机这个RRIncr就是bgm文件定义的SysClk，reactTime就是反应完的时间点，如第一次反应变为1

      if (!conflict && reactTime <= GlobalCfg.maxSysClk) { //add by lbj 反应结束的时间点如果超过了规定的最大时间就应该不加入反应集合
        //if (!conflict && !m.rule.random) {

        m.RRIncr = RRIncr
        var queue: Queue[Match] = null
        if (simQueue.contains(reactTime)) { //所有反应完为这个reactTime的都放到simQueue的key为reactTime中，如反应完时间为1的都放入simQueue(1,反应完时间都为1的Queue[Match])
          queue = simQueue(reactTime)
          queue += m
        } else {
          queue = Queue(m) //每个时间点第一个匹配会走这里
        }
        simQueue += reactTime -> queue //每个时间点所有能反应的match组成一个queue，如第一次匹配，SysClk从0开始，第一次反应，reactTime变为1
        reactTimeSet += (reactTime); //重复加也没问题，集合不会重复
        reactNodes ++= m.reactNodes //把处理过的node放入
        if (GlobalCfg.verbose) {
          println("add match: " + m.rule.name + "\treact nodes:" +
            m.reactNodes + "\treact time:" + reactTime)
        }
      } else if (conflict) {
      }
    })
    return true;
  }
}

object TestBackDerivationFeature {
  def main(args: Array[String]) {
    var dis: BackDerivationFeature = new BackDerivationFeature();

  }
}
