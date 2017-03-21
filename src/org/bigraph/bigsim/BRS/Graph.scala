package org.bigraph.bigsim.BRS

import java.io.File
import java.io.FileWriter
import java.io.Writer
import scala.collection.mutable.Map
import scala.collection.mutable.Set
import scala.collection.mutable.Stack
import scala.xml.XML
import org.bigraph.bigsim.data.DataModel
import org.bigraph.bigsim.model.ReactionRule
import org.bigraph.bigsim.strategy.ParseRules
import org.bigraph.bigsim.strategy.ParseXML
import org.bigraph.bigsim.utils.GlobalCfg
import org.bigraph.bigsim.data.Data
import org.bigraph.bigsim.utils.Graphviz
import org.bigraph.bigsim.strategy.PatternFlow
import org.bigraph.bigsim.model.Term
import javax.xml.bind.annotation.XmlElementDecl.GLOBAL

object Graph {
  var pathNum: Int = 0; //对每次只产生一条模拟路径的模拟器，pathNum即loopNum
  var loopNum: Int = 0;
  var loopNumPath: Int = 0;
  var path: String = "";
  var rrs: String = "";
  var variables: String = "";
  var dot: String = "";
  //  var patternPathStr: String = "";
  //  var patternPaths: Set[Stack[Vertex]] = Set();
}

class Graph(init: Vertex) {
  val root: Vertex = init;
  var pathColor: String = Graphviz.getColor
  //lut: look up table
  val lut: Map[Int, Vertex] = Map();
  if (root != null) lut(root.hash) = root;

  def add(v: Vertex): Unit = {
    lut(v.hash) = v;
  }

  def size: Int = lut.size;

  def backTrace(v: Vertex): String = {
    //todo
    var vertex: Vertex = v;
    var i: Int = 0;
    var here: String = "";
    while (vertex != null) {
      if (i == 0) here += "   <- *** VIOLATION *** \n";
      // FIXME: 字符串连接顺序可能有误，需要反过来？
      here += "#" + i + " " + vertex.bigraph.root.toString() + "\n";
      if (vertex.reactionRule == null) {
        here += " >> (root)\n";
      } else {
        here += " >> " + vertex.reactionRule.toString + "\n";
      }
      i += 1;
      vertex = vertex.parent;
    }
    here;
  }

  /**
   * get Pathes has interested pattern
   */
  def getPathsHasInterestPatterns: Set[Stack[Vertex]] = {
    var result: Set[Vertex] = Set()
    var allPaths: Set[Stack[Vertex]] = getAllPaths
    println("All pathes size is: " + allPaths.size)
    var selectedPaths: Set[Stack[Vertex]] = Set() //这里用于存储筛选掉之后的所有path

    allPaths.map(ite => {
      var pathStack: Stack[Vertex] = Stack()
      var size: Int = ite.size

      var ruleName: Array[String] = new Array[String](size)

      for (i <- 0 to size - 1) {
        if (ite.head.reactionRule != null) {
          ruleName(i) = ite.head.reactionRule.name
        } else {
          ruleName(i) = null
        }
        pathStack.push(ite.head)
        ite.pop
      }

      var containsInteresPattern: Boolean = false
      var interesRules: Set[String] = ParseXML.getNamesOfIntrestedPatterns(root.bigraph, XML.load(GlobalCfg.patternFile))
      interesRules.map(it => {
        if (ruleName.contains(it)) {
          containsInteresPattern = true
        }

      })

      if (containsInteresPattern) {
        pathStack.map(itps => {
          ite.push(itps)
        })
      }
      if (ite != null) {
        selectedPaths += ite
      }
      pathStack.clear
    })
    selectedPaths
  }
  /*
	 * 找一个数据结构，存储根据Vertexs中属性terminal为true的找出来每一条路径。
	 * 中间，根绝策略筛选路径。
	 * 筛选好的所有路径放入到一个set中去,返回的路径是以dot文件的形式，所以
	 */
  def findPathsByStrategy(rules: Set[ReactionRule]): Set[Stack[Vertex]] = {
    //这里，到底提供那种strategy，可以再global里面定义
    var result: Set[Vertex] = Set()
    var allPathes: Set[Stack[Vertex]] = getAllPaths
    var selectedPathes: Set[Stack[Vertex]] = Set() //这里用于存储筛选掉之后的所有path

    var dcuMaps: Map[String, Set[String]] = ParseRules.getAllDCUs(rules)
    var dpuMaps: Map[String, Set[String]] = ParseRules.getAllDPUs(rules)
    var sameDefRules: Map[String, Set[String]] = ParseRules.getAllRulesWithSameDef(rules)

    allPathes.map(ite => {
      var pathStack: Stack[Vertex] = Stack()
      var size: Int = ite.size
      var ruleName: Array[String] = new Array[String](size)

      for (i <- 0 to size - 1) {
        if (ite.head.reactionRule != null) {
          ruleName(i) = ite.head.reactionRule.name
        } else {
          ruleName(i) = null
        }
        pathStack.push(ite.head)
        ite.pop
      }

      var containsDU: Boolean = false

      //保证du的定义是清纯的，其实可以直接找路径中是否存在du对，已经概括了其中的情况了。
      for (i <- 0 to (size - 2) if containsDU == false) {
        for (j <- (i + 1) to (size - 1) if containsDU == false) {
          if (ruleName(i) != null) {
            if (GlobalCfg.allUses) {
              if (dcuMaps(ruleName(i)).contains(ruleName(j))) {
                for (k <- (j + 1) to (size - 1) if containsDU == false) {
                  if (dpuMaps(ruleName(i)).contains(ruleName(k))) {
                    containsDU = true
                  }
                }
              } else if (dpuMaps(ruleName(i)).contains(ruleName(j))) {
                for (k <- (j + 1) to (size - 1) if containsDU == false) {
                  if (dcuMaps(ruleName(i)).contains(ruleName(k))) {
                    containsDU = true
                  }
                }
              }
            } else if (GlobalCfg.allDefs) {
              if (dcuMaps(ruleName(i)).contains(ruleName(j)) || dpuMaps(ruleName(i)).contains(ruleName(j))) {
                containsDU = true
              }
            }

          }
        }
      }

      if (containsDU) {
        pathStack.map(itps => {
          ite.push(itps)
        })
      }
      if (ite != null) {
        selectedPathes += ite
      }
      pathStack.clear
    })

    selectedPathes
  }

  /**
   * 使用一个stack来存储一条路径。
   * 然后把所有路径放到一个set里面去。
   */
  def getAllPaths: Set[Stack[Vertex]] = {
    var allPathSet: Set[Stack[Vertex]] = Set()
    println("Vertexs size is : " + lut.values.size)

    lut.values.map(ite => {
      if (ite.parent != null) {
        var lastVertex: Boolean = true
        lut.values.map(it => {
          if (it.parent != null && ite.hash == it.parent.hash) {
            lastVertex = false
          }
        })
        if (lastVertex == true) {
          var pathStack: Stack[Vertex] = Stack()
          var tempVertex: Vertex = ite
          pathStack.push(tempVertex)
          while (tempVertex.parent != null) {
            pathStack.push(tempVertex.parent)
            tempVertex = tempVertex.parent
          }
          allPathSet.add(pathStack)
        }
      }
    })
    allPathSet
  }

  def dumpPaths(): String = {
    var allRulesNum: Double = root.bigraph.rules.size
    var defPathMap: Map[String, Set[Int]] = Map()

    var paths: Set[Stack[Vertex]] = Set()
    if (GlobalCfg.allUses || GlobalCfg.allDefs) {
      paths ++= findPathsByStrategy(root.bigraph.rules)
    } else if (GlobalCfg.checkInterestPattern) {
      paths ++= getPathsHasInterestPatterns
    } else {
      paths ++= getAllPathss
    }

    var out: String = ""
    var pathNum: Int = -1
    paths.map(ite => {
      var ruleNameSet: Set[String] = Set()
      ite.map(itN => {
        if (itN.reactionRule != null) {
          ruleNameSet.add(itN.reactionRule.name)
        }
      })
      var ruleNameSize: Double = ruleNameSet.size
      var ruleCoverage: String = (math floor (ruleNameSize / allRulesNum) * 100).toString + "%" //向下取整

      if (ite.size != 0) {
        pathNum += 1

        if (GlobalCfg.allDefs) {
          // 得到每一个def和路径的映射，def1{0， 2， 4， 8}；
          root.bigraph.rules.map(ruleIte => {
            if (ruleNameSet.contains(ruleIte.name)) {
              ruleIte.defTerm.map(defTermIte => {
                if (defPathMap.contains(defTermIte.toString)) {
                  defPathMap(defTermIte.toString).add(pathNum)
                } else {
                  defPathMap += (defTermIte.toString -> Set(pathNum))
                }
              })
            }
          })
        }

        out += pathNum + "{\n"
        //中间输出一个stack里面的Vertex的model，每一个model中间用分号分割
        while (ite.size > 0) {
          var rootStr: String = ite.head.bigraph.root.toString
          if (rootStr.charAt(0) != '(') {
            out += rootStr
          } else {
            out += rootStr.substring(1, rootStr.size - 1)
          }
          out += ";\n"
          ite.pop
        }
        out += "}" + ruleCoverage + "\n"
        println("pathNum: " + pathNum)
      }

    })
    if (GlobalCfg.pathOutput != "") {
      var file: File = new File(GlobalCfg.pathOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
      writer.close()
    }

    if (GlobalCfg.defPathMapFile != "") {
      var defPath: String = ""
      defPathMap.map(ite => {
        defPath += ite._1 + "{"
        var tempdefPath: String = ""
        ite._2.map(it => {
          tempdefPath += it + ", "
        })
        defPath += tempdefPath.substring(0, tempdefPath.size - 2)
        defPath += "}\n"
      })

      var file: File = new File(GlobalCfg.defPathMapFile);
      var writer: Writer = new FileWriter(file);
      writer.write(defPath);
      writer.flush;
    }
    println(out)
    out
  }

  def formatHash(hash: Int): String = { //格式化生成的hashCode，若小于零则返回 "_hashcode绝对值"
    if (hash < 0) "_" + hash.abs;
    else hash.toString;
  }

  def getAllPathss: Set[Stack[Vertex]] = {
    var allPathSet: Set[Stack[Vertex]] = Set()
    var allPathString: Set[String] = Set()
    root.terminal = true
    addPaths("", allPathString, root) //调用后把结果都存在allPathString里面

    allPathString.map(path => {
      var p: Stack[Vertex] = Stack()
      var Vertexs = path.split("#")
      for (i <- 0 to Vertexs.size - 1) {
        if (!Vertexs(Vertexs.size - 1 - i).contains("r_")) { //反应规则必须以r_开头，要不这里逻辑走不通
          var Vertex = lut.getOrElse(Vertexs(Vertexs.size - 1 - i).toInt, null) //查不到就置为空，主要看lut
          if (Vertex != null)
            p.push(Vertex)
          else
            println("error!")
        }
      }
      allPathSet.add(p)

    })

    allPathSet

  }

  var firstTime: Boolean = true
  //var pathFile: File = new File("Airport/paths/BusinessNormal.txt");
  //var writer: Writer = new FileWriter(pathFile);

  def addPaths(path: String, allPathString: Set[String], currentVertex: Vertex) { //调用时传入的是root，这个方法主要看target
    if (currentVertex.terminal && !firstTime) { //为终点且firstTime=false时才会退出这个方法

      println("allPathString: " + path + currentVertex.hash);
      allPathString.add(path + currentVertex.hash)
      //writer.write(path + currentVertex.hash + "\n");
      //writer.flush;
      return
    } else { //不为终点或firstTime=true时
      firstTime = false
      currentVertex.target.map(target => { //var target: Map[Vertex, ReactionRule] = Map();
        if (!path.contains(currentVertex.hash + "#" + target._2.name + "#" + target._1.hash)) {
          addPaths(path + currentVertex.hash + "#" + target._2.name + "#", allPathString, target._1) //递归，自己调用自己
          //          println("path:"+path+"   "+currentVertex.hash + "#" + target._2.name + "#" + target._1.hash);
          //          println(path + currentVertex.hash + "#" + target._2.name + "#"+"  "+ allPathString + "  "+target._1);
        }
      })
    }

  }

  //add by lbj
  def dumpPath(): Unit = {
    var allRulesNum: Double = root.bigraph.rules.size
    var defPathMap: Map[String, Set[Int]] = Map()
    Graph.loopNumPath += 1
    var enumPathNum: Int = 0
    var paths: Set[Stack[Vertex]] = Set()

    if (GlobalCfg.checkPattern) {
      var all = getPathSet //每次一条路径
      paths ++= getPathsHasPattern(all)
      println("paths: " + paths)
    } else {
      paths ++= getPathSet
      println("paths: " + paths)
    }

    var out: String = ""
    var rrStr: String = ""
    var variableStr: String = ""
    //    var pathNum: Int = -1
    paths.map(ite => {
      rrStr = (Graph.pathNum + 1) + "{\n"
      var ruleNameSet: Set[String] = Set() //不会重名
      ite.map(itN => {
        if (itN.reactionRule != null) {
          ruleNameSet.add(itN.reactionRule.name)
          rrStr += (itN.reactionRule.name + "\n")
        } else if (itN.reactionRules.size > 0) { // add by LBJ 添加并发反应处理
          itN.reactionRules.foreach { x => { ruleNameSet.add(x.name); rrStr += (x.name + "\n") } }
        }

      })
      rrStr += "}\n"
      Graph.rrs += rrStr;
      var ruleNameSize: Double = ruleNameSet.size
      var ruleCoverage: String = (math floor (ruleNameSize / allRulesNum) * 100).toString + "%" //向下取整

      if (ite.size != 0) {
        Graph.pathNum += 1

        variableStr = Graph.pathNum + "{\n"
        if (GlobalCfg.checkTime) { ite.foreach { x => variableStr += ("SysClk=" + x.sysClk + "," + x.variables + ";\n") } } //考虑时间则打印时间
        else ite.foreach { x => variableStr += (x.variables + ";\n") }
        variableStr += "}\n"

        out += Graph.pathNum + "{\n"
        //中间输出一个stack里面的Vertex的model，每一个model中间用分号分割
        while (ite.size > 0) {
          var rootStr: String = ite.top.bigraph.root.toString //modify by lbj
          if (rootStr.charAt(0) != '(') {
            out += rootStr
          } else {
            out += rootStr.substring(1, rootStr.size - 1)
          }
          out += ";\n"
          ite.pop
        }
        out += "}" + ruleCoverage + "\n"
        Graph.path += out;
        Graph.variables += variableStr // add by lbj 添加变量打印
        println("pathNum: " + Graph.pathNum)
        println(out)
      }
    })

    if ((GlobalCfg.SimulatorClass.startsWith("Enum")) && (Graph.loopNumPath == GlobalCfg.simLoop)) { //枚举输出哪次结果都一样，这里输出最后一次的结果

      if (GlobalCfg.pathOutput != "") {
        var file: File = new File(GlobalCfg.pathOutput);
        var writer: Writer = new FileWriter(file);
        writer.write(out);
        writer.flush;
        writer.close();
      }
    } else if (Graph.loopNumPath == GlobalCfg.simLoop && !GlobalCfg.SimulatorClass.startsWith("Enum")) {
      if (GlobalCfg.pathOutput != "") { //add by lbj 最后统一输出
        var file: File = new File(GlobalCfg.pathOutput);
        var writer: Writer = new FileWriter(file);
        writer.write(Graph.path); //每次产生一条路径的模拟器将每次Loop的结果合起来输出
        writer.flush;
        writer.close();
      }
      //add by lbj输出到data文件
      if (GlobalCfg.dataOutput != "") { //有输出路径且不是Enum，因为枚举模拟器是不考虑数据的   
        var writer: Writer = new FileWriter(GlobalCfg.dataOutput, GlobalCfg.append);
        writer.write(Graph.variables);
        writer.close();
      }
    }

    if (GlobalCfg.outputRR) {
      var writer: Writer = new FileWriter(GlobalCfg.RROutput, GlobalCfg.append);
      writer.write(Graph.rrs);
      writer.close();
    }
  }
  
  /**
   * select paths has pattern's define/cuse/puse
   * add by lbj
   */
  def getPathsHasPattern(allpaths: Set[Stack[Vertex]]): Set[Stack[Vertex]] = {
    var result: Set[Vertex] = Set()
    println("------allpaths: " + allpaths)
    //    println("------All pathes size: " + allpaths.size)
    var selectedPaths: Set[Stack[Vertex]] = Set() //这里用于存储筛选掉之后的所有path

    allpaths.map(ite => {
      var containPattern = false
      ite.map(itN => {

        if (itN.reactionRules.size > 0) {
          println("itN.reactionRules: " + itN.reactionRules)
          itN.reactionRules.map { rr =>
            {
              if (PatternFlow.patternDefRules.size > 0 && PatternFlow.patternDefRules.contains(rr.name)) { containPattern = true; selectedPaths.+=(ite) }
              if (PatternFlow.patternPUseRules.size > 0 && PatternFlow.patternPUseRules.contains(rr.name)) { containPattern = true; selectedPaths.+=(ite) }
              if (PatternFlow.patternCUseRules.size > 0 && PatternFlow.patternCUseRules.contains(rr.name)) { containPattern = true; selectedPaths.+=(ite) }
            }
          }
        }

        if (itN.reactionRule != null) {
          println("itN.reactionRule: " + itN.reactionRule)
          if (PatternFlow.patternDefRules.size > 0 && PatternFlow.patternDefRules.contains(itN.reactionRule.name)) { containPattern = true; selectedPaths.+=(ite) }
          if (PatternFlow.patternPUseRules.size > 0 && PatternFlow.patternPUseRules.contains(itN.reactionRule.name)) { containPattern = true; selectedPaths.+=(ite) }
          if (PatternFlow.patternCUseRules.size > 0 && PatternFlow.patternCUseRules.contains(itN.reactionRule.name)) { containPattern = true; selectedPaths.+=(ite) }
        }
      })
      if (containPattern) {
        ite.map { x =>
          {
            x.visited = true
          }
        }
      }
    })

    println("selectedPaths: " + selectedPaths)
    selectedPaths
  }

  //add by lbj 获得模拟结果路径集
  def getPathSet: Set[Stack[Vertex]] = {
    var allPathSet: Set[Stack[Vertex]] = Set()
    var allPathString: Set[String] = Set()
    root.terminal = true
    getPathString("", allPathString, root) //调用后把结果都存在allPathString里面

    allPathString.map(path => {
      var p: Stack[Vertex] = Stack()
      var Vertexs = path.split("#")
      for (i <- 0 to Vertexs.size - 1) {
        if (!Vertexs(Vertexs.size - 1 - i).contains("r_")) { //反应规则必须以r_开头，要不这里逻辑走不通
          var Vertex = lut.getOrElse(Vertexs(Vertexs.size - 1 - i).toInt, null) //查不到就置为空，主要看lut
          if (Vertex != null)
            p.push(Vertex)
          else
            println("error!")
        }
      }
      allPathSet.add(p)

    })

    allPathSet

  }

  //add by lbj
  def getPathString(path: String, allPathString: Set[String], currentVertex: Vertex) { //调用时传入的是root，这个方法主要看target
    if (currentVertex.terminal && !firstTime) { //为终点且firstTime=false时才会退出这个方法

      println("allPathString: " + path + currentVertex.hash);
      allPathString.add(path + currentVertex.hash)
      return
    } else { //不为终点或firstTime=true时
      firstTime = false
      if (currentVertex.target != null && currentVertex.target.size > 0) {
        currentVertex.target.map(target => {
          if (!path.contains(currentVertex.hash + "#" + target._2.name + "#" + target._1.hash)) {
            getPathString(path + currentVertex.hash + "#" + target._2.name + "#", allPathString, target._1) //递归，自己调用自己
          }
        })
      } else if (currentVertex.targets != null && currentVertex.targets.size > 0) { //考虑规则并发反应
        currentVertex.targets.map(targets => {
          var rrs: String = "";
          if (targets._2.size > 0) {
            var tarArry = targets._2.toArray
            rrs = tarArry(0).name.toString()
            for (i <- 1 to tarArry.size - 1) {
              rrs += "," + tarArry(i).name.toString() //逗号分隔当前并发反应的规则
            }
            if (!path.contains(currentVertex.hash + "#" + rrs + "#" + targets._1.hash)) {
              getPathString(path + currentVertex.hash + "#" + rrs + "#", allPathString, targets._1) //递归，自己调用自己
            }
          }
        })
      }
    }

  }

  def dumpDotForward: String = {
    //if (GlobalCfg.graphOutput == "") return "";
    var out: String = "";
    out += "digraph reaction_graph {\n";
    out += "   rankdir=LR;\n";
    out += "   Node [shape = circle];\n";
    out += "   BigSim_Report [shape = parallelogram color = aliceblue style=filled label=\"BigSim\nReport\"];\n"
    out += "BigSim_Report -> N_" + formatHash(root.hash) + "[color = aliceblue label = \"";
    if (!Data.getWeightExpr.equals("wExpr=")) //读取agent的权重表达式，输出模拟报告 表示权重表达式不为空，即有权重表达式
      out += Data.getWeightExpr + "=" + Data.getReport + "\n";
    out += Data.getValues(",") + "\"];\n";
    out += " N_" + formatHash(root.hash) + "\n" + " [shape=circle, color=lightblue2, style=filled];\n";
    lut.values.map(x => {
      var rr: String = "root";
      var dc: String = "";

      if (x.terminal) {
        dc = "shape = doublecircle, color=lightblue2, style=filled, ";
      }
      //out += "N_" + formatHash(x.hash) + "[ " + dc + "label=\"N_" + formatHash(x.hash) + "\n" + x.variables + "\"];\n";//data太多了不输出了
      out += "N_" + formatHash(x.hash) + "[ " + dc + "label=\"N_" + formatHash(x.hash) + "\"];\n"; //输出Bigraph模型的HashCode作为名字
      x.target.map(y => {
        rr = "?";
        if (y._2 != null)
          rr = y._2.name;

        if (y._1 != null) {
          rr = rr + "\nSystem Clock: " + y._1.sysClk
          if (GlobalCfg.checkData && y._2.conds.size != 0)
            rr = rr + "\nCond:" + y._2.getConds
          if (GlobalCfg.checkHMM && y._2.hmms.size != 0)
            rr = rr + "\nHMM:" + y._2.getHMM
          out += " N_" + formatHash(x.hash) + " -> N_" + formatHash(y._1.hash) + "[ label = \"" + rr + "\"];\n"
        }
      });

    });
    out += "}\n";
    if (GlobalCfg.graphOutput != "") {
      var file: File = new File(GlobalCfg.graphOutput);
      var writer: Writer = new FileWriter(file);
      writer.write(out);
      writer.flush;
    }
    out;
  }

  //add by lbj
  def dumpDotFile(): String = {
    Graph.loopNum += 1
    var out: String = "";
    out = "digraph reaction_graph {\n";
    out += "   rankdir=LR;\n";
    out += "   Node [shape = circle];\n";
    out += "   BigSim_Report [shape = parallelogram color = aliceblue style=filled label=\"BigSim\nReport\"];\n"
    out += "BigSim_Report -> N_" + formatHash(root.hash) + "[color = aliceblue label = \"";
    if (!Data.getWeightExpr.equals("wExpr=")) //读取agent的权重表达式，输出模拟报告 表示权重表达式不为空，即有权重表达式
      out += Data.getWeightExpr + "=" + Data.getReport + "\n"; //有权重表达式才输出到图上
    out += "\"];\n";
    out += " N_" + formatHash(root.hash) + "\n" + " [shape=circle, color=lightblue2, style=filled];\n";
    if (Graph.loopNum == 1) {
      Graph.dot = out
    }

    getConcurrenceRRs
    lut.values.map(x => if (!GlobalCfg.checkPattern || (GlobalCfg.checkPattern && x.visited)) {
      var rr: String = "root";
      var dc: String = "";

      if (!x.terminal && x.violateRecordTracking) { //异常比记录优先级高，若后面发生异常则覆盖记录
        dc = "shape = circle, color=yellow, style=filled, ";
      }

      if (!x.terminal && GlobalCfg.checkPattern) { //终止节点的pattern flow就显示正常的蓝色
        if (x.reactionRules.size > 0) {
          x.reactionRules.map { y =>
            if (PatternFlow.patternDefRules.contains(y.name)) { dc = "shape = circle, color=green, style=filled, "; }
            if (PatternFlow.patternCUseRules.contains(y.name)) {
              dc = "shape = circle, color=orange, style=filled, ";
            } //cuse显示橙色
            if (PatternFlow.patternPUseRules.contains(y.name)) {
              dc = "shape = circle, color=pink, style=filled, ";
            } //puse显示粉色
          }
        }

        if (x.reactionRule != null) {
          if (PatternFlow.patternDefRules.contains(x.reactionRule.name)) { dc = "shape = circle, color=green, style=filled, "; }
          if (PatternFlow.patternCUseRules.contains(x.reactionRule.name)) {
            dc = "shape = circle, color=orange, style=filled, ";
          } //cuse显示橙色
          if (PatternFlow.patternPUseRules.contains(x.reactionRule.name)) {
            dc = "shape = circle, color=pink, style=filled, ";
          } //puse显示粉色
        }
      }

      //         println("GlobalCfg.violateSorting: "+GlobalCfg.violateSorting)
      //        if(x.terminal && x.sysClk>0 && GlobalCfg.violateSorting){//异常比记录优先级高，若后面发生异常则覆盖记录        
      //          dc = "shape = doublecircle, color=red, style=filled, ";
      //       }

      if (x.terminal && x.sysClk > 0) { //sysClk为0的terminal也会为true所以起始点被画圈了
        if (GlobalCfg.violateExceptionTracking || GlobalCfg.violateBinding || GlobalCfg.violateSorting) { //异常终止
          dc = "shape = doublecircle, color=red, style=filled, ";
        } else { //正常终止
          dc = "shape = doublecircle, color=lightblue2, style=filled, ";
        }
      } else if (x.terminal && GlobalCfg.SimulatorClass.startsWith("Enum")) {
        if (GlobalCfg.violateExceptionTracking || GlobalCfg.violateBinding || GlobalCfg.violateSorting) {
          dc = "shape = doublecircle, color=red, style=filled, ";
        } else {
          dc = "shape = doublecircle, color=lightblue2, style=filled, ";
        }
      }

      out += "N_" + formatHash(x.hash) + "[ " + dc + "label=\"N_" + formatHash(x.hash) + "\"];\n"; //输出Bigraph模型的HashCode作为名字
      Graph.dot += "N_" + formatHash(x.hash) + "[ " + dc + "label=\"N_" + formatHash(x.hash) + "\"];\n";

      if (x.target != null && x.target.size > 0) { //add by lbj 单条反应
        x.target.map(y => {
          rr = "?";
          if (y._2 != null) {
            rr = y._2.name;
            if (GlobalCfg.checkPattern) {
              if (PatternFlow.patternDefRules.contains(rr)) {
                var defStr = "(define: " + root.bigraph.pattern + ")";
                rr = rr + " " + defStr
              }
              if (PatternFlow.patternCUseRules.contains(rr)) {
                var cuseStr = "(cuse: " + root.bigraph.pattern + ")";
                rr = rr + " " + cuseStr
              }
              if (PatternFlow.patternPUseRules.contains(rr)) {
                var puseStr = "(puse: " + root.bigraph.pattern + ")";
                rr = rr + " " + puseStr
              }
            }
            if (!x.terminal && x.violateRecordTracking) {
              var vioStr = "(violate: Tracking Record Assertion)";
              rr = rr + " " + vioStr
            }
            if (x.terminal && GlobalCfg.violateExceptionTracking) {
              var vioStr = "(violate: Tracking Exception Assertion)";
              rr = rr + " " + vioStr
            }
            if (x.terminal && GlobalCfg.violateBinding) {
              var vioStr = "(violate: Tracking Binding Constraint)";
              rr = rr + " " + vioStr
            }
            if (x.terminal && GlobalCfg.violateSorting) {
              var vioStr = "(violate: Tracking Sorting Constraint)";
              rr = rr + " " + vioStr
            }
          }
          if (y._1 != null) {
            if (GlobalCfg.checkTime)
              rr = rr + "\nSystem Clock: " + y._1.sysClk
            if (GlobalCfg.checkData && y._2.conds.size != 0)
              rr = rr + "\nCond:" + y._2.getConds
            if (GlobalCfg.checkHMM && y._2.hmms.size != 0)
              rr = rr + "\nHMM:" + y._2.getHMM
            out += " N_" + formatHash(x.hash) + " -> N_" + formatHash(y._1.hash) + "[ color = purple  label = \"" + rr + "\"];\n"
            Graph.dot += " N_" + formatHash(x.hash) + " -> N_" + formatHash(y._1.hash) + "[ color = " + pathColor + " label = \"" + rr + "\"];\n"
          }
        });
      } else if (x.targets != null && x.targets.size > 0) { //并发反应
        x.targets.map(y => {
          var rrs: String = "";

          if (y._1 != null) {
            rrs = "System Clock: " + y._1.sysClk + "\n"

            if (y._2 != null && y._2.size > 0) {
              var tarArry = y._2.toArray
              rrs = rrs + tarArry(0).name.toString()
              if (GlobalCfg.checkPattern) {
                if (PatternFlow.patternDefRules.contains(tarArry(0).name)) {
                  var defStr = "(define: " + root.bigraph.pattern + ")";
                  rrs = rrs + " " + defStr
                }
                if (PatternFlow.patternCUseRules.contains(tarArry(0).name)) {
                  var cuseStr = "(cuse: " + root.bigraph.pattern + ")";
                  rrs = rrs + " " + cuseStr
                }
                if (PatternFlow.patternPUseRules.contains(tarArry(0).name)) {
                  var puseStr = "(puse: " + root.bigraph.pattern + ")";
                  rrs = rrs + " " + puseStr
                }
              }
              if (!x.terminal && x.violateRecordTracking) {
                var vioStr = "(violate: Tracking Record Assertion)";
                rrs = rrs + " " + vioStr
              }
              if (x.terminal && GlobalCfg.violateExceptionTracking) {
                var vioStr = "(violate: Tracking Exception Assertion)";
                rrs = rrs + " " + vioStr
              }
              if (x.terminal && GlobalCfg.violateBinding) {
                var vioStr = "(violate: Tracking Binding Constraint)";
                rrs = rrs + " " + vioStr
              }
              if (x.terminal && GlobalCfg.violateSorting) {
                var vioStr = "(violate: Tracking Sorting Constraint)";
                rrs = rrs + " " + vioStr
              }
              for (i <- 1 to tarArry.size - 1) {
                rrs += "," + tarArry(i).name.toString() //逗号分隔当前并发反应的规则

                if (GlobalCfg.checkPattern) {
                  if (PatternFlow.patternDefRules.contains(tarArry(i).name)) {
                    var defStr = "(define: " + root.bigraph.pattern + ")";
                    rrs = rrs + " " + defStr
                  }
                  if (PatternFlow.patternCUseRules.contains(tarArry(i).name)) {
                    var cuseStr = "(cuse: " + root.bigraph.pattern + ")";
                    rrs = rrs + " " + cuseStr
                  }
                  if (PatternFlow.patternPUseRules.contains(tarArry(i).name)) {
                    var puseStr = "(puse: " + root.bigraph.pattern + ")";
                    rrs = rrs + " " + puseStr
                  }
                }
                if (!x.terminal && x.violateRecordTracking) {
                  var vioStr = "(violate: Tracking Record Assertion)";
                  rrs = rrs + " " + vioStr
                }
                if (x.terminal && GlobalCfg.violateExceptionTracking) {
                  var vioStr = "(violate: Tracking Exception Assertion)";
                  rrs = rrs + " " + vioStr
                }
                if (x.terminal && GlobalCfg.violateBinding) {
                  var vioStr = "(violate: Tracking Binding Constraint)";
                  rrs = rrs + " " + vioStr
                }
                if (x.terminal && GlobalCfg.violateSorting) {
                  var vioStr = "(violate: Tracking Sorting Constraint)";
                  rrs = rrs + " " + vioStr
                }
              }
            }
            Graph.dot += " N_" + formatHash(x.hash) + " -> N_" + formatHash(y._1.hash) + "[ color = " + pathColor + " label = \"" + rrs + "\"];\n"
          }
        });
      }

    });
    println("Graph.dot: " + Graph.dot)

    if (Graph.loopNum == GlobalCfg.simLoop && GlobalCfg.outputGraph) { //最后一次循环完再输出结果
      var file: File = new File(GlobalCfg.graphOutput);
      var writer: Writer = new FileWriter(file);
      if (GlobalCfg.SimulatorClass.startsWith("Enum")) { //若为枚举，输出最后一次Loop结果即可
        out += "}\n";
        if(!GlobalCfg.IsFromNetWork)
          writer.write(out); //out是最后一次loop的结果
        else {
          GlobalCfg.dotContent = out;
        }
      } else {
        Graph.dot += "}\n";
         if(!GlobalCfg.IsFromNetWork)
        writer.write(Graph.dot); //Graph.dot是所有loop的结果拼起来
         else
          GlobalCfg.dotContent = Graph.dot;
      }
      writer.flush;
      writer.close();
    }
    out
  }

  def getConcurrenceRRs = {
    lut.values.map(x => {
      if (x.terminal && x.parents.size > 0 && !x.parents.contains(null)) {
        addConcurrenceRRs(x)
      }
    });
  }

  def addConcurrenceRRs(v: Vertex) {
    if (!v.parents.contains(null) && v.parents.size > 0) {
      var parentArr: Array[Vertex] = new Array[Vertex](v.parents.size)
      parentArr = v.parents.toArray
      for (i <- 0 to parentArr.size - 1) {
        var lineSpendTime: Double = v.sysClk - parentArr(i).sysClk;
        parentArr(i).targets.map(y => {
          y._2.map(r => {
            if (r.sysClkIncr > lineSpendTime) {
              println("pathNum: " + parentArr(i).parent.getTargetsRRs(parentArr(i)).size)
              var newRRs: Set[ReactionRule] = parentArr(i).parent.getTargetsRRs(parentArr(i)).+=(r)
              println("pathNum: " + newRRs.size)
              parentArr(i).parent.addTargets(newRRs, parentArr(i))
              r.sysClkIncr = r.sysClkIncr - lineSpendTime.intValue()
            }
          });
        });

        addConcurrenceRRs(parentArr(i))
      }
    }

  }

}
