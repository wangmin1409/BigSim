package org.bigraph.bigsim.utils

import java.util.Properties
import java.io._

object GlobalCfg {

  val prop: Properties = new Properties

  try {
    prop.load(new FileInputStream("config.properties"))
  } catch {
    case e: Exception =>
      e.printStackTrace
      sys.exit(1)
  }

  /**
   * For condition configurations
   */
  val condPrefStr: String = prop.getProperty("condPrefStr")
  val exprPrefStr: String = prop.getProperty("exprPrefStr")
  val sysClkPrefStr: String = prop.getProperty("sysClkPrefStr")
  val randomPrefStr: String = prop.getProperty("randomPrefStr")
  val wExprPrefStr: String = prop.getProperty("wExprPrefStr")
  val hmmPrefStr: String = prop.getProperty("hmmPrefStr")
  val ratePrefStr: String = prop.getProperty("ratePrefStr")
  val probabilityPrefStr: String = prop.getProperty("probabilityPrefStr")
  val reversePrefStr: String = prop.getProperty("reversePrefStr")
  val reactPrefStr: String = prop.getProperty("reactPrefStr")
  val minProbability: Double = prop.getProperty("minProbability").toDouble

  val SimulatorClass: String = prop.getProperty("SimulatorClass")

  var DEBUG: Boolean = prop.getProperty("DEBUG").toBoolean
  var checkLocal: Boolean = prop.getProperty("checkLocal").toBoolean
  var maxSteps: Long = prop.getProperty("maxSteps").toLong
  var reportInterval: Long = prop.getProperty("reportInterval").toLong
  var printMode: Boolean = prop.getProperty("printMode").toBoolean
  var ranNameIndex: Int = prop.getProperty("ranNameIndex").toInt

  /**
   *  set inputs: models, data, hmm
   */
  var fileSeparator: String = prop.getProperty("fileSeparator")
  var inputPath: String = prop.getProperty("inputPath")
  var modelName: String = prop.getProperty("modelName")
  var filename: String = inputPath + fileSeparator + "models" + fileSeparator + modelName + ".bgm"
  // whether check data
  var checkData: Boolean = prop.getProperty("checkData").toBoolean
  var dataInput: String = {
    if (checkData) inputPath + fileSeparator + "data" + fileSeparator + modelName + ".data"
    else ""
  }
  
  var bdDataInput: String = {//add by lbj back derivation init
    inputPath + fileSeparator + "data" + fileSeparator + modelName + ".bd"
  }
  // whether check HMM
  var checkHMM: Boolean = prop.getProperty("checkHMM").toBoolean
  var hmmInput: String = {
    if (checkHMM) inputPath + fileSeparator + "hmm" + fileSeparator + modelName + ".hmm"
    else ""
  }
//  // whether check sorting
//  var checkSorting: Boolean = prop.getProperty("checkSorting").toBoolean
//  var sortingInput: String = {
//    if (checkSorting) inputPath + fileSeparator + "sortings" + fileSeparator + modelName + ".xml"
//    else ""
//  }
  // whether check interest pattern
  var checkInterestPattern: Boolean = prop.getProperty("checkInterestPattern").toBoolean
  var interestPatternInput: String = {
    if (checkInterestPattern) inputPath + fileSeparator + "patterns" + fileSeparator + modelName + ".xml"
    else ""
  }

  /**
   * set outputs: paths, results
   */
//  var outputPath: Boolean = prop.getProperty("outputPath").toBoolean
  var outputPath: Boolean = true //path文件默认都输出
  var pathOutput: String = {
    if (outputPath) inputPath + fileSeparator + "paths" + fileSeparator + modelName + ".path"
    else ""
  }
//  var outputGraph: Boolean = prop.getProperty("outputGraph").toBoolean
  var outputGraph: Boolean = true //dot文件默认都输出
  var graphOutput: String = {
    if (outputGraph) inputPath + fileSeparator + "results" + fileSeparator + modelName + ".dot"
    else ""
  }
  var outputData: Boolean = checkData && prop.getProperty("outputData").toBoolean
  var dataOutput: String = {
    if (outputData) inputPath + fileSeparator + "paths" + fileSeparator + modelName + ".data"
    else ""
  }
  
  var outputRR: Boolean = prop.getProperty("outputRR").toBoolean
  var RROutput: String = {
    if (outputRR) inputPath + fileSeparator + "paths" + fileSeparator + modelName + ".txt"
    else ""
  }
  
  // add by lbj hmm文件也这样处理，其他两个文件默认都输出  SysClk这种是否考虑在模拟器中设置Globals的全局变量  
//  var checkData : Boolean = false
  var dataFileLocation : String = inputPath + fileSeparator + "paths" + fileSeparator + modelName + ".data"
  val dataFile : File = new File(dataFileLocation)
  if(dataFile.exists()) 
    checkData = true 
  
    // add by lbj
    var checkTime : Boolean = false;//后面用程序设置要不要考虑时间
    var checkStochastic : Boolean = false;//考虑Rate
    var checkTracking : Boolean = false;//默认不打开Tracking检测
    var checkBinding : Boolean = false;//默认不打开Binding检测
    var checkPattern : Boolean = false;//默认不打开Pattern检测
    var checkSorting : Boolean = false;//默认不打开Sorting检测
    var violateExceptionTracking : Boolean = false;
    var violateBinding : Boolean = false;
    var violateSorting : Boolean = false;
    var isBackDerivation : Boolean = false;//是否正在进行回溯


  // system clock init
  var SysClk: Double = prop.getProperty("initSysClk").toDouble
  // system clock increaser
  var SysClkIncr: Double = prop.getProperty("sysClkIncr").toDouble
  // the max system clock
  var maxSysClk: Double = prop.getProperty("maxSysClk").toDouble

  def getRanNameIndex: Int = {
    ranNameIndex += 1
    ranNameIndex
  }

  // how many times of simulation
  var simLoop: Int = prop.getProperty("simLoop").toInt
  var curLoop: Int = 0;
  var append: Boolean = false

  var allDefs: Boolean = false
  var allUses: Boolean = false

  var patternFile: String = {
    if (checkInterestPattern) inputPath + fileSeparator + "patterns" + fileSeparator + modelName + ".xml"
    else ""
  }
  var defPathMapFile: String = ""

  var node: Boolean = true
  var verbose: Boolean = true //是否输出print语句，和配置文件中DEBUG的功能一样
  var printDiscovered: Boolean = false
  var localCheck: Boolean = false
  var reportFrequency: Int = 500
  var stochastic: Boolean = false
  
  
  var IsFromNetWork:Boolean = prop.getProperty("isFromNetWork").toBoolean
  var bgmContent:String = ""
  var dotContent:String = ""
}
