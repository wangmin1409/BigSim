package org.bigraph.bigsim.parser

import scala.io.Source
import scala.Array.canBuildFrom

object HMM {
  var hmms: Map[String, HMM] = Map()

  def constructHMM(n: String, p: List[Double], tr: Int, t: List[Double], cr: Int, c: List[Double], o: List[Int]) {
    var transMatrix: Array[Array[Double]] = Array.ofDim[Double](tr, t.size / tr)
    var confuMatrix: Array[Array[Double]] = Array.ofDim[Double](cr, c.size / cr)
    var index = 0
    for (i <- 0 to tr - 1)
      for (j <- 0 to t.size / tr - 1) {
        transMatrix(i)(j) = t(index)
        index += 1
      }
    index = 0
    for (i <- 0 to cr - 1)
      for (j <- 0 to c.size / cr - 1) {
        confuMatrix(i)(j) = c(index)
        index += 1
      }
    val hmm: HMM = new HMM(n, p.toArray, transMatrix, confuMatrix, o.toArray)
    hmms += n -> hmm
  }

  def parseHMM(path: String) {
    var file = Source.fromFile(path)
    var tag: String = ""

    var name: String = ""
    var pi: List[Double] = List()
    var transRows: Int = 0
    var transList: List[Double] = List()
    var confuRows: Int = 0
    var confuList: List[Double] = List()
    var obsList: List[Int] = List()

    for (line <- Source.fromFile(path).getLines) {
      if (line.trim().equals("Name:")) {
        if (!name.equals("")) {
          constructHMM(name, pi, transRows, transList, confuRows, confuList, obsList)
          pi = List()
          transRows = 0
          transList = List()
          confuRows = 0
          confuList = List()
          obsList = List()
        }
        tag = "name"
      } else if (line.trim().equals("Pi:")) {
        tag = "pi"
      } else if (line.trim().equals("A:")) {
        tag = "trans"
      } else if (line.trim().equals("B:")) {
        tag = "confu"
      } else if (line.trim().equals("I:")) {
        tag = "obs"
      } else if (!line.trim().equals("")) {
        if (tag.equals("name")) {
          name = line.trim()
        } else if (tag.equals("pi")) {
          line.trim().split("\t").map(item => { pi = pi.:+(item.toDouble) })
        } else if (tag.equals("obs")) {
          line.trim().split("\t").map(item => { obsList = obsList.:+(item.toInt) })
        } else if (tag.equals("trans")) {
          transRows += 1
          line.trim().split("\t").map(item => { transList = transList.:+(item.toDouble) })
        } else if (tag.equals("confu")) {
          confuRows += 1
          line.trim().split("\t").map(item => { confuList = confuList.:+(item.toDouble) })
        }
      }
    }

    if (!name.equals("")) {
      constructHMM(name, pi, transRows, transList, confuRows, confuList, obsList)
    }
  }
}

class HMM(n: String, p: Array[Double], t: Array[Array[Double]], c: Array[Array[Double]], o: Array[Int]) {
  val name = n
  val pi = p
  val transMatrix = t
  val confuMatrix = c
  var obs = o

  /**
   * Get the probability of an observed sequence.
   * @Param obs: the observed sequence.
   * @Return probability.
   */
  def getObsProbability: Double = {
    var result: Double = 0
    for (i <- 0 to pi.length - 1) {
      result += calcForward(i, obs.length - 1, obs)
      //println("middle result:" + result)
    }
    //println("result:" + result)
    result
  }

  /**
   * HMM forward algorithm
   * curState: current latent state
   * curObsIndex: current observation
   * ob: observation sequence
   * curObs: ob(curObsIndex)
   * initialize p(x0) = pi(initialState) *
   * transition matrix
   * confusion matrix
   */
  def calcForward(curState: Int, curObsIndex: Int, obs: Array[Int]): Double = {
    if (curObsIndex == 0) {
      /**
       * When it is the initial state, the P equals P()
       */
      //println(pi(curState) * confuMatrix(curState)(obs(curObsIndex)))
      return pi(curState) * confuMatrix(curState)(obs(curObsIndex))
    } else {
      var result: Double = 0
      for (i <- 0 to pi.length - 1) {
        // current state: curState, prv state: i
        var a = calcForward(i, curObsIndex - 1, obs)
        result += transMatrix(i)(curState) * a
        //println(transMatrix(i)(curState) + " * " + a)
      }
      //println(curState)
      //println("--" + result + "*" + confuMatrix(curState)(obs(curObsIndex)) + "=" + result * confuMatrix(curState)(obs(curObsIndex)))
      return result * confuMatrix(curState)(obs(curObsIndex))
    }
  }
}

object testHMM {
  def main(args: Array[String]) {
    HMM.parseHMM("Examples/MobileCloud/hmm/museum.hmm")
    println(HMM.hmms("Signal").getObsProbability)
    println(HMM.hmms("weather").getObsProbability)
  }
}
  
  