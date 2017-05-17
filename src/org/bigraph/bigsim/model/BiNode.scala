package org.bigraph.bigsim.model

/**
 * @author amy
 */

object BiNode{
    def GetHead: BiNode = {
    var res: BiNode = null;
    return res;
  }
  
}

<<<<<<< HEAD
class BiNode(b: Bigraph, childPair: Map[Int, Int]) {
  var bigraph: Bigraph = b;
  var childPair: Map[Int, Int] = null
=======
class TSPair{
  var T: ReactionRule = null; //trans
  var S: BiNode = null;  //state
  var pid: String = null; //this trans belongs which process
}


class BiNode(b: Bigraph, child: TSPair) {
  var bigraph: Bigraph = b;
>>>>>>> 207ff948d72d13445ec5c8a31e93c6dc86abf4ed
  var hash: Int = {//根据当前偶图的root生成唯一的hashCode
    if (bigraph.root != null)
      bigraph.root.toString.hashCode();
    else "".hashCode();
  }
  
  def GetChild: List[TSPair] = {
    return null;
  }
<<<<<<< HEAD
=======
}

class TSPair{
  var T: ReactionRule = null; //trans
  var S: BiNode = null;  //state
  var pid: String = ""; //this trans belongs which process
>>>>>>> 2d1ee42e7a27809a6d7e7cdccf52caca2390c0de
}