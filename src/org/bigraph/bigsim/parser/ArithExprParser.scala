package org.bigraph.bigsim.parser

import scala.util.parsing.combinator.RegexParsers
import org.bigraph.bigsim.data.Data

object ArithExpressionParser extends RegexParsers {

  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ { _.toDouble }
  def variable: Parser[Double] = """[a-zA-Z]+(\d*)?+(\.[a-zA-Z]*)?""".r ^^ {
    Data.getValue(_)
  }
  def factor: Parser[Double] = "abs(" ~> expr <~ ")" ^^ { Math.abs(_) } | "(" ~> expr <~ ")" | variable | number
  def term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor | "%" ~ factor) ^^ {
    case number ~ list => (number /: list) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
      case (x, "%" ~ y) => x % y
    }
  }
  def expr: Parser[Double] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case nu ~ list => list.foldLeft(nu) { // same as before, using alternate name for /:
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }

  def apply(input: String): Double = parseAll(expr, input) match {
    case Success(result, _) => result
    case failure: NoSuccess => scala.sys.error(failure.msg)
  }

  def main(args: Array[String]) {
    Data.parseData("Examples/MobileCloud/data/earthquake.txt")
    var exp = "abs(m1.power+energy*15+(0.5%500))";
    exp = "10%3"
    println(apply(exp))

  }
}