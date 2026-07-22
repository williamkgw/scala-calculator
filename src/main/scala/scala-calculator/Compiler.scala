package scala_calculator

import scala.util.matching.Regex
import ExpressionOperations.*
sealed trait Token

case class Number(value: Int) extends Token
case class Operator(op: ExpressionOperations) extends Token
case object RightParen extends Token
case object LeftParen extends Token

object Tokenizer {
  private val tokenRegex: Regex = """\d+|[()+\-*/]""".r
  private val number = raw"(\d+)".r

  def splitter(expression: String): List[String] =
    tokenRegex.findAllIn(expression).toList

  def matchSplit(token: String): Token =
    token match
      case "("         => LeftParen
      case ")"         => RightParen
      case "+"         => Operator(Sum)
      case "-"         => Operator(Sub)
      case "*"         => Operator(Mult)
      case "/"         => Operator(Div)
      case number(n)   => Number(n.toInt)
      case _           => throw new IllegalArgumentException(s"Unknown token: $token")

  def tokenize(expression: String): List[Token] =
    val splitted = splitter(expression)
    splitted.map {
        split => matchSplit(split)
    }
}

