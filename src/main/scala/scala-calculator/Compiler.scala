package scala_calculator
import scala.util.matching.Regex

// Calculator expression compiler design
//
// =========================================
//
// String
// |
// |
// Tokenizer
// |
// |
// List[Token]
// |
// |
// Parser
// |
// |
// Expression (AST)
// |
// |
// process()
// |
// |
// Int: result
//
// =========================================

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

sealed trait Tree
case class Leaf(value: Number) extends Tree
case class Branch(
  middle: Token,
  left: Tree,
  right: Tree
) extends Tree

case class ProcessingTree(exp: List[Token], acc: List[Token] = List(), tree: Option[Tree] = None)

object AbstractSyntaxTreeParser {
  def parseExpression(exp: List[Token] ): (Tree, List[Token]) =
    val (left, rest) = parseTerm(exp)

    rest match {
      case (op @ Operator(Sum | Sub)) :: tail =>
        val (right, remaining) = parseExpression(tail)
        (Branch(op, left, right), remaining)
      case _ =>
        (left, rest)
    }

  def parseTerm(exp: List[Token]): (Tree, List[Token]) =
    val (left, rest) = parseFactor(exp)

    rest match {
      case (op @ Operator(Mult | Div)) :: tail =>
        val (right, remaining) = parseTerm(tail)
        (Branch(op, left, right), remaining)
      case _ =>
        (left, rest)
    }

  def parseFactor(exp: List[Token]): (Tree, List[Token]) =
    exp match {
      case Number(n) :: rest =>
        return (Leaf(Number(n)), rest)

      case LeftParen :: rest =>
        val (expr, remaining) = parseExpression(rest)

        remaining match
          case RightParen :: tail =>
            (expr, tail)
          case _ =>
              throw new IllegalArgumentException(s"Expected $RightParen")

      case _ =>
        throw new IllegalArgumentException("Expected a factor")
    }
}

