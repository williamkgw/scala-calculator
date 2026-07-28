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

    def loopExpression(acc: Tree, exp: List[Token]): (Tree, List[Token]) =
      exp match {
        case (op @ Operator(Sum | Sub)) :: tail =>
          val (right, remaining) = parseTerm(tail)
          val newAcc = Branch(op, acc, right)
          loopExpression(newAcc, remaining)
        case _ =>
          (acc, exp)
      }

    loopExpression(left, rest)

  def parseTerm(exp: List[Token]): (Tree, List[Token]) =
    val (left, rest) = parseFactor(exp)

    def loopTerm(acc: Tree, exp: List[Token]): (Tree, List[Token]) =
      exp match {
        case (op @ Operator(Mult | Div)) :: tail =>
          val (right, remaining) = parseFactor(tail)
          val newAcc = Branch(op, acc, right)
          loopTerm(newAcc, remaining)

        case _ =>
          (acc, exp)
      }

    loopTerm(left, rest)

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

object Evaluator {
  def calculate(middle: Token, left: Int, right: Int): Int =
    middle match {
      case Operator(Mult) => left * right
      case Operator(Div) => left / right
      case Operator(Sum) => left + right
      case Operator(Sub) => left - right
      case x => throw new IllegalArgumentException(s"Expected Operator(Mult | Div | Sum | Sub) received: $x")
    }

  def evaluate(ast: Tree): (Int) =
    ast match {
      case Branch(middle, left, right) =>
        val evaluateLeft = evaluate(left)
        val evaluateRight = evaluate(right)
        calculate(middle, evaluateLeft, evaluateRight)

      case Leaf(Number(n)) =>
        n
    }
}

