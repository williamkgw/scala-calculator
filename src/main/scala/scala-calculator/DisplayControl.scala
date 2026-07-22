package scala_calculator

enum ExpressionOperations {
  case Sum, Sub, Mult, Div
}

sealed trait Expression:
  def process(): Int

case class Value(value: Int) extends Expression {
  def process() = value
}

case class Operation(
  left: Expression,
  right: Expression,
  op: ExpressionOperations
) extends Expression {
  import ExpressionOperations.*

  def process() = op match
    case Sum => left.process() + right.process()
    case Sub => left.process() - right.process()
    case Mult => left.process() * right.process()
    case Div => left.process() / right.process()
}

