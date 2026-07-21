package scala_calculator

enum ExpressionOperations {
  case Sum, Sub, Mult, Div
}

sealed trait Expression:
  def process(): Int

case class Value(value: Int) extends Expression {
  def process() = value
}

//
// (((1 + 2) + 3) + ((10 + 20) + 30))
//
// val inOp = Operation(
//   Operation(Operation(Value(1), Value(2), Sum), Value(3), Sum),
//   Operation(Operation(Value(10), Value(20), Sum), Value(30), Sum),
//   Sum
// )
//
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

