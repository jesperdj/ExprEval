package org.jesperdj.expreval

sealed trait Expression

sealed trait Terminal extends Expression

case class Literal(value: Double) extends Terminal

case class Variable(name: String) extends Terminal

sealed trait BinaryExpression extends Expression {
  def left: Expression
  def right: Expression
}

case class Add(left: Expression, right: Expression) extends BinaryExpression

case class Subtract(left: Expression, right: Expression) extends BinaryExpression

case class Multiply(left: Expression, right: Expression) extends BinaryExpression

case class Divide(left: Expression, right: Expression) extends BinaryExpression
