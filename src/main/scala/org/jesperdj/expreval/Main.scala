package org.jesperdj.expreval

object Main extends App {
  val parser = new ExpressionParser

  def evaluator(expr: Expression)(values: String => Double): Double = {
    def eval(e: Expression): Double = e match {
      case Literal(value) => value
      case Variable(name) => values(name)
      case Add(x, y) => eval(x) + eval(y)
      case Subtract(x, y) => eval(x) - eval(y)
      case Multiply(x, y) => eval(x) * eval(y)
      case Divide(x, y) => eval(x) / eval(y)
    }

    eval(expr)
  }

  def rpn(expr: Expression): List[String] = expr match {
    case Literal(value) => List(value.toString)
    case Variable(name) => List(name)
    case Add(x, y) => rpn(x) ++ rpn(y) :+ "+"
    case Subtract(x, y) => rpn(x) ++ rpn(y) :+ "-"
    case Multiply(x, y) => rpn(x) ++ rpn(y) :+ "*"
    case Divide(x, y) => rpn(x) ++ rpn(y) :+ "/"
  }

  // val input = "1 + (2 - (3 / (4 * x)))"
  val input = "1 + (2 - 3 / 4) * x"

  val expr = parser.parse(input).get
  println(expr)

  val evaluate = evaluator(expr) _
  val result = evaluate(Map("x" -> 4.0))
  println(result)

  println(rpn(expr).mkString(" "))
}
