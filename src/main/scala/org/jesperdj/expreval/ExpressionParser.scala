package org.jesperdj.expreval

import scala.util.parsing.combinator.RegexParsers

class ExpressionParser extends RegexParsers {

  def literal: Parser[Literal] =
    """-?\d+(\.\d+)?""".r ^^ { s => Literal(s.toDouble) }

  def variable: Parser[Variable] =
    """[a-zA-Z]\w*""".r ^^ { s => Variable(s) }

  def terminal: Parser[Terminal] =
    literal | variable

  def termOrExpr: Parser[Expression] =
    terminal | "(" ~> expression <~ ")"

  def productExpr: Parser[Expression] =
    termOrExpr ~ rep("*" ~ termOrExpr | "/" ~ termOrExpr) ^^ {
      case e ~ list => list.foldLeft(e) {
        case (x, "*" ~ y) => Multiply(x, y)
        case (x, "/" ~ y) => Divide(x, y)
      }
    }

  def sumExpr: Parser[Expression] =
    productExpr ~ rep("+" ~ productExpr | "-" ~ productExpr) ^^ {
      case e ~ list => list.foldLeft(e) {
        case (x, "+" ~ y) => Add(x, y)
        case (x, "-" ~ y) => Subtract(x, y)
      }
    }

  def expression: Parser[Expression] = sumExpr

  def parse(input: String): ParseResult[Expression] = parseAll(expression, input)
}
