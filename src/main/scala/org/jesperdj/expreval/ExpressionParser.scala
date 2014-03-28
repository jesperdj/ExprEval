package org.jesperdj.expreval

import scala.util.parsing.combinator.RegexParsers

class ExpressionParser extends RegexParsers {

  // A literal is a number consisting of an optional minus sign, one or more digits and optionally a decimal point
  // followed by one or more digits.
  // We use a regex here, which is implicitly converted to a Parser[String], then we transform that using ^^
  // to a Parser[Literal].
  def literal: Parser[Literal] =
    """-?\d+(\.\d+)?""".r ^^ { s => Literal(s.toDouble) }

  // A variable name starts with a letter (lower or upper case) followed by zero or more word characters.
  // We use ^^ to transform the result of the regex to a Variable object. Note the short-hand syntax,
  // we could also have written: ... ^^ { s => Variable(s) }
  def variable: Parser[Variable] =
    """[a-zA-Z]\w*""".r ^^ Variable

  // A terminal is a literal or a variable.
  def terminal: Parser[Terminal] =
    literal | variable

  // A termOrExpr is a terminal or an expression between parentheses. We use ~> and <~ here, because we don't want
  // the parentheses themselves to be included in the parse result.
  def termOrExpr: Parser[Expression] =
    terminal | "(" ~> expression <~ ")"

  // A productExpr is a termOrExpr followed by a repetition (zero or more times) of either: "*" followed by a
  // termOrExpr, or "/" followed by a termOrExpr.
  // Note that the rep(...) is translated into a List[Expression].
  def productExpr: Parser[Expression] =
    termOrExpr ~ rep("*" ~ termOrExpr | "/" ~ termOrExpr) ^^ {
      case e ~ list => list.foldLeft(e) {
        case (x, "*" ~ y) => Multiply(x, y)
        case (x, "/" ~ y) => Divide(x, y)
      }
    }

  // Similar to productExpr.
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
