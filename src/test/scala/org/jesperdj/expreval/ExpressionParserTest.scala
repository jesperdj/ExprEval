package org.jesperdj.expreval

import org.scalatest.FunSuite

class ExpressionParserTest extends FunSuite {
  val parser = new ExpressionParser

  test("Integer literals") {
    testParse("53", Literal(53.0))
    testParse("-683", Literal(-683.0))
  }

  test("Floating-point literals") {
    testParse("72.825", Literal(72.825))
    testParse("-1.5", Literal(-1.5))
  }

  test("Literals with errors") {
    testParseError(".25")
    testParseError("42.")
    testParseError("4.5.8")
  }

  test("Variables") {
    testParse("abc", Variable("abc"))
    testParse("x0", Variable("x0"))
    testParse("Hello_World__955", Variable("Hello_World__955"))
  }

  test("Variables with errors") {
    testParseError("_")
    testParseError("_abc")
    testParseError("0x")
    testParseError("a$")
  }

  test("Products") {
    testParse("abc * 58", Multiply(Variable("abc"), Literal(58.0)))
    testParse("65.5 / 34", Divide(Literal(65.5), Literal(34.0)))
    testParse("2 / 3 * 4", Multiply(Divide(Literal(2.0), Literal(3.0)), Literal(4.0)))
    testParse("2 / (3 * 4)", Divide(Literal(2.0), Multiply(Literal(3.0), Literal(4.0))))
  }

  test("Products with errors") {
    testParseError("3 *")
    testParseError("/ 5")
  }

  test("Sums") {
    testParse("xy + ab", Add(Variable("xy"), Variable("ab")))
    testParse("5 - 3 - 6", Subtract(Subtract(Literal(5.0), Literal(3.0)), Literal(6.0)))
    testParse("5 - (3 - 6)", Subtract(Literal(5.0), Subtract(Literal(3.0), Literal(6.0))))
    testParse("((1 + 2) + 3) + 4", Add(Add(Add(Literal(1.0), Literal(2.0)), Literal(3.0)), Literal(4.0)))
    testParse("1 + (2 + (3 + 4))", Add(Literal(1.0), Add(Literal(2.0), Add(Literal(3.0), Literal(4.0)))))
  }

  test("Sums with errors") {
    testParseError("+ 77")
    testParseError("32 -")
  }

  test("Miscellaneous expressions") {
    testParse("2 + 3 * 5 - 4", Subtract(Add(Literal(2.0), Multiply(Literal(3.0), Literal(5.0))), Literal(4.0)))
    testParse("2 * (3 + 5)", Multiply(Literal(2.0), Add(Literal(3.0), Literal(5.0))))
    testParse("(3 + 5) * 2", Multiply(Add(Literal(3.0), Literal(5.0)), Literal(2.0)))
  }

  test("Miscellaneous expressions with errors") {
    testParseError("")
    testParseError("()")
    testParseError("3 * + 4")
  }

  def testParse(input: String, expected: Expression) {
    parser.parse(input) match {
      case result: parser.Success[_] => assert(result.get == expected)
      case result: parser.NoSuccess  => fail(result.toString)
    }
  }

  def testParseError(input: String) {
    parser.parse(input) match {
      case result: parser.Success[_] => fail(s"Expected parsing to fail for: $input")
      case result: parser.NoSuccess  => // Expected result
    }
  }
}
