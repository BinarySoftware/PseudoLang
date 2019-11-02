package org.enso.syntax.text

import org.enso.syntax.text.ast.AST
import org.enso.Logger
import org.enso.flexer.Parser.Result
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.Assertion

class ParserTests extends FlatSpec with Matchers {
  val logger = new Logger()

  def assertExpr(input: String, result: AST): Assertion = {
    val output = Parser.run(input)
    output match {
      case Result(_, Result.Success(value)) =>
        assert(value == result)
        assert(value.show() == input)
      case _ =>
        fail(s"Parsing documentation failed, consumed ${output.offset} chars")
    }
  }

  implicit class TestString(input: String) {
    def parse(str: String): String = {
      val escape = (str: String) => str.replace("\n", "\\n")
      s"parse `${escape(str)}`"
    }

    private val testBase = it should parse(input)

    def ?=(out: AST): Unit = testBase in {
      assertExpr(input, out)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Testing Environment /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "Funkcja ()" ?= AST(Function("Funkcja",None,None,Unit))
}
