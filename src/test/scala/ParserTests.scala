package org.BinarySoftware.PseudoLang.syntax.text

import org.BinarySoftware.PseudoLang.syntax.text.ast.AST
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

  "" ?= AST()
  "Foo" ?= AST(AST.Var("Foo"))
  "Foo: Int" ?= AST(AST.Var("Foo", "Int"))
  "//Comment" ?= AST(AST.Comment("Comment"))
  "Funkcja()" ?= AST(AST.Func(AST.Var("Funkcja")))
  "Funkcja(a)" ?= AST(AST.Func(AST.Var("Funkcja"),AST.Var("a")))
  "Funkcja(a, b)" ?= AST(AST.Func(AST.Var("Funkcja"),AST.Var("a"),AST.Var("b")))
}
