package org.PseudoLang.syntax.text

import org.PseudoLang.syntax.text.ast.AST
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
        pprint.pprintln(value)
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

  /* Variables */
  "Foo"   ?= AST(AST.Var("Foo"))
  "Foo  " ?= AST(AST.Var("Foo"), AST.Spacing(2))
  // FIXME - Type Annotation : "Foo: Int" ?= AST(AST.Var("Foo", "Int"))

  /* Comments */
  "//Com"      ?= AST(AST.Comment("Com"))
  "Foo//Com"   ?= AST(AST.Var("Foo"), AST.Comment("Com"))
  "Foo  //Com" ?= AST(AST.Var("Foo"), AST.Spacing(2), AST.Comment("Com"))

  /* Functions */
  "Funkcja()"  ?= AST(AST.Func(AST.Var("Funkcja")))
  "Funkcja(a)" ?= AST(AST.Func(AST.Var("Funkcja"), AST.Var("a")))
  "Funkcja(a, b)" ?= AST(
    AST.Func(AST.Var("Funkcja"), AST.Var("a"), AST.Var("b"))
  )

  /* Bad function definition */
  "Funkcja ()" ?= AST(AST.Var("Funkcja"), AST.Spacing(), AST.Undefined("()"))

  /* Indent tests */
  """Foo
    |  Bar
    |  Baz
    |Bo""".stripMargin ?= AST(
    AST.Var("Foo"),
    AST.Elem.Newline,
    AST.Block(2, AST.Var("Bar"), AST.Elem.Newline, AST.Var("Baz")),
    AST.Var("Bo")
  )
// FIXME Error in how block is being made
//  """Foo
//    |  Bar
//    |    Ba
//    |    Be
//    |  Baz
//    |Bo""".stripMargin ?= AST(
//    AST.Var("Foo"),
//    AST.Elem.Newline,
//    AST.Block(
//      2,
//      AST.Var("Bar"),
//      AST.Elem.Newline,
//      AST.Block(2, AST.Var("Ba"), AST.Elem.Newline, AST.Var("Be")),
//      AST.Var("Baz")
//    ),
//    AST.Var("Bo")
//  )
}
