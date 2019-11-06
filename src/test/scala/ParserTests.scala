package org.PseudoLang.syntax.text

import org.PseudoLang.PrettyPrinter
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
        println(PrettyPrinter.pretty(value.toString))
        assert(value == result)
        assert(value.show() == input)
      case _ =>
        fail(s"Parsing failed, consumed ${output.offset} chars")
    }
  }

  def assertExprNoPrinting(input: String, result: AST): Assertion = {
    val output = Parser.run(input)
    output match {
      case Result(_, Result.Success(value)) =>
        println(PrettyPrinter.pretty(value.toString))
        assert(value == result)
      case _ =>
        fail(s"Parsing failed, consumed ${output.offset} chars")
    }
  }

  implicit class TestString(input: String) {
    def parse(str: String): String = {
      val escape = (str: String) => str.replace("\n", "\\n")
      s"parse code: `${escape(str)}`"
    }

    private val testBase = it should parse(input)

    def ?=(out: AST): Unit = testBase in {
      assertExpr(input, out)
    }
    def ?==(out: AST): Unit = testBase in {
      assertExprNoPrinting(input, out)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Testing Environment /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  "" ?= AST()

  /* Variables */
  "Foo"   ?= AST(AST.Var("Foo"))
  "Foo  " ?= AST(AST.Var("Foo"), AST.Spacing(2))
//  "Foo: Int" ?= AST(AST.Opr(AST.Opr.TpAnn, AST.Var("Foo"), AST.Var("Int")))

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
    AST.Block(2, AST.Var("Bar"), AST.Newline(), AST.Var("Baz")),
    AST.Var("Bo")
  )

  """Foo
    |  Bar
    |    Ba
    |    Be
    |  Baz
    |Bo""".stripMargin ?= AST(
    AST.Var("Foo"),
    AST.Block(
      2,
      AST.Var("Bar"),
      AST.Block(4, AST.Var("Ba"), AST.Newline(), AST.Var("Be")),
      AST.Var("Baz")
    ),
    AST.Var("Bo")
  )

  /* Operator tests */
  "Bar<-Foo+Bo*Fo/Mo" ?== AST(
    AST.Opr(
      AST.Opr.Assign,
      AST.Var("Bar"),
      AST.Opr(
        AST.Opr.Add,
        AST.Var("Foo"),
        AST.Opr(
          AST.Opr.Mul,
          AST.Var("Bo"),
          AST.Opr(AST.Opr.Div, AST.Var("Fo"), AST.Var("Mo"))
        )
      )
    )
  )

  "Bar <- Foo + Bo * Fo / Mo   " ?= AST(
    AST.Opr(
      AST.Opr.Assign,
      AST.Var("Bar"),
      AST.Opr(
        AST.Opr.Add,
        AST.Var("Foo"),
        AST.Opr(
          AST.Opr.Mul,
          AST.Var("Bo"),
          AST.Opr(AST.Opr.Div, AST.Var("Fo"), AST.Var("Mo"))
        )
      )
    ),
    AST.Spacing(3)
  )

  """Bar <- Foo + Bo * Fo / Mo
    |Bar <- Foo + Bo
    |""".stripMargin ?= AST(
    AST.Opr(
      AST.Opr.Assign,
      AST.Var("Bar"),
      AST.Opr(
        AST.Opr.Add,
        AST.Var("Foo"),
        AST.Opr(
          AST.Opr.Mul,
          AST.Var("Bo"),
          AST.Opr(AST.Opr.Div, AST.Var("Fo"), AST.Var("Mo"))
        )
      )
    ),
    AST.Newline(),
    AST.Opr(
      AST.Opr.Assign,
      AST.Var("Bar"),
      AST.Opr(
        AST.Opr.Add,
        AST.Var("Foo"),
        AST.Var("Bo")
      )
    ),
    AST.Newline()
  )

//  """Foo
//    |  Bar
//    |    Ba
//    |    Be
//    |Bo""".stripMargin ?= AST(
//    AST.Var("Foo"),
//    AST.Newline(),
//    AST.Block(
//      2,
//      AST.Var("Bar"),
//      AST.Newline(),
//      AST.Block(4, AST.Var("Ba"), AST.Newline(), AST.Var("Be"))
//    ),
//    AST.Var("Bo")
//  )
//
//  """Foo
//    |  Bar
//    |    Ba
//    |    Be""".stripMargin ?= AST(
//    AST.Var("Foo"),
//    AST.Newline(),
//    AST.Block(
//      2,
//      AST.Var("Bar"),
//      AST.Newline(),
//      AST.Block(4, AST.Var("Ba"), AST.Newline(), AST.Var("Be"))
//    )
//  )
}
