package org.PseudoLang.syntax.text

import org.PseudoLang.Debug
import org.PseudoLang._
import org.PseudoLang.syntax.text.ast.AST
import org.enso.Logger
import org.enso.flexer.Parser.Result
import org.scalatest.Assertion
import org.scalatest.FlatSpec
import org.scalatest.Matchers

class ParserTests extends FlatSpec with Matchers {
  val logger = new Logger()

  def assertExpr(input: String, result: AST): Assertion = {
    val output = Parser.run(input)
    output match {
      case Result(_, Result.Success(value)) =>
        println(Debug.pretty(value.toString))
        println(value.show())
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
        println(Debug.pretty(value.toString))
        println(value.show())
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

  /* Comments */
  "//Com"      ?= AST(AST.Comment("Com"))
  "Foo//Com"   ?= AST(AST.Var("Foo"), AST.Comment("Com"))
  "Foo  //Com" ?= AST(AST.Var("Foo"), AST.Spacing(2), AST.Comment("Com"))

  /* Functions */
  "Funkcja()"  ?= AST(AST.Func(AST.Var("Funkcja"), AST.Parens()))
  "Funkcja(a)" ?= AST(AST.Func(AST.Var("Funkcja"), AST.Parens(AST.Var("a"))))
  "Funkcja(a,b)" ?= AST(
    AST.Func(
      AST.Var("Funkcja"),
      AST.Parens(AST.Var("a"), AST.Undefined(","), AST.Var("b"))
    )
  )
  "Funkcja ()" ?== AST(AST.Func(AST.Var("Funkcja")))
  """Funkcja ()
    |  return a""".stripMargin ?== AST(
    AST.Func(
      AST.Var("Funkcja"),
      AST.Block(2, AST.Func.Return(AST.Spacing(), AST.Var("a")))
    )
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

  /* Indent tests */
  """Foo
    |  Bar
    |  Baz
    |Bo""".stripMargin ?== AST(
    AST.Var("Foo"),
    AST.Block(2, AST.Var("Bar"), AST.Newline(), AST.Var("Baz")),
    AST.Newline(),
    AST.Var("Bo")
  )

  """Foo
    |  Bar
    |    Ba
    |    Be
    |  Baz
    |Bo""".stripMargin ?== AST(
    AST.Var("Foo"),
    AST.Block(
      2,
      AST.Var("Bar"),
      AST.Block(4, AST.Var("Ba"), AST.Newline(), AST.Var("Be")),
      AST.Newline(),
      AST.Var("Baz")
    ),
    AST.Newline(),
    AST.Var("Bo")
  )

  """Foo
    |  Bar
    |    Ba
    |    Be
    |Bo""".stripMargin ?== AST(
    AST.Var("Foo"),
    AST.Block(
      2,
      AST.Var("Bar"),
      AST.Block(4, AST.Var("Ba"), AST.Newline(), AST.Var("Be"))
    ),
    AST.Newline(),
    AST.Var("Bo")
  )

  """Foo
    |  Bar
    |    Ba
    |    Be""".stripMargin ?== AST(
    AST.Var("Foo"),
    AST.Block(
      2,
      AST.Var("Bar"),
      AST.Block(4, AST.Var("Ba"), AST.Newline(), AST.Var("Be"))
    )
  )

  """Foo
    |  Bar
    |    Ba
    |    Be
    |
    |""".stripMargin ?== AST(
    AST.Var("Foo"),
    AST.Block(
      2,
      AST.Var("Bar"),
      AST.Block(4, AST.Var("Ba"), AST.Newline(), AST.Var("Be"))
    ),
    AST.Newline(),
    AST.Newline()
  )

  """Swap(a,b)
    |  c <- a
    |  a <- b
    |  b <- c""".stripMargin ?== AST(
    AST.Func(
      AST.Var("Swap"),
      AST.Block(
        2,
        AST.Opr(AST.Opr.Assign, AST.Var("c"), AST.Var("a")),
        AST.Newline(),
        AST.Opr(AST.Opr.Assign, AST.Var("a"), AST.Var("b")),
        AST.Newline(),
        AST.Opr(AST.Opr.Assign, AST.Var("b"), AST.Var("c"))
      ),
      AST.Parens(AST.Var("a"), AST.Undefined(","), AST.Var("b"))
    )
  )

  """|Fu(a,b)
     |  c <- a
     |b""".stripMargin ?== AST(
    AST.Func(
      AST.Var("Fu"),
      AST.Block(
        2,
        AST.Opr(AST.Opr.Assign, AST.Var("c"), AST.Var("a"))
      ),
      AST.Parens(AST.Var("a"), AST.Undefined(","), AST.Var("b"))
    ),
    AST.Newline(),
    AST.Var("b")
  )

  /* Control Flow tests */
  """|If(a<b)
     |  then a
     |  else b""".stripMargin ?== AST(
    AST.If(
      AST.Parens(AST.Opr(AST.Opr.isLe, AST.Var("a"), AST.Var("b"))),
      AST.Block(
        2,
        AST.If.ThenCase(
          AST.Spacing(),
          AST.Var("a")
        ),
        AST.Newline(),
        AST.If.ElseCase(AST.Spacing(), AST.Var("b"))
      )
    )
  )

  """|If(a<b)
     |  then a
     |  else If(a=b)
     |    then 2
     |    else b""".stripMargin ?== AST(
    AST.If(
      AST.Parens(AST.Opr(AST.Opr.isLe, AST.Var("a"), AST.Var("b"))),
      AST.Block(
        2,
        AST.If
          .ThenCase(
            AST.Spacing(),
            AST.Var("a")
          ),
        AST.Newline(),
        AST.If.ElseCase(
          AST.If(
            AST.Parens(AST.Opr(AST.Opr.isEq, AST.Var("a"), AST.Var("b"))),
            AST.Block(
              4,
              AST.If
                .ThenCase(
                  AST.Spacing(),
                  AST.Var("2")
                ),
              AST.Newline(),
              AST.If.ElseCase(AST.Spacing(), AST.Var("b"))
            )
          )
        )
      )
    )
  )

  /* Loops tests */
  """do
    |  b <- b + a
    |  a <- a + 1
    |while (a < 5)""".stripMargin ?== AST(
    AST.DoWhile(
      AST.Parens(AST.Opr(AST.Opr.isLe, AST.Var("a"), AST.Var("5"))),
      AST.Block(
        2,
        AST.Opr(
          AST.Opr.Assign,
          AST.Var("b"),
          AST.Opr(AST.Opr.Add, AST.Var("b"), AST.Var("a"))
        ),
        AST.Newline(),
        AST.Opr(
          AST.Opr.Assign,
          AST.Var("a"),
          AST.Opr(AST.Opr.Add, AST.Var("a"), AST.Var("1"))
        )
      )
    )
  )

  """while (a < 5)
    |  b <- b + a
    |  a <- a + 1""".stripMargin ?== AST(
    AST.While(
      AST.Parens(AST.Opr(AST.Opr.isLe, AST.Var("a"), AST.Var("5"))),
      AST.Block(
        2,
        AST.Opr(
          AST.Opr.Assign,
          AST.Var("b"),
          AST.Opr(AST.Opr.Add, AST.Var("b"), AST.Var("a"))
        ),
        AST.Newline(),
        AST.Opr(
          AST.Opr.Assign,
          AST.Var("a"),
          AST.Opr(AST.Opr.Add, AST.Var("a"), AST.Var("1"))
        )
      )
    )
  )

  """repeat
    |  b <- b + a
    |  a <- a + 1
    |until (a > 5)""".stripMargin ?== AST(
    AST.RepeatUntil(
      AST.Parens(AST.Opr(AST.Opr.isGr, AST.Var("a"), AST.Var("5"))),
      AST.Block(
        2,
        AST.Opr(
          AST.Opr.Assign,
          AST.Var("b"),
          AST.Opr(AST.Opr.Add, AST.Var("b"), AST.Var("a"))
        ),
        AST.Newline(),
        AST.Opr(
          AST.Opr.Assign,
          AST.Var("a"),
          AST.Opr(AST.Opr.Add, AST.Var("a"), AST.Var("1"))
        )
      )
    )
  )

  """for (i in a)
    |  b <- b + i""".stripMargin ?== AST(
    AST.For(
      AST.Parens(
        AST.Var("i"),
        AST.Spacing(),
        AST.Var("in"),
        AST.Spacing(),
        AST.Var("a")
      ),
      AST.Block(
        2,
        AST.Opr(
          AST.Opr.Assign,
          AST.Var("b"),
          AST.Opr(AST.Opr.Add, AST.Var("b"), AST.Var("i"))
        )
      )
    )
  )

  val allPossibleMarkers: List[AST.Opr.Marker] = List(
    AST.Opr.Add,
    AST.Opr.Sub,
    AST.Opr.Mul,
    AST.Opr.Div,
    AST.Opr.Mod,
    AST.Opr.Pow,
    AST.Opr.isEq,
    AST.Opr.isGr,
    AST.Opr.isLe,
    AST.Opr.isGrOrEq,
    AST.Opr.isLeOrEq,
    AST.Opr.isNotEq,
    AST.Opr.And,
    AST.Opr.Or,
    AST.Opr.Not,
    AST.Opr.Assign
  )

  allPossibleMarkers.foreach(
    t =>
      s"a ${t.m} b" ?= AST(
        AST.Opr(t, AST.Var("a"), AST.Var("b"))
      )
  )

  """a[1,2,3,4,5]
    |a[1]""".stripMargin ?== AST(
    AST.Array(AST.Var("a"), AST.Parens('[', ']', AST.Var("1,2,3,4,5") :: Nil)),
    AST.Newline(),
    AST.Array(AST.Var("a"), AST.Parens('[', ']', AST.Var("1") :: Nil))
  )
}
