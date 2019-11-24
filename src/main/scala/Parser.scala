package org.PseudoLang.syntax.text

import org.enso.flexer
import org.enso.flexer.Reader
import flexer.Parser.{Result => res}
import org.PseudoLang.syntax.text.ast.AST
import org.PseudoLang.syntax.text.spec.ParserDef

////////////////////////////////////////////////////////////////////////////////
//// PseudoLang Parser /////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/**
  * This is the main PseudoLang Parser class.
  *
  * This is where running pseudocode starts. The parser engine is initialized
  * here. Then it is passed input string, which is then analyzed/parsed, and if
  * everything ends successfully, it outputs AST, if not - Exception is thrown.
  */
class Parser {
  import Parser._
  private val engine = newEngine()
  private val errMsg = "Internal PseudoLang Parser Error has occurred"

  def runMatched(input: String): AST = run(input) match {
    case res(_, res.Success(v)) => v
    case _                      => throw new Exception(errMsg)
  }

  def run(input: String): Result[AST] = engine.run(new Reader(input))
}

object Parser {
  type Result[T] = org.enso.flexer.Parser.Result[T]
  private val newEngine = org.enso.flexer.Parser.compile(ParserDef())

  def runMatched(input: String): AST  = new Parser().runMatched(input)
  def run(input: String): Result[AST] = new Parser().run(input)
}
