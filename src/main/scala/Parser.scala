package org.enso.syntax.text

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.ast.AST
import org.enso.syntax.text.spec.ParserDef
import java.io.File
import java.io.PrintWriter

import javax.print.attribute.DocAttributeSet

////////////////////////////////////////////////////////////////////////////////
//// Parser ////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

class Parser {
  import Parser._
  private val engine = newEngine()
  private val errMsg = "Internal Parser Error has occured"

//  def runMatched(input: String): AST = run(input) match {
//    case res(_, res.Success(v)) => v
//    case _                      => throw new Exception(errMsg)
//  }

  def run(input: String): Result[AST] = engine.run(new Reader(input))
}

object Parser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(ParserDef())

//  def runMatched(input: String): AST  = new Parser().runMatched(input)
  def run(input: String): Result[AST] = new Parser().run(input)
}
