package org.enso.syntax.text.spec

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.enso.data.List1
import org.enso.syntax.text.ast.AST._
import org.enso.syntax.text.ast.AST

import scala.reflect.runtime.universe.reify

case class ParserDef() extends Parser[AST] {

  //////////////////////////////////////////////////////////////////////////////
  //// Result //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  override def getResult(): Option[AST] = result.ast

  final object result {
    var current: Option[Elem] = None
    var ast: Option[AST]      = None
    var stack: List[Elem]     = Nil

    def push(): Unit = logger.trace {
      if (current.isDefined) {
        logger.log(s"Pushed: $current")
        stack +:= current.get
        current = None
      } else {
        logger.err("Undefined current")
      }
    }

    def pop(): Unit = logger.trace {
      if (stack.nonEmpty) {
        current = Some(stack.head)
        stack   = stack.tail
        logger.log(s"New result: $current")
      } else {
        logger.err("Trying to pop empty AST stack")
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Basic Char Classification ///////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  val lowerChar: Pattern  = range('a', 'z')
  val upperChar: Pattern  = range('A', 'Z')
  val digit: Pattern      = range('0', '9')
  val whitespace: Pattern = ' '.many1
  val newline: Char       = '\n'

  val char: Pattern = lowerChar | upperChar
  val specialChars
  : Pattern                = "," | "." | ":" | "/" | "’" | "=" | "'" | "|" | "+" | "-"
  val possibleChars: Pattern = char | digit | whitespace | specialChars

  val normalText: Pattern = possibleChars.many1

  //////////////////////////////////////////////////////////////////////////////
  //// Text ////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object text {
    def onPushing(in: String): Unit = logger.trace {
        push(in)
      }

    def push(in: String): Unit = logger.trace {
      result.current = Some(AST.Undefined(in))
      result.push()
    }
  }

  def onEOF(): Unit = {
    result.ast = Some(AST(result.stack))
  }

  ROOT || normalText || reify { text.onPushing(currentMatch) }
  ROOT || eof || reify { onEOF() }
}
