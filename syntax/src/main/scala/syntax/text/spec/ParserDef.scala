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

  val spaces: Pattern = ' '.many
  val newline: Char       = '\n'

  val varChars = lowerChar | upperChar | digit
  val varName = varChars.many
  val tpAnChar = ':'

  //////////////////////////////////////////////////////////////////////////////
  //// Variables ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object Var {
    def onPushing(in: String): Unit = logger.trace {
      val inNoSpaces = in.replaceAll(" ", "")
      if (inNoSpaces.contains(':')) {
        val elem = inNoSpaces.split(':')
        val varName = elem.head
        val tp = elem.tail.head
        pushWithType(varName, tp)
      } else {
        push(inNoSpaces)
      }
    }

    def push(in: String): Unit = logger.trace {
      result.current = Some(AST.Var(in))
      result.push()
    }

    def pushWithType(in: String, tp: String): Unit = logger.trace {
      result.current = Some(AST.Var(in, tp))
      result.push()
    }

    val varWithTp = varName >> spaces >> tpAnChar >> spaces >> varName
  }

  ROOT || varName || reify { Var.onPushing(currentMatch) }
  ROOT || Var.varWithTp || reify { Var.onPushing(currentMatch) }

  //////////////////////////////////////////////////////////////////////////////
  //// Indentation Manager /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object IndentManager {
    def pushNewLine(): Unit = logger.trace {
      result.current = Some(AST.Elem.Newline)
      result.push()
    }
  }

  ROOT || newline || reify { IndentManager.pushNewLine() }

  //////////////////////////////////////////////////////////////////////////////
  //// End Of File /////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object EOF {
    def onEOF(): Unit = {
      result.ast = Some(AST(result.stack.reverse))
    }
  }

  ROOT || eof || reify { EOF.onEOF() }

  //////////////////////////////////////////////////////////////////////////////
  //// Undefined ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object Undefined {
    def onPushing(in: String): Unit = logger.trace {
      push(in)
    }

    def push(in: String): Unit = logger.trace {
      result.current = Some(AST.Undefined(in))
      result.push()
    }
  }

  ROOT || any || reify { Undefined.onPushing(currentMatch) }
}
