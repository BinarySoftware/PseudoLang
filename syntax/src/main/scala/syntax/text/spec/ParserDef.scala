package org.PseudoLang.syntax.text.spec

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.enso.data.List1
import org.PseudoLang.syntax.text.ast.AST._
import org.PseudoLang.syntax.text.ast.AST

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
  //// Functions ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Func {
    def onPushing(in: String): Unit = logger.trace {
      val args = in.dropRight(1).substring(1)
      result.pop()
      result.current match {
        case Some(v:AST.Var) => push(v, args)
        case _ =>
          result.push()
          Undefined.push(in)
      }
    }

    def push(name: AST.Var, args: String): Unit = logger.trace {
      var argsList: List[AST.Var] = List()
      if (args.length > 0) {
        val al = args.split(',').toList
        for (a <- al) {
          val aNoSpaces = a.replaceAll(" ", "")
          if (aNoSpaces.contains(':')) {
            val elem = aNoSpaces.split(':')
            val varName = elem.head
            val tp = elem.tail.head
            argsList +:= AST.Var(varName, tp)
          } else {
            argsList +:= AST.Var(aNoSpaces)
          }
        }
      }
      result.current = Some(AST.Func(name, argsList.reverse))
      result.push()
    }

    val funcArgs = '(' >> not(')').many >> ')'
  }

  ROOT || Func.funcArgs || reify { Func.onPushing(currentMatch) }

  //////////////////////////////////////////////////////////////////////////////
  //// Comments ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object Comment {
    def onPushing(in: String): Unit = logger.trace {
      push(in.substring(2))
    }

    def push(in: String): Unit = logger.trace {
      result.current = Some(AST.Comment(in))
      result.push()
    }

    val pattern: Pattern = "//" >> not(newline).many
  }

  // FIXME Something is wrong with comment pattern
  ROOT || Comment.pattern || reify { Comment.onPushing(currentMatch) }

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
