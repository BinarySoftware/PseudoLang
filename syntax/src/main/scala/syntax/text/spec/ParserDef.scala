package org.PseudoLang.syntax.text.spec

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
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

    def pushElem(elem: Elem): Unit = logger.trace {
      current = Some(elem)
      push()
    }

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

  val lowerChar: Pattern = range('a', 'z')
  val upperChar: Pattern = range('A', 'Z')
  val digit: Pattern     = range('0', '9')

  val spaces: Pattern = ' '.many
  val newline: Char   = '\n'

  val varChars: Pattern = lowerChar | upperChar | digit
  val varName: Pattern  = varChars.many

  val tpAnnMarker  = ':'
  val assignMarker = "<-"

  //////////////////////////////////////////////////////////////////////////////
  //// Variables ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  // TODO: Add type annotation support
  final object Var {
    def onPushing(in: String): Unit = logger.trace {
      val vr = AST.Var(in)
      push(vr)
    }

    def push(elem: AST.Elem): Unit = logger.trace {
      result.current = Some(elem)
      result.push()
    }
  }

  ROOT || varName || reify { Var.onPushing(currentMatch) }

  //////////////////////////////////////////////////////////////////////////////
  //// Functions ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Func {
    def onPushingArgs(in: String): Unit = logger.trace {
      val args = in.dropRight(1).substring(1)
      result.pop()
      result.current match {
        case Some(v: AST.Var) => onPushingFunc(v, args)
        case _ =>
          result.push()
          Undefined.onPushing(in)
      }
    }

    def onPushingFunc(name: AST.Var, args: String): Unit = logger.trace {
      var argsList: List[AST.Var] = List()
      if (args.length > 0) {
        val al = args.split(',').toList
        for (a <- al) {
          val aNoSpaces = a.replaceAll(" ", "")
          if (aNoSpaces.contains(':')) {
            val elem    = aNoSpaces.split(':')
            val varName = elem.head
            val tp      = elem.tail.head
            argsList +:= AST.Var(varName, tp)
          } else {
            argsList +:= AST.Var(aNoSpaces)
          }
        }
      }
      val fun = AST.Func(name, argsList.reverse)
      result.pushElem(fun)
    }

    val funcArgs: Pattern = '(' >> not(')').many >> ')'
  }

  ROOT || Func.funcArgs || reify { Func.onPushingArgs(currentMatch) }

  //////////////////////////////////////////////////////////////////////////////
  //// Comments ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object Comment {
    def onPushing(in: String): Unit = logger.trace {
      val com = AST.Comment(in.substring(2))
      result.pushElem(com)
    }

    val pattern: Pattern = "//" >> not(newline).many
  }

  // FIXME Something is wrong with comment pattern
  ROOT || Comment.pattern || reify { Comment.onPushing(currentMatch) }

  //////////////////////////////////////////////////////////////////////////////
  //// Indentation Manager /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object IndentManager {
    def onPushingSpacing(str: String): Unit = logger.trace {
      val len = str.length
      val sp  = AST.Spacing(len)
      result.pushElem(sp)
    }

    def onPushingNewLine(): Unit = logger.trace {
      val nl = AST.Elem.Newline
      result.pushElem(nl)
    }
  }

  ROOT || spaces  || reify { IndentManager.onPushingSpacing(currentMatch) }
  ROOT || newline || reify { IndentManager.onPushingNewLine()             }

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
      val und = AST.Undefined(in)
      result.pushElem(und)
    }
  }

  ROOT || any || reify { Undefined.onPushing(currentMatch) }
}
