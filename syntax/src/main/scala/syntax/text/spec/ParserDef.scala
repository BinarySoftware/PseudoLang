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

  val spaces: Pattern = ' '.many1
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

  final object Indent {
    var latest: Int  = 0
    var current: Int = 0

    def onIndent(): Unit = logger.trace {
      current = currentMatch.length
      val diff = current - latest
      if (diff > 0) {
        val b = AST.Block(current)
        result.pushElem(b)
      } else if (diff < 0) {
        val elems: List[Elem] = onFillingBlocks()
        result.current match {
          case Some(b: AST.Block) =>
            val block = AST.Block(b.indent, elems)
            result.pushElem(block)
          case _ =>
            val block = AST.Block(current, elems)
            result.pushElem(block)
        }
      }
      latest = current
    }

    def onFillingBlocks(): List[Elem] = logger.trace {
      var elems: List[Elem] = Nil
      result.pop()
      while (result.stack.nonEmpty) {
        result.pop()
        result.current match {
          case Some(b: AST.Block) =>
            if (b.elems.isEmpty) {
              return elems
            } else {
              elems +:= b
            }
          case Some(value) => elems +:= value
          case None        =>
        }
      }
      elems
    }

    def onEmptyLine(): Unit = logger.trace {
      Indent.onPushingNewLine()
    }

    def onIndentPattern(): Unit = logger.trace {
      state.end()
      if (result.stack.nonEmpty) {
        Indent.onPushingNewLine()
      }
      Indent.onIndent()
    }

    def onEOFPattern(): Unit = logger.trace {
      state.end()
      Indent.onPushingNewLine()
      EOF.onEOF()
    }

    val emptyLine: Pattern     = spaces.opt >> newline
    val indentPattern: Pattern = spaces.opt.many
    val EOFPattern: Pattern    = indentPattern >> eof

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

  val NEWLINE: State = state.define("Newline")

  ROOT    || spaces               || reify { Indent.onPushingSpacing(currentMatch) }
  ROOT    || newline              || reify { state.begin(NEWLINE)                  }
  NEWLINE || Indent.EOFPattern    || reify { Indent.onEOFPattern()                 }
  NEWLINE || Indent.indentPattern || reify { Indent.onIndentPattern()              }

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
