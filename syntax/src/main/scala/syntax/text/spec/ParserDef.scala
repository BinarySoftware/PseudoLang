package org.PseudoLang.syntax.text.spec

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.PseudoLang.syntax.text.ast.AST._
import org.PseudoLang.syntax.text.ast.AST
import scala.reflect.runtime.universe.reify

case class ParserDef() extends Parser[AST] {

  // FIXME
  //  - Get rid of AST.Spacing - bad idea
  //  - Tailrec method for adding R to operator, nesting operators etc
  //  - Push block on EOF, on large indent diff, close many blocks at once

  // TODO
  //  - Add Scala code generator
  //  - Add conditional functions
  //  - Add Array Support with []
  //  - Add AST.Args with ()
  //  - Add Return
  //  - Add While, for and do while loops
  //  - Add AST.Function.Call to call func

  /* tail recursive functions for block creation and operator nesting

@tailrec def pushFullBlock(s: List[AST.Elem], ci: Int) : s = logger.trace {
	s match {
		case b: AST.Block :: rest =>
			if(b.elems.nonEmpty) {
				// wszystkie elementy dotychczas do bloku
				if(b.indent > ci) {
					bl :: pushFullBlock(rest, ci)
				} else {
					bl :: rest
				}
		 	}
		case e :: rest => e :: pushFullBlock(rest)
		case Nil => Nil
	}
}
   */

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

  //////////////////////////////////////////////////////////////////////////////
  //// Operators ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Opr {
    def onPushing(opr: AST.Opr.Marker): Unit = logger.trace {
      var op = AST.Opr(opr)
      result.pop()
      result.current match {
        case Some(v) =>
          v match {
            case e: Var => op = AST.Opr(opr, e)
            case _: Spacing =>
              result.stack.head match {
                case e: Var =>
                  result.pop()
                  op = AST.Opr(opr, e)
                case e: Func =>
                  result.pop()
                  op = AST.Opr(opr, e)
                case _ => result.push()
              }
            case e: Func => op = AST.Opr(opr, e)
            case _       => result.push()
          }
        case None =>
      }
      result.pushElem(op)
    }
  }

  ROOT || AST.Opr.Add.m      || reify { Opr.onPushing(AST.Opr.Add)      }
  ROOT || AST.Opr.Sub.m      || reify { Opr.onPushing(AST.Opr.Sub)      }
  ROOT || AST.Opr.Mul.m      || reify { Opr.onPushing(AST.Opr.Mul)      }
  ROOT || AST.Opr.Div.m      || reify { Opr.onPushing(AST.Opr.Div)      }
  ROOT || AST.Opr.Mod.m      || reify { Opr.onPushing(AST.Opr.Mod)      }
  ROOT || AST.Opr.Pow.m      || reify { Opr.onPushing(AST.Opr.Pow)      }
  ROOT || AST.Opr.Assign.m   || reify { Opr.onPushing(AST.Opr.Assign)   }
  ROOT || AST.Opr.TpAnn.m    || reify { Opr.onPushing(AST.Opr.TpAnn)    }
  ROOT || AST.Opr.isEq.m     || reify { Opr.onPushing(AST.Opr.isEq)     }
  ROOT || AST.Opr.isGr.m     || reify { Opr.onPushing(AST.Opr.isGr)     }
  ROOT || AST.Opr.isLe.m     || reify { Opr.onPushing(AST.Opr.isLe)     }
  ROOT || AST.Opr.isGrOrEq.m || reify { Opr.onPushing(AST.Opr.isGrOrEq) }
  ROOT || AST.Opr.isLeOrEq.m || reify { Opr.onPushing(AST.Opr.isLeOrEq) }

  //////////////////////////////////////////////////////////////////////////////
  //// Variables ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object Var {
    def onPushing(in: String): Unit = logger.trace {
      val vr = AST.Var(in)
      result.pushElem(vr)
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
          argsList +:= AST.Var(aNoSpaces)
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
        result.pop()
        val b = AST.Block(current)
        result.pushElem(b)
      } else if (diff < 0) {
        onPushingBlock()
      }
      latest = current
    }

    private def onPushingBlock(): Unit = {
      val elems: List[Elem] = onFillingBlocks()
      result.current match {
        case Some(b: Block) =>
          val block = AST.Block(b.indent, elems)
          result.pushElem(block)
        case _ =>
          val block = AST.Block(current, elems)
          result.pushElem(block)
      }
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

    def onSpacing(str: String): Unit = logger.trace {
      val len = str.length
      val sp  = AST.Spacing(len)
      result.pushElem(sp)
    }

    def pushLineWithOpr(s: List[AST.Elem]): List[AST.Elem] = logger.trace {
      s match {
        case (c: AST.Comment) :: rest =>
          result.pop()
          c :: pushLineWithOpr(rest)
        case (v: AST.Var) :: (o: AST.Opr) :: rest =>
          result.pop()
          result.pop()
          val opr = AST.Opr(o.marker, o.Le, v)
          pushLineWithOpr(opr :: rest)
        case (or: AST.Opr) :: (ol: AST.Opr) :: rest =>
          result.pop()
          result.pop()
          val opr = AST.Opr(ol.marker, ol.Le, or)
          pushLineWithOpr(opr :: rest)
        case rest => rest
        case Nil  => Nil
      }
    }

    def onTraversingLineForOprs(): Unit = logger.trace {
      val traversed: List[Elem] = pushLineWithOpr(result.stack)
      result.stack = traversed
    }

    def onPushingNewLine(): Unit = logger.trace {
      onTraversingLineForOprs()
      val nl = AST.Newline()
      result.pushElem(nl)
    }
  }

  val NEWLINE: State = state.define("Newline")

  ROOT    || spaces               || reify { Indent.onSpacing(currentMatch) }
  ROOT    || newline              || reify { state.begin(NEWLINE)           }
  NEWLINE || Indent.EOFPattern    || reify { Indent.onEOFPattern()          }
  NEWLINE || Indent.indentPattern || reify { Indent.onIndentPattern()       }

  //////////////////////////////////////////////////////////////////////////////
  //// End Of File /////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  final object EOF {
    def onEOF(): Unit = {
      Indent.onTraversingLineForOprs()
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
