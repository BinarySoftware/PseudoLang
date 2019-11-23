package org.PseudoLang.syntax.text.spec

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.PseudoLang.syntax.text.ast.AST._
import org.PseudoLang.syntax.text.ast.AST

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

  /* Keywords */
  val _then_   = "then"
  val _else_   = "else"
  val _do_     = "do"
  val _repeat_ = "repeat"
  val _return_ = "return"
  val _if_     = "if"
  val _while_  = "while"
  val _for_    = "for"
  val _until_  = "until"

  val parenOpen    = '('
  val parenClose   = ')'
  val bracketOpen  = '['
  val bracketClose = ']'

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

    def traverseLineWithOpr(stack: List[AST.Elem]): List[AST.Elem] =
      logger.trace {
        stack match {
          case elem1 :: rest =>
            result.pop()
            onFirstElementWhileTraversingThroughLine(elem1, rest)
          case Nil => Nil
        }
      }

    private def onFirstElementWhileTraversingThroughLine(
      elem1: Elem,
      rest: List[Elem]
    ): List[Elem] = {
      elem1 match {
        case s: Spacing =>
          s :: traverseLineWithOpr(rest)
        case c: Comment =>
          c :: traverseLineWithOpr(rest)
        case v: Var =>
          onSecondElementWhileTraversingThroughLine(rest, v)
        case o: Opr =>
          onSecondElementWhileTraversingThroughLine(rest, o)
        case _ => elem1 :: rest
      }
    }

    private def onSecondElementWhileTraversingThroughLine(
      rest: List[Elem],
      prev: AST.Elem
    ): List[Elem] = {
      rest match {
        case (_: Spacing) :: (o: Opr) :: restTail =>
          val opr = AST.Opr(o.marker, o.Le, prev)
          traverseLineWithOpr(opr :: restTail)
        case (o: Opr) :: restTail =>
          val opr = AST.Opr(o.marker, o.Le, prev)
          traverseLineWithOpr(opr :: restTail)
        case _ => prev :: rest
      }
    }

    def onTraversingLineForOprs(): Unit = logger.trace {
      val traversed: List[Elem] = traverseLineWithOpr(result.stack)
      result.stack = traversed
    }
  }

  ROOT || AST.Opr.Add.m      || Opr.onPushing(AST.Opr.Add)
  ROOT || AST.Opr.Sub.m      || Opr.onPushing(AST.Opr.Sub)
  ROOT || AST.Opr.Mul.m      || Opr.onPushing(AST.Opr.Mul)
  ROOT || AST.Opr.Div.m      || Opr.onPushing(AST.Opr.Div)
  ROOT || AST.Opr.Mod.m      || Opr.onPushing(AST.Opr.Mod)
  ROOT || AST.Opr.Pow.m      || Opr.onPushing(AST.Opr.Pow)
  ROOT || AST.Opr.Assign.m   || Opr.onPushing(AST.Opr.Assign)
  ROOT || AST.Opr.isEq.m     || Opr.onPushing(AST.Opr.isEq)
  ROOT || AST.Opr.isGr.m     || Opr.onPushing(AST.Opr.isGr)
  ROOT || AST.Opr.isLe.m     || Opr.onPushing(AST.Opr.isLe)
  ROOT || AST.Opr.isGrOrEq.m || Opr.onPushing(AST.Opr.isGrOrEq)
  ROOT || AST.Opr.isLeOrEq.m || Opr.onPushing(AST.Opr.isLeOrEq)
  ROOT || AST.Opr.isNotEq.m  || Opr.onPushing(AST.Opr.isNotEq)
  ROOT || AST.Opr.And.m      || Opr.onPushing(AST.Opr.And)
  ROOT || AST.Opr.Or.m       || Opr.onPushing(AST.Opr.Or)
  ROOT || AST.Opr.Not.m      || Opr.onPushing(AST.Opr.Not)
  ROOT || AST.Opr.Assign.m   || Opr.onPushing(AST.Opr.Assign)
  ROOT || AST.Opr.FloorDiv.m || Opr.onPushing(AST.Opr.FloorDiv)
  //////////////////////////////////////////////////////////////////////////////
  //// Variables ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Var {
    def onPushing(in: String): Unit = logger.trace {
      in.toLowerCase match {
        case `_then_`   => result.pushElem(AST.If.ThenCase())
        case `_else_`   => result.pushElem(AST.If.ElseCase())
        case `_do_`     => Func.onPushingDo()
        case `_repeat_` => Func.onPushingRepeat()
        case `_return_` => result.pushElem(AST.Func.Return())
        case _          => result.pushElem(AST.Var(in))
      }
    }

    val varChars: Pattern = lowerChar | upperChar | digit
    val varName: Pattern  = varChars.many
  }

  ROOT || Var.varName || Var.onPushing(currentMatch)

  //////////////////////////////////////////////////////////////////////////////
  //// Functions ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Func {
    def onPushingArgs(args: AST.Parens): Unit = logger.trace {
      result.pop()
      result.current match {
        case Some(v: AST.Var) =>
          result.pop()
          matchPreviousVar(args, v)
        case Some(_: AST.Spacing) =>
          result.pop()
          result.current match {
            case Some(v: AST.Var) =>
              result.pop()
              matchPreviousVar(args, v)
            case _ =>
              result.push()
              result.pushElem(args)
          }
        case _ =>
          result.push()
          result.pushElem(args)
      }
    }

    private def matchPreviousVar(args: AST.Parens, v: Var): Unit = {
      v.name.toLowerCase match {
        case `_if_`    => onPushingIf(args)
        case `_while_` => onPushingWhile(args)
        case `_for_`   => onPushingFor(args)
        case `_until_` => onPushingWhile(args)
        case _         => onPushingFunc(v, args)
      }
    }

    def onPushingFunc(name: AST.Var, args: AST.Parens): Unit = logger.trace {
      val fun = AST.Func(name, AST.Empty(), args)
      result.pushElem(fun)
    }

    def onPushingIf(cond: AST.Parens): Unit = logger.trace {
      val fun = AST.If(cond)
      result.pushElem(fun)
    }

    def onPushingFor(cond: AST.Parens): Unit = logger.trace {
      val fun = AST.For(cond)
      result.pushElem(fun)
    }

    def onPushingWhile(cond: AST.Parens): Unit = logger.trace {
      val fun = AST.While(cond)
      result.pushElem(fun)
    }

    def onPushingDo(): Unit = logger.trace {
      val fun = AST.DoWhile()
      result.pushElem(fun)
    }
    def onPushingRepeat(): Unit = logger.trace {
      val fun = AST.RepeatUntil()
      result.pushElem(fun)
    }

    def onPushingArray(str: String): Unit = logger.trace {
      val elems  = str.dropRight(1).substring(1)
      val varEls = AST.Var(elems) :: Nil
      val paren  = AST.Parens(bracketOpen, bracketClose, varEls)
      val arr    = AST.Array(AST.Empty(), paren)
      result.pushElem(arr)
    }

    def onPushingEmptyParen(): Unit = logger.trace {
      val elem = AST.Parens()
      result.pushElem(elem)
    }

    def onPushingClosingParen(): Unit = logger.trace {
      Opr.onTraversingLineForOprs()
      var stack: List[AST.Elem] = Nil
      while (!result.stack.head.isInstanceOf[AST.Parens]) {
        result.pop()
        stack +:= result.current.get
      }
      result.pop()
      val elem = AST.Parens('(', ')', stack)
      onPushingArgs(elem)
    }

    val funcArgs: Pattern = parenOpen >> not(parenClose).many >> parenClose
    val array: Pattern    = bracketOpen >> not(bracketClose).many >> bracketClose
  }

  ROOT || parenOpen  || Func.onPushingEmptyParen()
  ROOT || parenClose || Func.onPushingClosingParen()
  ROOT || Func.array || Func.onPushingArray(currentMatch)
  //  ROOT || Func.funcArgs || Func.onPushingArgs(currentMatch)

  //////////////////////////////////////////////////////////////////////////////
  //// Comments ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Comment {
    def onPushing(in: String): Unit = logger.trace {
      val com = AST.Comment(in.substring(2))
      result.pushElem(com)
    }

    val pattern: Pattern = AST.Comment.marker >> not(newline).many
  }

  ROOT || Comment.pattern || Comment.onPushing(currentMatch)

  //////////////////////////////////////////////////////////////////////////////
  //// Indentation Manager /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Indent {
    var latest: Int      = 0
    var current: Int     = 0
    var stack: List[Int] = Nil

    def onIndent(): Unit = logger.trace {
      current = currentMatch.length
      val diff = current - latest
      if (diff > 0) {
        pushNewEmptyBlock()
      } else if (diff < 0) {
        pushFilledBlock()
      }
      latest = current
    }

    private def pushFilledBlock(): Unit = logger.trace {
      result.pop()
      checkIfThereIsBlockInStack()
      onPushingNewLine()
    }

    private def pushNewEmptyBlock(): Unit = logger.trace {
      stack +:= current
      result.pop()
      val b = AST.Block(current)
      result.pushElem(b)
    }

    def checkIfThereIsBlockInStack(): Unit = logger.trace {
      while (stack.nonEmpty && current < stack.head) {
        stack = stack.tail
        onPushingBlock()
      }
    }

    private def onPushingBlock(): Unit = logger.trace {
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

    def onSpacing(str: String): Unit = logger.trace {
      val len = str.length
      val sp  = AST.Spacing(len)
      result.pushElem(sp)
    }

    def onPushingNewLine(): Unit = logger.trace {
      Opr.onTraversingLineForOprs()
      val nl = AST.Newline()
      result.pushElem(nl)
    }

    val emptyLine: Pattern     = spaces.opt >> newline
    val indentPattern: Pattern = spaces.opt.many
    val EOFPattern: Pattern    = indentPattern >> eof
  }

  val NEWLINE: State = state.define("Newline")

  ROOT    || spaces               || Indent.onSpacing(currentMatch)
  ROOT    || newline              || state.begin(NEWLINE)
  NEWLINE || Indent.EOFPattern    || Indent.onEOFPattern()
  NEWLINE || Indent.indentPattern || Indent.onIndentPattern()

  //////////////////////////////////////////////////////////////////////////////
  //// End Of File /////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object EOF {
    def fillBlocksBeforeEOF(): Unit = logger.trace {
      if (Indent.stack.nonEmpty) {
        Indent.current = 0
        Indent.checkIfThereIsBlockInStack()
      }
    }

    // TODO: Refactor this mess!
    def connectBlocksToAppropriateMethods(s: List[AST.Elem]): List[AST.Elem] = {
      s match {
        case (f: AST.Func) :: (b: AST.Block) :: rest =>
          val bl =
            AST.Block(b.indent, connectBlocksToAppropriateMethods(b.elems))
          AST.Func(f.name, bl, f.args) :: connectBlocksToAppropriateMethods(
            rest
          )
        case (i: AST.If) :: (b: AST.Block) :: rest =>
          val bl =
            AST.Block(b.indent, connectBlocksToAppropriateMethods(b.elems))
          AST.If(i.condition, bl) :: connectBlocksToAppropriateMethods(rest)
        case (_: AST.If.ThenCase) :: rest =>
          if (rest.contains(AST.If.ElseCase())) {
            val inThen = rest.take(rest.indexOf(AST.If.ElseCase()) - 1)
            AST.If.ThenCase(connectBlocksToAppropriateMethods(inThen)) :: AST
              .Newline() :: connectBlocksToAppropriateMethods(
              rest.drop(rest.indexOf(AST.If.ElseCase()))
            )
          } else {
            AST.If.ThenCase(connectBlocksToAppropriateMethods(rest)) :: Nil
          }
        case (_: AST.If.ElseCase) :: rest =>
          AST.If.ElseCase(connectBlocksToAppropriateMethods(rest)) :: Nil
        case (_: AST.DoWhile) :: (b: AST.Block) :: (w: AST.While) :: rest =>
          val bl =
            AST.Block(b.indent, connectBlocksToAppropriateMethods(b.elems))
          AST.DoWhile(w.condition, bl) :: connectBlocksToAppropriateMethods(
            rest
          )
        case (_: AST.RepeatUntil) :: (b: AST.Block) :: (w: AST.While) :: rest =>
          val bl =
            AST.Block(b.indent, connectBlocksToAppropriateMethods(b.elems))
          AST.RepeatUntil(w.condition, bl) :: connectBlocksToAppropriateMethods(
            rest
          )
        case (w: AST.While) :: (b: AST.Block) :: rest =>
          val bl =
            AST.Block(b.indent, connectBlocksToAppropriateMethods(b.elems))
          AST.While(w.condition, bl) :: connectBlocksToAppropriateMethods(
            rest
          )
        case (f: AST.For) :: (b: AST.Block) :: rest =>
          val bl =
            AST.Block(b.indent, connectBlocksToAppropriateMethods(b.elems))
          AST.For(f.condition, bl) :: connectBlocksToAppropriateMethods(
            rest
          )
        case (_: AST.Func.Return) :: rest =>
          AST.Func.Return(
            connectBlocksToAppropriateMethods(
              rest
            )
          ) :: Nil
        case (v: AST.Var) :: (a: AST.Array) :: rest =>
          AST.Array(v, a.elems) :: connectBlocksToAppropriateMethods(rest)
        case v :: rest => v :: connectBlocksToAppropriateMethods(rest)
        case Nil       => Nil
      }
    }

    def onEOF(): Unit = logger.trace {
      Opr.onTraversingLineForOprs()
      fillBlocksBeforeEOF()
      val stack = connectBlocksToAppropriateMethods(result.stack.reverse)
      result.ast = Some(AST(stack))
    }
  }

  ROOT || eof || EOF.onEOF()

  //////////////////////////////////////////////////////////////////////////////
  //// Undefined ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Undefined {
    def onPushing(in: String): Unit = logger.trace {
      val und = AST.Undefined(in)
      result.pushElem(und)
    }
  }

  ROOT || any || Undefined.onPushing(currentMatch)
}
