package org.PseudoLang.syntax.text.spec

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.PseudoLang.syntax.text.ast.AST._
import org.PseudoLang.syntax.text.ast.AST

/**
  * This is the parser definition class. It is built upon [[org.enso.flexer]] library
  */
case class ParserDef() extends Parser[AST] {

  /**
    * This is the Result object.
    * It is used to store parsed data on stack with LIFO method, applied
    * by push and pop methods.
    */
  //////////////////////////////////////////////////////////////////////////////
  //// Result //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  override def getResult(): Option[AST] = Result.ast

  final object Result {
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

  /**
    * This is the Opr object.
    * It is used to parse operators in text, and append appropriate AST Elements
    * to those operators by traversing stack and looking for matching patterns.
    */
  //////////////////////////////////////////////////////////////////////////////
  //// Operators ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Opr {
    def onPushing(opr: AST.Opr.Marker): Unit = logger.trace {
      var op = AST.Opr(opr)
      Result.pop()
      Result.current match {
        case Some(v) =>
          v match {
            case e: Var => op = AST.Opr(opr, e)
            case _: Spacing =>
              Result.stack.head match {
                case e: Var =>
                  Result.pop()
                  op = AST.Opr(opr, e)
                case e: Func =>
                  Result.pop()
                  op = AST.Opr(opr, e)
                case _ => Result.push()
              }
            case e: Func => op = AST.Opr(opr, e)
            case _       => Result.push()
          }
        case None =>
      }
      Result.pushElem(op)
    }

    def traverseLineWithOpr(stack: List[AST.Elem]): List[AST.Elem] =
      logger.trace {
        stack match {
          case elem1 :: rest =>
            Result.pop()
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
      val traversed: List[Elem] = traverseLineWithOpr(Result.stack)
      Result.stack = traversed
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

  /**
    * This is the Var object.
    * It is used to create primitive variables, as well as to match on names to
    * create control flow AST, functions, and loops.
    */
  //////////////////////////////////////////////////////////////////////////////
  //// Variables ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Var {
    def onPushing(in: String): Unit = logger.trace {
      in.toLowerCase match {
        case `_then_`   => Result.pushElem(AST.If.ThenCase())
        case `_else_`   => Result.pushElem(AST.If.ElseCase())
        case `_do_`     => Func.onPushingDo()
        case `_repeat_` => Func.onPushingRepeat()
        case `_return_` => Result.pushElem(AST.Func.Return())
        case _          => Result.pushElem(AST.Var(in))
      }
    }

    val varChars: Pattern = lowerChar | upperChar | digit
    val varName: Pattern  = varChars.many
  }

  ROOT || Var.varName || Var.onPushing(currentMatch)

  /**
    * This is the Func object.
    * It does much more than simply creating function. It is also responsible for
    * creation of loops, control flow statement, arrays - all the things which
    * use [[AST.Parens]] expressions
    */
  //////////////////////////////////////////////////////////////////////////////
  //// Functions ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Func {
    def onPushingArgs(args: AST.Parens): Unit = logger.trace {
      Result.pop()
      Result.current match {
        case Some(v: AST.Var) =>
          Result.pop()
          matchPreviousVar(args, v)
        case Some(_: AST.Spacing) =>
          Result.pop()
          Result.current match {
            case Some(v: AST.Var) =>
              Result.pop()
              matchPreviousVar(args, v)
            case _ =>
              Result.push()
              Result.pushElem(args)
          }
        case _ =>
          Result.push()
          Result.pushElem(args)
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
      Result.pushElem(fun)
    }

    def onPushingIf(cond: AST.Parens): Unit = logger.trace {
      val fun = AST.If(cond)
      Result.pushElem(fun)
    }

    def onPushingFor(cond: AST.Parens): Unit = logger.trace {
      val fun = AST.For(cond)
      Result.pushElem(fun)
    }

    def onPushingWhile(cond: AST.Parens): Unit = logger.trace {
      val fun = AST.While(cond)
      Result.pushElem(fun)
    }

    def onPushingDo(): Unit = logger.trace {
      val fun = AST.DoWhile()
      Result.pushElem(fun)
    }
    def onPushingRepeat(): Unit = logger.trace {
      val fun = AST.RepeatUntil()
      Result.pushElem(fun)
    }

    def onPushingArray(str: String): Unit = logger.trace {
      val elems  = str.dropRight(1).substring(1)
      val varEls = AST.Var(elems) :: Nil
      val paren  = AST.Parens(bracketOpen, bracketClose, varEls)
      val arr    = AST.Array(AST.Empty(), paren)
      Result.pushElem(arr)
    }

    def onPushingEmptyParen(): Unit = logger.trace {
      val elem = AST.Parens()
      Result.pushElem(elem)
    }

    def onPushingClosingParen(): Unit = logger.trace {
      Opr.onTraversingLineForOprs()
      var stack: List[AST.Elem] = Nil
      while (!Result.stack.head.isInstanceOf[AST.Parens]) {
        Result.pop()
        stack +:= Result.current.get
      }
      Result.pop()
      val elem = AST.Parens('(', ')', stack)
      onPushingArgs(elem)
    }

    val funcArgs: Pattern = parenOpen >> not(parenClose).many >> parenClose
    val array: Pattern    = bracketOpen >> not(bracketClose).many >> bracketClose
  }

  ROOT || parenOpen  || Func.onPushingEmptyParen()
  ROOT || parenClose || Func.onPushingClosingParen()
  ROOT || Func.array || Func.onPushingArray(currentMatch)

  /**
    * This is the Comment object.
    * Nothing exciting, only pushing simple string as comment.
    */
  //////////////////////////////////////////////////////////////////////////////
  //// Comments ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Comment {
    def onPushing(in: String): Unit = logger.trace {
      val com = AST.Comment(in.substring(2))
      Result.pushElem(com)
    }

    val pattern: Pattern = AST.Comment.marker >> not(newline).many
  }

  ROOT || Comment.pattern || Comment.onPushing(currentMatch)

  /**
    * This is the Indentation manager.
    * It is a pretty robust element of PseudoLang Parser, as thanks to it, the
    * language doesn't need any semicolons, curly braces etc. It is used to create
    * indented [[AST.Block]] for use in functions, control flow and loops.
    */
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
      Result.pop()
      checkIfThereIsBlockInStack()
      onPushingNewLine()
    }

    private def pushNewEmptyBlock(): Unit = logger.trace {
      stack +:= current
      Result.pop()
      val b = AST.Block(current)
      Result.pushElem(b)
    }

    def checkIfThereIsBlockInStack(): Unit = logger.trace {
      while (stack.nonEmpty && current < stack.head) {
        stack = stack.tail
        onPushingBlock()
      }
    }

    private def onPushingBlock(): Unit = logger.trace {
      val elems: List[Elem] = onFillingBlocks()
      Result.current match {
        case Some(b: Block) =>
          val block = AST.Block(b.indent, elems)
          Result.pushElem(block)
        case _ =>
          val block = AST.Block(current, elems)
          Result.pushElem(block)
      }
    }

    def onFillingBlocks(): List[Elem] = logger.trace {
      var elems: List[Elem] = Nil
      while (Result.stack.nonEmpty) {
        Result.pop()
        Result.current match {
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
      if (Result.stack.nonEmpty) {
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
      Result.pushElem(sp)
    }

    def onPushingNewLine(): Unit = logger.trace {
      Opr.onTraversingLineForOprs()
      val nl = AST.Newline()
      Result.pushElem(nl)
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

  /**
    * This is the End Of File object.
    * It is used to fill block after the end od analyzing code operation.
    * It also does a really important operation, as it connects all blocks found
    * in stack to appropriate methods.
    * After all processes end their job in EOF, stack is ready to create output
    * [[AST]]
    */
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
      val stack = connectBlocksToAppropriateMethods(Result.stack.reverse)
      Result.ast = Some(AST(stack))
    }
  }

  ROOT || eof || EOF.onEOF()

  /**
    * This is the undefined object.
    * If parser cannot match on a char, it wont fail, but will push
    * [[AST.Undefined]], which is known to be invalid AST.
    */
  //////////////////////////////////////////////////////////////////////////////
  //// Undefined ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  final object Undefined {
    def onPushing(in: String): Unit = logger.trace {
      val und = AST.Undefined(in)
      Result.pushElem(und)
    }
  }

  ROOT || any || Undefined.onPushing(currentMatch)
}
