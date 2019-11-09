package org.PseudoLang.syntax.text.ast

import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.Repr._

sealed trait Symbol extends Repr.Provider {
  def show(): String = repr.build()

//  val scalaRepr: Repr.Builder
//  def generateScala(): String = scalaRepr.build()
}

////////////////////////////////////////////////////////////////////////////////
//// AST ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

final case class AST(elems: List[AST.Elem]) extends Symbol {
  val repr: Repr.Builder = R + elems
//  val scalaRepr: Repr.Builder = R + "object Main extends App {" + AST
//      .Newline() + elems.map(_.scalaRepr) + AST
//      .Newline() + "}"
}

object AST {
  def apply(): AST                 = new AST(Nil)
  def apply(elem: AST.Elem): AST   = new AST(elem :: Nil)
  def apply(elems: AST.Elem*): AST = new AST(elems.toList)

  sealed trait Elem extends Symbol
  object Elem {
    sealed trait Invalid extends Elem
  }

  case class Newline() extends Elem {
    val repr: Repr.Builder = R + "\n"
//    val scalaRepr: Repr.Builder = R + "\n"
  }

  case class Undefined(str: String) extends Elem.Invalid {
    val repr: Repr.Builder = R + str
//    val scalaRepr: Repr.Builder = R + str
  }
  case class Empty() extends Elem {
    val repr: Repr.Builder = R
//    val scalaRepr: Repr.Builder = R
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Variable ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Var(name: String) extends Elem {
    val repr: Repr.Builder = R + name
//    val scalaRepr: Repr.Builder = R + name
  }
  object Var {
    def apply(name: String) = new Var(name)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Operator ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Opr(marker: Opr.Marker, Le: Elem, Re: Elem) extends Elem {
    val repr: Repr.Builder = R + Le + " " + marker + " " + Re
//    val scalaRepr: Repr.Builder = {
//      val beginning: Builder = marker match {
//        case Opr.DefAndAssign => R + "var "
//        case _                => R
//      }
//      R + beginning + Le.scalaRepr + " " + marker.scalaRepr + " " + Re.scalaRepr
//    }
  }
  object Opr {
    def apply(m: Opr.Marker)                          = new Opr(m, Empty(), Empty())
    def apply(m: Opr.Marker, e: Elem)                 = new Opr(m, e, Empty())
    def apply(m: Opr.Marker, Le: Elem, Re: Elem): Opr = new Opr(m, Le, Re)

    abstract class Marker(val m: String) extends Elem {
      val repr: Repr.Builder = R + m
//      val scalaRepr: Repr.Builder = R + m
    }

    /* Arithmetic operators */
    case object Add extends Marker("+")
    case object Sub extends Marker("-")
    case object Mul extends Marker("*")
    case object Div extends Marker("/")
    case object Mod extends Marker("mod") {
//      override val scalaRepr: Builder = R + "%"
    }
    case object Pow extends Marker("^")

    case object DefAndAssign extends Marker("<--") {
//      override val scalaRepr: Builder = R + "="
    }
    case object Assign extends Marker("<-") {
//      override val scalaRepr: Builder = R + "="
    }
    case object TpAnn extends Marker(":")
    case object isEq extends Marker("=") {
//      override val scalaRepr: Builder = R + "=="
    }
    case object isGr     extends Marker(">")
    case object isLe     extends Marker("<")
    case object isGrOrEq extends Marker(">=")
    case object isLeOrEq extends Marker("<=")
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Spacing /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Spacing(len: Int) extends Elem {
    val repr: Repr.Builder = R + len
//    val scalaRepr: Repr.Builder = R + len
  }
  object Spacing {
    def apply(): Spacing         = new Spacing(1)
    def apply(len: Int): Spacing = new Spacing(len)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Comment /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Comment(str: String) extends Elem {
    val repr: Repr.Builder = R + Comment.marker + str
    //    val scalaRepr: Repr.Builder = R + Comment.marker + str
  }

  object Comment {
    val marker: String              = "//"
    def apply(str: String): Comment = new Comment(str)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Array ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Array(name: AST.Elem, str: String) extends Elem {
    val repr: Repr.Builder = R + name + "[" + str + "]"
  }

  object Array {
    def apply(str: String): Array                 = new Array(AST.Empty(), str)
    def apply(elem: AST.Elem, str: String): Array = new Array(elem, str)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Function ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Func(name: Var, block: AST.Elem, args: List[Var]) extends Elem {
    val repr: Repr.Builder = {
      val nameRepr = R + name + '('
      val argsRepr = {
        if (args.nonEmpty) {
          R + args.head + args.tail.map(R + ", " + _)
        } else {
          R
        }
      }
      val close = ')'
      R + nameRepr + argsRepr + close + block
    }

//    val scalaRepr: Repr.Builder = {
//      val nameRepr = R + "def " + name.scalaRepr + "[T]("
//      val argsRepr = {
//        if (args.nonEmpty) {
//          R + args.head.scalaRepr + ": T" + args.tail.map(
//            R + ", " + _.scalaRepr + ": T"
//          )
//        } else {
//          R
//        }
//      }
//      val close = "): Unit = "
//      R + nameRepr + argsRepr + close
//    }
  }
  object Func {
    def apply(name: Var): Func = new Func(name, AST.Empty(), Nil)
    def apply(name: Var, arg: AST.Var): Func =
      new Func(name, AST.Empty(), arg :: Nil)
    def apply(name: Var, args: AST.Var*): Func =
      new Func(name, AST.Empty(), args.toList)
    def apply(name: Var, block: AST.Block): Func = new Func(name, block, Nil)
    def apply(name: Var, block: AST.Block, arg: AST.Var): Func =
      new Func(name, block, arg :: Nil)
    def apply(name: Var, block: AST.Block, args: AST.Var*): Func =
      new Func(name, block, args.toList)

    case class Return(value: List[AST.Elem]) extends Elem {
      val repr: Repr.Builder = R + "Return " + value
//      val scalaRepr: Repr.Builder = R + "return " + value
    }
    case object Return {
      def apply(): Return                 = new Return(Nil)
      def apply(value: AST.Elem): Return  = new Return(value :: Nil)
      def apply(value: AST.Elem*): Return = new Return(value.toList)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Control Flow ////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class If(condition: String, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "If" + "(" + condition + ")" + block.repr
//    val scalaRepr
//      : Repr.Builder = R + "if" + "(" + condition + ")" + block.scalaRepr
  }
  object If {
    def apply(condition: String): If                  = new If(condition, AST.Empty())
    def apply(condition: String, block: AST.Elem): If = new If(condition, block)

    case class ElseCase(e: List[AST.Elem]) extends Elem {
      val repr: Repr.Builder = R + "Else " + e
//      val scalaRepr: Repr.Builder = R + "else " + block.scalaRepr
    }
    object ElseCase {
      def apply(): ElseCase             = new ElseCase(Nil)
      def apply(e: AST.Elem): ElseCase  = new ElseCase(e :: Nil)
      def apply(e: AST.Elem*): ElseCase = new ElseCase(e.toList)
    }

    case class ThenCase(e: List[AST.Elem]) extends Elem {
      val repr: Repr.Builder = R + "Then " + e
//      val scalaRepr: Repr.Builder = R + "then " + block.scalaRepr
    }
    object ThenCase {
      def apply(): ThenCase             = new ThenCase(Nil)
      def apply(e: AST.Elem): ThenCase  = new ThenCase(e :: Nil)
      def apply(e: AST.Elem*): ThenCase = new ThenCase(e.toList)
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Loops ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class While(condition: String, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "While" + "(" + condition + ")" + block.repr
//    val scalaRepr
//      : Repr.Builder = R + "while" + "(" + condition + ")" + block.scalaRepr
  }
  object While {
    def apply(condition: String): While = new While(condition, AST.Empty())
    def apply(condition: String, block: AST.Elem): While =
      new While(condition, block)
  }

  case class DoWhile(condition: String, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "Do" + block.repr + "While (" + condition + ")"
//    val scalaRepr
//      : Repr.Builder = R + "do" + block.scalaRepr + "while (" + condition + ")"
  }
  object DoWhile {
    def apply(): DoWhile                  = new DoWhile("", AST.Empty())
    def apply(condition: String): DoWhile = new DoWhile(condition, AST.Empty())
    def apply(condition: String, block: AST.Elem): DoWhile =
      new DoWhile(condition, block)
  }

  case class For(condition: String, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "For" + "(" + condition + ")" + block.repr
//    val scalaRepr
//      : Repr.Builder = R + "for" + "(" + condition + ")" + block.scalaRepr
  }
  object For {
    def apply(condition: String): For = new For(condition, AST.Empty())
    def apply(condition: String, block: AST.Elem): For =
      new For(condition, block)
  }

  case class RepeatUntil(condition: String, block: AST.Elem) extends Elem {
    val repr
      : Repr.Builder = R + "Repeat" + block.repr + "Until (" + condition + ")"
//    val scalaRepr
//      : Repr.Builder = R + "do" + block.scalaRepr + "while !(" + condition + ")"
  }
  object RepeatUntil {
    def apply(): RepeatUntil = new RepeatUntil("", AST.Empty())
    def apply(condition: String): RepeatUntil =
      new RepeatUntil(condition, AST.Empty())
    def apply(condition: String, block: AST.Elem): RepeatUntil =
      new RepeatUntil(condition, block)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Block ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Block(indent: Int, elems: List[Elem]) extends Elem {
    val repr: Repr.Builder = R + Newline() + indent + elems.map {
        case elem: Newline => R + elem + indent
        case b: AST.Block  => R + b + indent
        case elem          => R + elem
      } + Newline()

//    val scalaRepr: Repr.Builder = R + "{" + Newline() + indent + elems.map {
//        case n: Newline   => R + n + indent
//        case b: AST.Block => R + b.scalaRepr + indent
//        case elem         => R + elem.scalaRepr
//      } + Newline() + (indent - 2) + "}"
  }
  object Block {
    def apply(): Block                 = new Block(0, Nil)
    def apply(elem: AST.Elem): Block   = new Block(0, elem :: Nil)
    def apply(elems: AST.Elem*): Block = new Block(0, elems.toList)
    def apply(indent: Int): Block      = new Block(indent, Nil)
    def apply(indent: Int, elem: AST.Elem): Block =
      new Block(indent, elem :: Nil)
    def apply(indent: Int, elems: AST.Elem*): Block =
      new Block(indent, elems.toList)
  }
}
