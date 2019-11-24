package org.PseudoLang.syntax.text.ast

import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.Repr._

/**
  * This is the symbol trait
  * It is the most primitive element, on top of which AST is built. It is used to
  * extend [[Repr.Provider]], a better implementation of StringBuilder for the
  * needs of Parser.
  */
sealed trait Symbol extends Repr.Provider {
  def show(): String = repr.build()
}

////////////////////////////////////////////////////////////////////////////////
//// AST ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
/**
  * This is the [[AST]] class.
  * It is created to output ready AST, contains the parsed stack.
  * @param elems - elements in AST
  */
final case class AST(elems: List[AST.Elem]) extends Symbol {
  val repr: Repr.Builder = R + elems
}

object AST {
  def apply(): AST                 = new AST(Nil)
  def apply(elem: AST.Elem): AST   = new AST(elem :: Nil)
  def apply(elems: AST.Elem*): AST = new AST(elems.toList)

  /**
    * This is the [[AST.Elem]].
    * It is the simplest element, extends Symbol, used as a in-AST implementation
    * of Symbol.
    */
  sealed trait Elem extends Symbol

  /**
    * This is the [[AST.Newline]].
    * It is used to store new line symbol
    */
  case class Newline() extends Elem {
    val repr: Repr.Builder = R + "\n"
  }

  /**
    * This is the [[AST.Undefined]].
    * It is used to store elements, which couldn't be matched by parser
    * @param str - undefined element
    */
  case class Undefined(str: String) extends Elem {
    val repr: Repr.Builder = R + str
  }

  /**
    * This is the [[AST.Empty]].
    * It doesn't do anything, just is being used to avoid use of [[Option]]
    */
  case class Empty() extends Elem {
    val repr: Repr.Builder = R
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Variable ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  /**
    * This is the [[AST.Var]].
    * It is used to store simple variables.
    * @param name - specifies name of variable
    */
  case class Var(name: String) extends Elem {
    val repr: Repr.Builder = R + name
  }
  object Var {
    def apply(name: String) = new Var(name)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Operator ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  /**
    * This is the [[AST.Opr]].
    * It is used to store expressions with operators
    * @param marker - specifies operator marker from [[AST.Opr.Marker]]
    */
  case class Opr(marker: Opr.Marker, Le: Elem, Re: Elem) extends Elem {
    val repr: Repr.Builder = R + Le + " " + marker + " " + Re
  }
  object Opr {
    def apply(m: Opr.Marker)                          = new Opr(m, Empty(), Empty())
    def apply(m: Opr.Marker, e: Elem)                 = new Opr(m, e, Empty())
    def apply(m: Opr.Marker, Le: Elem, Re: Elem): Opr = new Opr(m, Le, Re)

    /**
      * This is the [[AST.Opr.Marker]].
      * It is used to store operator's marker
      * @param m - specifies markers textual representation
      */
    abstract class Marker(val m: String) extends Elem {
      val repr: Repr.Builder = R + m
    }

    /* Arithmetic operators */
    case object Add      extends Marker("+")
    case object Sub      extends Marker("-")
    case object Mul      extends Marker("*")
    case object Div      extends Marker("/")
    case object Mod      extends Marker("mod")
    case object Pow      extends Marker("^")
    case object FloorDiv extends Marker("/f")
    /* Logical operators */
    case object isEq     extends Marker("=")
    case object isGr     extends Marker(">")
    case object isLe     extends Marker("<")
    case object isGrOrEq extends Marker(">=")
    case object isLeOrEq extends Marker("<=")
    case object isNotEq  extends Marker("<>")
    case object And      extends Marker("&")
    case object Or       extends Marker("|")
    case object Not      extends Marker("!")
    case object Assign   extends Marker("<-")
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Spacing /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  /**
    * This is the [[AST.Spacing]].
    * It provides support for spacing between elements in AST
    * @param len - specifies number of spaces
    */
  case class Spacing(len: Int) extends Elem {
    val repr: Repr.Builder = R + len
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
  }

  object Comment {
    val marker: String              = "//"
    def apply(str: String): Comment = new Comment(str)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Array ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Array(name: AST.Elem, elems: AST.Parens) extends Elem {
    val repr: Repr.Builder = R + name + "[" + elems + "]"
  }

  object Array {
    def apply(par: AST.Parens): Array                 = new Array(AST.Empty(), par)
    def apply(elem: AST.Elem, par: AST.Parens): Array = new Array(elem, par)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Parentheses /////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Parens(open: Char, close: Char, elems: List[AST.Elem])
      extends Elem {
    val repr: Repr.Builder = R + open + elems.map(_.repr) + close
  }

  object Parens {
    def apply(): Parens                 = new Parens('(', ')', List())
    def apply(elem: AST.Elem): Parens   = new Parens('(', ')', elem :: Nil)
    def apply(elems: AST.Elem*): Parens = new Parens('(', ')', elems.toList)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Function ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Func(name: Var, block: AST.Elem, args: AST.Parens) extends Elem {
    val repr: Repr.Builder = R + name + args + block
  }
  object Func {
    def apply(name: Var): Func =
      new Func(name, AST.Empty(), AST.Parens())
    def apply(name: Var, block: AST.Block): Func =
      new Func(name, block, AST.Parens())
    def apply(name: Var, par: AST.Parens): Func =
      new Func(name, AST.Empty(), par)
    def apply(name: Var, block: AST.Block, par: AST.Parens): Func =
      new Func(name, block, par)

    case class Return(value: List[AST.Elem]) extends Elem {
      val repr: Repr.Builder = R + "Return " + value
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
  case class If(condition: AST.Parens, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "If" + condition + block.repr
  }
  object If {
    def apply(condition: AST.Parens): If = new If(condition, AST.Empty())
    def apply(condition: AST.Parens, block: AST.Elem): If =
      new If(condition, block)

    case class ElseCase(e: List[AST.Elem]) extends Elem {
      val repr: Repr.Builder = R + "Else " + e
    }
    object ElseCase {
      def apply(): ElseCase             = new ElseCase(Nil)
      def apply(e: AST.Elem): ElseCase  = new ElseCase(e :: Nil)
      def apply(e: AST.Elem*): ElseCase = new ElseCase(e.toList)
    }

    case class ThenCase(e: List[AST.Elem]) extends Elem {
      val repr: Repr.Builder = R + "Then " + e
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
  case class While(condition: AST.Parens, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "While" + condition + block.repr
  }
  object While {
    def apply(condition: AST.Parens): While = new While(condition, AST.Empty())
    def apply(condition: AST.Parens, block: AST.Elem): While =
      new While(condition, block)
  }

  case class DoWhile(condition: AST.Parens, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "Do" + block.repr + "While " + condition
  }
  object DoWhile {
    def apply(): DoWhile = new DoWhile(AST.Parens(), AST.Empty())
    def apply(condition: AST.Parens): DoWhile =
      new DoWhile(condition, AST.Empty())
    def apply(condition: AST.Parens, block: AST.Elem): DoWhile =
      new DoWhile(condition, block)
  }

  case class For(condition: AST.Parens, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "For" + condition + block.repr
  }
  object For {
    def apply(condition: AST.Parens): For = new For(condition, AST.Empty())
    def apply(condition: AST.Parens, block: AST.Elem): For =
      new For(condition, block)
  }

  case class RepeatUntil(condition: AST.Parens, block: AST.Elem) extends Elem {
    val repr: Repr.Builder = R + "Repeat" + block.repr + "Until " + condition
  }
  object RepeatUntil {
    def apply(): RepeatUntil =
      new RepeatUntil(AST.Parens(), AST.Empty())
    def apply(condition: AST.Parens): RepeatUntil =
      new RepeatUntil(condition, AST.Empty())
    def apply(condition: AST.Parens, block: AST.Elem): RepeatUntil =
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
