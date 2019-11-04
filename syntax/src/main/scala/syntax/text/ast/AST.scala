package org.PseudoLang.syntax.text.ast

import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.Repr._

sealed trait Symbol extends Repr.Provider {
  def show(): String = repr.build()
}

////////////////////////////////////////////////////////////////////////////////
//// AST ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

final case class AST(elems: List[AST.Elem]) extends Symbol {
  val repr: Repr.Builder = R + elems
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
  }

  case class Undefined(str: String) extends Elem.Invalid {
    val repr: Repr.Builder = R + str
  }
  object Empty extends Elem {
    val repr: Repr.Builder = R
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Variable ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Var(name: String, tp: Option[String]) extends Elem {
    val repr: Repr.Builder = {
      val nameRepr = R + name
      val tpRepr = tp match {
        case Some(v) => R + ": " + v
        case None    => R
      }
      R + nameRepr + tpRepr
    }
  }
  object Var {
    def apply(name: String, tp: String) = new Var(name, Some(tp))
    def apply(name: String)             = new Var(name, None)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Spacing /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
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
    val marker: String     = "//"
    val repr: Repr.Builder = R + marker + str
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Function ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Func(name: Var, args: List[Var]) extends Elem {
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
      R + nameRepr + argsRepr + close
    }
  }
  object Func {
    def apply(name: Var): Func                 = new Func(name, Nil)
    def apply(name: Var, arg: AST.Var): Func   = new Func(name, arg :: Nil)
    def apply(name: Var, args: AST.Var*): Func = new Func(name, args.toList)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Block ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Block(indent: Int, elems: List[Elem]) extends Elem {
    val repr: Repr.Builder = R + Newline() + indent + elems.map {
        case elem: Newline => R + elem + indent
        case b: AST.Block  => R + b.repr + indent
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
