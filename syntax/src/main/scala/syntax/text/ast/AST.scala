package org.PseudoLang.syntax.text.ast

import org.enso.data.ADT
import org.enso.data.List1
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.Repr._

sealed trait Symbol extends Repr.Provider {
  def show() = repr.build()
}

////////////////////////////////////////////////////////////////////////////////
//// AST ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

final case class AST(elems: List[AST.Elem]) extends Symbol {
  val repr: Repr.Builder = R + elems
}

object AST {
  def apply(): AST = new AST(List())
  def apply(elem: AST.Elem): AST = new AST(List(elem))
  def apply(elems: AST.Elem*): AST = new AST(elems.toList)

  sealed trait Elem extends Symbol
  object Elem {
    sealed trait Invalid extends Elem

    case object Newline extends Elem {
      val repr: Repr.Builder = R + "\n"
    }
  }

  case class Undefined(str: String) extends Elem.Invalid {
    val repr: Repr.Builder = R + str
  }
  object Undefined {
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Variable ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Var(name: String, tp: Option[String]) extends Elem {
    val repr: Repr.Builder = {
      val nameRepr = R + name
      val tpRepr = tp match {
        case Some(v) => R + ": " + tp
        case None => R
      }
      R + nameRepr + tpRepr
    }
  }
  object Var {
    def apply(name: String, tp: String) = new Var(name, Some(tp))
    def apply(name: String) = new Var(name, None)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Comment /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Comment(str: String) extends Elem {
    val marker: String = "//"
    val repr: Repr.Builder = R + marker + str
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Function ////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  case class Func(name: Var, args: List[Var]) extends Elem {
    val repr: Repr.Builder = {
      val nameRepr = R + name + '('
      val argsRepr = { if (args.length > 0) {
         R + args.head + args.tail.map(R + ", " + _.repr)
       } else {
         R
       }
      }
      val close = ')'
      R + nameRepr + argsRepr + close
    }
  }
  object Func {
    def apply(name: Var): Func = new Func(name, List())
    def apply(name: Var, arg: AST.Var): Func = new Func(name, List(arg))
    def apply(name: Var, args: AST.Var*): Func = new Func(name, args.toList)
  }
}
