package org.enso.syntax.text.ast

import org.enso.data.ADT
import org.enso.data.List1
import org.enso.syntax.text.ast.Repr.R

sealed trait Symbol extends Repr.Provider {
  def show() = repr.build()
}

////////////////////////////////////////////////////////////////////////////////
//// AST ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

final case class AST(elems: Option[AST]) extends Symbol {
  val repr: Repr.Builder = R + elems
}

object AST {
  def apply(): AST = AST(None)

  sealed trait Elem extends Symbol
  object Elem {
    sealed trait Invalid extends Elem

    case object Newline extends Elem {
      val repr: Repr.Builder = R + "\n"
    }
  }
}
