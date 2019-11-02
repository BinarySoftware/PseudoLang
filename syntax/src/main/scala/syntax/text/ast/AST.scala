package org.enso.syntax.text.ast

import com.sun.xml.internal.bind.v2.model.core.NonElement
import org.enso.data.ADT
import org.enso.data.List1
import org.enso.syntax.text.ast.Repr.R

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
  ////////////////////////////////////////////w//////////////////////////////////
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

  case class Comment(str: String) extends Elem {
    val marker: String = "//"
    val repr: Repr.Builder = R + marker + str
  }
}
