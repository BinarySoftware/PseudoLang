package org.PseudoLang

import org.PseudoLang.syntax.text.ast.AST
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.Repr._

////////////////////////////////////////////////////////////////////////////////
//// Transpiler ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
object Transpiler {
  def run(ast: AST):       String       = transpile(ast).build()
  def transpile(ast: AST): Repr.Builder = traverse(ast.elems)

  def traverse(stack: List[AST.Elem]): Repr.Builder = {
    stack match {
      case elem :: rest => R + elem.repr + traverse(rest)
      case Nil          => R
    }
  }
}
