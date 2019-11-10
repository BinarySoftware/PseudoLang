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
      case (f: AST.Func) :: rest =>
        f.block match {
          case b: AST.Block =>
            val fDecl = R + "def " + f.name + f.args + ":"
            R + fDecl + AST.Newline() + b.indent + traverse(b.elems) + traverse(
              rest
            )
          case _ => R + f.name + f.args + traverse(rest)
        }
      case (c: AST.Comment) :: rest => R + traverse(rest)
      case undefined :: rest        => R + undefined.repr + traverse(rest)
      case Nil                      => R
    }
  }
}
