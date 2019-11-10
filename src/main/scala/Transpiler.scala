package org.PseudoLang

import org.PseudoLang.syntax.text.ast.AST
import org.enso.flexer.Parser.Result.Failure
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.Repr._

////////////////////////////////////////////////////////////////////////////////
//// Transpiler ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
object Transpiler {
  def run(ast: AST):       String       = transpile(ast).build()
  def transpile(ast: AST): Repr.Builder = traverse(0, ast.elems)

  def traverse(indent: Int, stack: List[AST.Elem]): Repr.Builder = {
    stack match {
      case (f: AST.Func) :: rest =>
        f.block match {
          case b: AST.Block =>
            val fDecl = R + "def " + f.name + f.args + ":"
            R + fDecl + AST
              .Newline() + b.indent + traverse(b.indent, b.elems) + traverse(
              indent,
              rest
            )
          case _ => R + f.name + f.args + traverse(indent, rest)
        }
      case (n: AST.Newline) :: rest => R + n + indent + traverse(indent, rest)
      case (i: AST.If) :: rest =>
        i.block match {
          case b: AST.Block =>
            R + "if" + i.condition + ":" + AST
              .Newline() + b.indent + traverse(b.indent, b.elems) + traverse(
              indent,
              rest
            )
          case _ => R + "if" + i.condition + traverse(indent, rest)
        }
      case (t: AST.If.ThenCase) :: rest =>
        R + traverse(indent, t.e.tail) + traverse(indent, rest)
      case (c: AST.Comment) :: rest => R + traverse(indent, rest)
      case undefined :: rest        => R + undefined.repr + traverse(indent, rest)
      case Nil                      => R
    }
  }
}