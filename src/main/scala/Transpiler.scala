package org.PseudoLang

import org.PseudoLang.syntax.text.ast.AST
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.Repr._
import sys.process._

////////////////////////////////////////////////////////////////////////////////
//// Transpiler ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
object Transpiler {
  def run(ast: AST): String             = transpile(ast).build()
  def transpile(ast: AST): Repr.Builder = traverse(0, ast.elems)

  def callPython(name: String): Unit = {
    val result = s"python3 $name.py" ! ProcessLogger(
        stdout append _,
        stderr append _
      )
    println(result.toString.dropRight(1))
    println("stdout: " + stdout)
    println("stderr: " + stderr)
  }

  def traverse(indent: Int, stack: List[AST.Elem]): Repr.Builder = {
    stack match {
      case (f: AST.Func) :: rest =>
        f.block match {
          case b: AST.Block =>
            val fDecl = R + "def " + f.name + traverseParens(f.args) + ":"
            R + fDecl + traverseBlock(b) + traverse(indent, rest)
          case _ =>
            if (f.name.name == "length") {
              R + "len" + f.args + traverse(indent, rest)
            } else {
              R + f.name + f.args + traverse(indent, rest)
            }
        }
      case (n: AST.Newline) :: rest => R + n + indent + traverse(indent, rest)
      case (i: AST.If) :: rest =>
        val ifRepr = R + "if" + traverseParens(i.condition) + ":"
        val bRepr = i.block match {
          case b: AST.Block => R + traverseBlock(b)
          case oth          => R + oth
        }
        R + ifRepr + bRepr + traverse(indent, rest)
      case (t: AST.If.ThenCase) :: rest =>
        val headRepr = t.e.head match {
          case _: AST.Spacing => R + traverse(indent, t.e.tail)
          case _              => R + traverse(indent, t.e)
        }
        R + headRepr + traverse(indent, rest)
      case (e: AST.If.ElseCase) :: rest =>
        val iRepr = e.e.head match {
          case i: AST.If =>
            val elRepr = R + "elif " + traverseParens(i.condition) + ":"
            val bRepr = i.block match {
              case b: AST.Block => traverseBlock(b)
              case oth          => oth.repr
            }
            R + elRepr + bRepr + traverse(indent, e.e.tail)
          case _: AST.Spacing => R + "else: " + traverse(indent, e.e.tail)
          case _              => R + "else: " + traverse(indent, e.e)
        }
        R + iRepr + traverse(indent, rest)
      case (l: AST.While) :: rest =>
        val bRepr = l.block match {
          case b: AST.Block => R + traverseBlock(b)
          case oth          => R + oth
        }
        R + "while " + traverseParens(l.condition) + ":" + bRepr + traverse(
          indent,
          rest
        )
      case (l: AST.DoWhile) :: rest =>
        val bRepr = l.block match {
          case b: AST.Block => R + traverseBlock(b)
          case oth          => R + oth
        }
        R + "while True:" + bRepr + AST
          .Newline() + l.block
          .asInstanceOf[AST.Block]
          .indent + "if " + traverseParens(l.condition) + AST
          .Newline() + l.block
          .asInstanceOf[AST.Block]
          .indent + 4 + "break" + AST
          .Newline() + traverse(indent, rest)
      case (l: AST.For) :: rest =>
        val bRepr = l.block match {
          case b: AST.Block => R + traverseBlock(b)
          case oth          => R + oth
        }
        R + "for " + traverseParens(l.condition) + ":" + bRepr + traverse(
          indent,
          rest
        )
      case (l: AST.RepeatUntil) :: rest =>
        val bRepr = l.block match {
          case b: AST.Block => R + traverseBlock(b)
          case oth          => R + oth
        }
        R + "while True:" + bRepr + AST
          .Newline() + l.block
          .asInstanceOf[AST.Block]
          .indent + "if !" + traverseParens(l.condition) + AST
          .Newline() + l.block
          .asInstanceOf[AST.Block]
          .indent + 4 + "break" + AST
          .Newline() + traverse(indent, rest) // [1]
      case (o: AST.Opr) :: rest =>
        R + traverseOpr(o, indent) + traverse(indent, rest)
      case (_: AST.Comment) :: rest => R + traverse(indent, rest)
      case (r: AST.Func.Return) :: rest =>
        R + "return " + traverse(0, r.value) + traverse(indent, rest)
      case (a: AST.Array) :: rest =>
        R + a.name + traverseParens(a.elems) + traverse(
          indent,
          rest
        )
      case undefined :: rest => R + undefined.repr + traverse(indent, rest)
      case Nil               => R
    }
  }

  def traverseParens(p: AST.Parens): Repr.Builder = {
    R + p.open + traverse(0, p.elems) + p.close
  }

  def traverseBlock(b: AST.Block): Repr.Builder = {
    R + AST.Newline() + b.indent + traverse(b.indent, b.elems)
  }

  def traverseOpr(o: AST.Opr, indent: Int): Repr.Builder = {
    val lef = traverse(indent, o.Le :: Nil)
    val rig = traverse(indent, o.Re :: Nil)
    o.marker match {
      case AST.Opr.isEq     => R + lef + 1 + "==" + 1 + rig
      case AST.Opr.Assign   => R + lef + 1 + "=" + 1 + rig
      case AST.Opr.Mod      => R + lef + 1 + "%" + 1 + rig
      case AST.Opr.FloorDiv => R + lef + 1 + "//" + 1 + rig
      case oth              => R + lef + 1 + oth + 1 + rig
    }
  }
}
/*
 * Note [1]
 *
 * There is no implementation of repeat until loop in Py
 * But it can be easily replaced with do..while loop with negated condition
 *
 * Note [2]
 *
 * This is uuugly. But it works. If it works then it isn't ugly anymore
 */
