package org.PseudoLang

import org.PseudoLang.syntax.text.ast.AST.Func
import org.PseudoLang.syntax.text.ast.AST.If
import org.PseudoLang.syntax.text.ast.Repr.R
import org.PseudoLang.syntax.text.ast.AST
import org.PseudoLang.syntax.text.ast.Repr
import sys.process._
import scala.sys.process.ProcessLogger
import scala.sys.process.stderr
import scala.sys.process.stdout

/**
  * This is the Transpiler.
  * It is used to change parsed text to Python code, by tail-recursively traversing
  * through AST
  */
object Transpiler {
  def run(ast: AST):       String       = transpile(ast).build()
  def transpile(ast: AST): Repr.Builder = traverse(0, ast.elems)

  /**
    * this function calls python compiler with transpiled code
    * @param name - name of file with generated code
    */
  def callPython(name: String): Unit = {
    val result = s"python3 $name.py" ! ProcessLogger(
        stdout append _,
        stderr append _
      )
    val a = result.toString.dropRight(1)
    println(a)
    println()
    println("stdout: " + stdout)
    println("stderr: " + stderr)
  }

  /**
    * This function traverses through AST to create Python code.
    * all functions under are it's helpers
    */
  private def traverse(indent: Int, stack: List[AST.Elem]): Repr.Builder = {
    stack match {
      case (f: AST.Func) :: rest        => transpileFunction(indent, f, rest)
      case (n: AST.Newline) :: rest     => R + n + indent + traverse(indent, rest)
      case (i: AST.If) :: rest          => transpileIf(indent, i, rest)
      case (t: AST.If.ThenCase) :: rest => transpileThen(indent, t, rest)
      case (e: AST.If.ElseCase) :: rest => transpileElse(indent, e, rest)
      case (l: AST.While) :: rest       => transpileWhile(indent, l, rest)
      case (l: AST.DoWhile) :: rest     => transpileDoWhile(indent, l, rest)
      case (l: AST.For) :: rest         => transpileFor(indent, l, rest)
      case (l: AST.RepeatUntil) :: rest => transpileRepeatUntil(indent, l, rest)
      case (o: AST.Opr) :: rest         => transpileOperator(indent, o, rest)
      case (_: AST.Comment) :: rest     => R + traverse(indent, rest)
      case (r: AST.Func.Return) :: rest => transpileReturn(indent, r, rest)
      case (a: AST.Array) :: rest       => transpileArray(indent, a, rest)
      case undefined :: rest            => R + undefined.repr + traverse(indent, rest)
      case Nil                          => R
    }
  }

  private def transpileReturn(
    indent: Int,
    r: Func.Return,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    R + "return " + traverse(0, r.value) + traverse(indent, rest)
  }

  private def transpileArray(
    indent: Int,
    a: AST.Array,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    R + a.name + traverseParens(a.elems) + traverse(indent, rest)
  }

  private def transpileRepeatUntil(
    indent: Int,
    l: AST.RepeatUntil,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    val bRepr = l.block match {
      case b: AST.Block => R + traverseBlock(b)
      case oth          => R + oth
    }
    R + AST.Newline() + indent + "while True:" + bRepr + AST
      .Newline() + l.block
      .asInstanceOf[AST.Block]
      .indent + "if not " + traverseParens(l.condition) + AST
      .Newline() + l.block
      .asInstanceOf[AST.Block]
      .indent + 4 + "break" + AST
      .Newline() + traverse(indent, rest) // [1]
  }

  private def transpileFor(
    indent: Int,
    l: AST.For,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    val bRepr = matchBlock(l.block)
    R + AST
      .Newline() + indent + "for " + traverseParens(l.condition) + ":" + bRepr + traverse(
      indent,
      rest
    )
  }

  private def transpileDoWhile(
    indent: Int,
    l: AST.DoWhile,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    val bRepr = matchBlock(l.block)
    R + AST.Newline() + indent + "while True:" + bRepr + AST
      .Newline() + l.block
      .asInstanceOf[AST.Block]
      .indent + "if " + traverseParens(l.condition) + AST
      .Newline() + l.block
      .asInstanceOf[AST.Block]
      .indent + 4 + "break" + AST
      .Newline() + traverse(indent, rest) // [2]
  }

  private def transpileWhile(
    indent: Int,
    l: AST.While,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    val bRepr = matchBlock(l.block)
    R + AST
      .Newline() + indent + "while " + traverseParens(l.condition) + ":" + bRepr + traverse(
      indent,
      rest
    )
  }

  private def transpileElse(
    indent: Int,
    e: If.ElseCase,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    val iRepr = e.e.head match {
      case i: If =>
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
  }

  private def transpileThen(
    indent: Int,
    t: If.ThenCase,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    val headRepr = t.e.head match {
      case _: AST.Spacing => R + traverse(indent, t.e.tail)
      case _              => R + traverse(indent, t.e)
    }
    R + headRepr + traverse(indent, rest)
  }

  private def transpileIf(
    indent: Int,
    i: AST.If,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    val ifRepr = R + "if" + traverseParens(i.condition) + ":"
    val bRepr  = matchBlock(i.block)
    R + AST.Newline() + indent + ifRepr + bRepr + traverse(indent, rest) + AST
      .Newline() + indent
  }

  private def transpileFunction(
    indent: Int,
    f: AST.Func,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    f.block match {
      case b: AST.Block =>
        val fDecl = R + "def " + f.name + traverseParens(f.args) + ":"
        R + AST.Newline() + indent + fDecl + traverseBlock(b) + traverse(
          indent,
          rest
        )
      case _ =>
        if (f.name.name == "length") {
          R + "len" + f.args + traverse(indent, rest)
        } else {
          R + f.name + f.args + traverse(indent, rest)
        }
    }
  }

  private def traverseParens(p: AST.Parens): Repr.Builder = {
    R + p.open + traverse(0, p.elems) + p.close
  }

  private def traverseBlock(b: AST.Block): Repr.Builder = {
    R + AST.Newline() + b.indent + traverse(b.indent, b.elems)
  }

  private def matchBlock(l: AST.Elem): Repr.Builder = {
    l match {
      case b: AST.Block => R + traverseBlock(b)
      case oth          => R + oth
    }
  }

  private def transpileOperator(
    indent: Int,
    o: AST.Opr,
    rest: List[AST.Elem]
  ): Repr.Builder = {
    R + traverseOpr(o, indent) + traverse(indent, rest)
  }

  private def traverseOpr(o: AST.Opr, indent: Int): Repr.Builder = {
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
