package org.PseudoLang

import org.PseudoLang.syntax.text.Parser
import org.enso.debug._
import sys.process._

////////////////////////////////////////////////////////////////////////////////
//// Main //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

object Main extends App {
  //////////////////////////////////////////////////////////////////////////////
  //// PseudoLang interactive running environmnet //////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  println("======================= PSEUDO LANG ========================")
  val code   = FileManager.readFileWithPseudo("", "Main")
  val parsed = new Parser().runMatched(code)
  println("========================== AST =============================")
  println(Debug.pretty(parsed.toString))
  println("========================== CODE ============================")
  println(parsed.show())
  println("======================= TRANSPILED =========================")
  val transpiled = Transpiler.run(parsed)
  println(transpiled)
  val name = "Generated"
  FileManager.saveCodeToFile("", name, transpiled, "py")
  println("========================= OUTPUT ===========================")
  Transpiler.callPython(name)
  println("============================================================")
}
