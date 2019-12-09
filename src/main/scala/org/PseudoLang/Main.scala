package org.PseudoLang

/**
  * This is the Main object of PseudoLang. This is essentially the starting point
  * from which the parser and transpiler is being called. It contains 2 methods
  * for different running modes - with or without debugging
  */
object Main extends App {
  //////////////////////////////////////////////////////////////////////////////
  //// PseudoLang running environment //////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  val code       = FileManager.readFileWithPseudo("", "Main")
  val parsed     = new Parser().runMatched(code)
  val transpiled = Transpiler.run(parsed)
  val name       = "Generated"
  FileManager.saveCodeToFile("", name, transpiled, "py")

  def runParserWD(): Unit = {
    println("========================= INPUT ============================")
    println(code)
    println("========================== AST =============================")
    println(Debug.pretty(parsed.toString))
    println("========================== CODE ============================")
    println(parsed.show())
    println("============================================================")
  }

  def runWithDebugging(): Unit = {
    println("======================= PSEUDO LANG ========================")
    println("========================= INPUT ============================")
    println(code)
    println("========================== AST =============================")
    println(Debug.pretty(parsed.toString))
    println("========================== CODE ============================")
    println(parsed.show())
    println("======================= TRANSPILED =========================")
    println(transpiled)
    println("========================= OUTPUT ===========================")
    Transpiler.callPython(name)
    println("============================================================")
  }

  def runWithoutDebugging(): Unit = {
    println("======================= PSEUDO LANG ========================")
    println("========================= INPUT ============================")
    println(code)
    println("========================= OUTPUT ===========================")
    Transpiler.callPython(name)
    println("============================================================")
  }

//  runWithoutDebugging()
//  runWithDebugging()
  runParserWD()
}
