package org.PseudoLang

import org.PseudoLang.syntax.text.Parser
import org.enso.debug._

//////////////
//// Main ////
//////////////

object Main extends App {
  //////////////////////////////////////////////////////////////////////////////
  //// PseudoLang interactive testing environmnet //////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  println("======================= PSEUDO LANG ========================")
  val code   = FileManager.readFileWithPseudo("", "Main")
  val parsed = new Parser().runMatched(code)
//  pprint.pprintln(parsed)
  println(Debug.pretty(parsed.toString))
  println("========================== CODE ============================")
  println(parsed.show())
//  println("======================= SCALA CODE =========================")
//  val scalaCode = parsed.generateScala()
//  println(scalaCode)
//  FileManager.saveScalaCodeToFile("", "Generated", scalaCode)
//  println("============================================================")
}
