package org.PseudoLang

import org.PseudoLang.syntax.text.Parser
import java.io.File
import java.io.PrintWriter

import scala.io.Source

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
  println(PrettyPrinter.pretty(parsed.toString))
  println("========================== CODE ============================")
  println(parsed.show())
  println("======================= SCALA CODE =========================")
  val scalaCode = parsed.generateScala()
  println(scalaCode)
  // FileManager.saveScalaCodeToFile("", "Generated", scalaCode)
  println("============================================================")
}

object FileManager {

  def readFileWithPseudo(path: String, name: String): String = {
    val bufferedSource = Source.fromFile(path + name + ".pseudo")
    val code           = bufferedSource.getLines.mkString("\n")
    bufferedSource.close
    code
  }

  def saveScalaCodeToFile(
    path: String,
    name: String,
    code: String
  ): Unit = {
    val writer = new PrintWriter(new File(path + name + ".scala"))
    writer.write(code.toString)
    writer.close()
  }
}
