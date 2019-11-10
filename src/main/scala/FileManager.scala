package org.PseudoLang

import java.io.File
import java.io.PrintWriter
import scala.io.Source

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
