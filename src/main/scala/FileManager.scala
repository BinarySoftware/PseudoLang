package org.PseudoLang

import java.io.File
import java.io.PrintWriter
import scala.io.Source

////////////////////////////////////////////////////////////////////////////////
//// File Manager //////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

object FileManager {
  def readFileWithPseudo(path: String, name: String): String = {
    val bufferedSource = Source.fromFile(path + name + ".pseudo")
    val code           = bufferedSource.getLines.mkString("\n")
    bufferedSource.close
    code
  }

  def saveCodeToFile(
    path: String,
    name: String,
    code: String,
    tp: String
  ): Unit = {
    val writer = new PrintWriter(new File(path + name + "." + tp))
    writer.write(code.toString)
    writer.close()
  }
}
