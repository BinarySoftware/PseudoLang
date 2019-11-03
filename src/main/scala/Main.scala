package org.PseudoLang

import org.PseudoLang.syntax.text.Parser
import org.PseudoLang.PrettyPrinter

//////////////
//// Main ////
//////////////

object Main extends App {
  //////////////////////////////////////////////////////////////////////////////
  //// PseudoLang interactive testing environmnet///////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  val inp =
    """Foo// Bez typu
      |Bar: Int
      |Function()
      |Function(a,b)""".stripMargin

  /** Invoking the  Parser */
  println("===== PSEUDO PARSER =====")
  val parsed    = new Parser().runMatched(inp)
  pprint.pprintln(parsed)
//  println(PrettyPrinter.pretty(parsed.toString))
  println("------")
  println(parsed.show())
  println("=========================")
}