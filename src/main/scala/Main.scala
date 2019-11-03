package org.PseudoLang

import org.PseudoLang.syntax.text.Parser

//////////////
//// Main ////
//////////////

object Main extends App {
  //////////////////////////////////////////////////////////////////////////////
  //// PseudoLang interactive testing environmnet //////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  val inp =
    """Foo  // Bez typu
      |Bar: Int    //foo
      |Function()
      |  Foo <- 2
      |  Bar <- Foo*2 //4
      |Function(a,b)""".stripMargin

  /** Invoking the  Parser */
  println("===== PSEUDO PARSER =====")
  val parsed = new Parser().runMatched(inp)
  pprint.pprintln(parsed)
//  println(PrettyPrinter.pretty(parsed.toString))
  println("------")
  println(parsed.show())
  println("=========================")
}
