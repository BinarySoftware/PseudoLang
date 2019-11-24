package org.PseudoLang.syntax.text

import org.PseudoLang.Parser
import org.PseudoLang.syntax.text.ast.AST
import org.enso.flexer.Parser.Result
import org.scalameter.api._
import org.scalameter.execution.LocalExecutor
import org.scalameter.picklers.Implicits._

import scala.math.pow

object ParserBench extends Bench.OfflineRegressionReport {

  override def executor = new LocalExecutor(warmer, aggregator, measurer)

  val range = 0
  def exp(i: Int): Gen[Int] =
    Gen.exponential("size")(pow(2, i - range).toInt, pow(2, i).toInt, 2)

  def gen(range: Gen[Int], f: Int => String): Gen[String] =
    for { i <- range } yield f(i)

  val tests = List(
    "variables" -> gen(exp(12), i => "aaaaaaaaaa\n" * i),
    "functions" -> gen(exp(12), i => "function()\n" * i),
    "operators" -> gen(exp(12), i => "a<-b+c*d/e\n" * i),
    "blocks"    -> gen(exp(12), i => "of\n  b\n c\n" * i),
    "arrays"    -> gen(exp(12), i => "f0o[1,3,4]\n" * i)
  )

  def run(str: String): Result[AST] = Parser.run(str)
  performance of "Parser" in {
    tests.foreach {
      case (name, gen) => measure method name in (using(gen) in run)
    }
  }
}
