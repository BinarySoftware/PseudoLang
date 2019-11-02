package org.enso.syntax

import org.enso.syntax.text.Parser
import org.enso.syntax.text.ast.AST
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
    "funtions" -> gen(exp(14), i => "function ()\n" * i)
  )

  def run(str: String): Result[AST] = Parser.run(str)
  performance of "Parser" in {
    tests.foreach {
      case (name, gen) => measure method name in (using(gen) in run)
    }
  }
}
