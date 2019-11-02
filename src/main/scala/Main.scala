package org.enso.syntax.text

import scala.annotation.tailrec

//////////////
//// Main ////
//////////////

object Main extends App {

  def pretty(str: String): String = {

    def checkClosing(in: List[Char]): Int = {
      @tailrec
      def go(i: Int, rest: Int, in: List[Char], bias: Int): Int =
        (rest, bias, in) match {
          case (0, _, _)   => 0
          case (_, 0, _)   => i
          case (_, _, Nil) => i
          case (_, _, s :: ss) =>
            s match {
              case '(' => go(i + 1, rest - 1, ss, bias - 1)
              case ')' => go(i + 1, rest - 1, ss, bias + 1)
              case _   => go(i + 1, rest - 1, ss, bias)
            }

        }
      go(0, 10, in, -1)
    }

    @tailrec
    def go(ind: Int, in: List[Char], out: List[String]): List[String] = {
      def newline(i: Int) = "\n" + " " * i * 2
      in match {
        case Nil => out
        case s :: ss =>
          val s2 = s.toString
          s match {
            case '(' =>
              checkClosing(ss) match {
                case 0 => go(ind + 1, ss, newline(ind + 1) :: s2 :: out)
                case i =>
                  go(
                    ind,
                    ss.drop(i),
                    ss.take(i).mkString("") :: s2 :: out
                  )
              }

            case ')' => go(ind - 1, ss, s2 :: newline(ind - 1) :: out)
            case ',' => go(ind, ss, newline(ind) :: s2 :: out)
            case _   => go(ind, ss, s2 :: out)
          }
      }
    }
    go(0, str.toList, List()).reverse.mkString("")
  }

  val inp =
    """ DEPRECATED
      | REMOVED - replaced by SwiftUI
      | ADDED
      | MODIFIED
      | UPCOMING
      | ALAMAKOTA a kot ma Ale
      | Construct and manage a graphical, event-driven user interface for your
      | iOS or tvOS app.
      |
      | The UIKit framework provides the required infrastructure for your iOS or
      | tvOS apps. It provides the window and view architecture for implementing
      | your interface, the event handling infrastructure for delivering Multi-
      | Touch and other types of input to your app, and the main run loop needed
      | to manage interactions among the user, the system, and your app. Other
      | features offered by the framework include animation support, document
      | support, drawing and printing support, information about the current
      | device, text management and display, search support, accessibility
      | support, app extension support, and resource management.
      |
      | ! Important
      |   Use UIKit classes only from your app’s main thread or main dispatch
      |   queue, unless otherwise indicated. This restriction particularly
      |   applies to classes derived from UIResponder or that involve
      |   manipulating your app’s user interface in any way.
      |       def Maybe a
      |           sub x y = x - y
      |           def Nothing
      |""".stripMargin

  /** Invoking the Enso Documentation Parser */
  println("===== DOCUMENTATION =====")
  val documentation    = new DocParser().runMatched(inp)
  val htmlPath         = "target/"
  val cssFileName      = "style.css"
  val title            = "Foo Bar Baz"
  val fileName         = "output"
  val documentationHtml = DocParserHTMLGenerator.generateHTML(documentation, htmlPath, cssFileName, title, fileName)
  println(pretty(documentation.toString))
  println("------")
  println(documentation.show())
  println("=========================")
}
