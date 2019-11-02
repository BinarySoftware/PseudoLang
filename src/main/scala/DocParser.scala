package org.enso.syntax.text

import org.enso.flexer
import org.enso.flexer.Reader
import org.enso.syntax.text.ast.Doc
import org.enso.syntax.text.spec.DocParserDef
import scalatags.Text.TypedTag
import scalatags.Text.{all => HTML}
import HTML._
import java.io.File
import java.io.PrintWriter
import flexer.Parser.{Result => res}

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser ////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/**
  * This is the class used to invoke Documentation Parser.
  *
  * It is used to create structured documentation from user input, just like
  * markdown, but it is superior to it.
  */
class DocParser {
  import DocParser._
  private val engine = newEngine()
  private val errMsg = "Internal Documentation Parser Error"

  /**
    * Used to match result of [[run]] function to possibly retrieve Doc
    *
    * @param input - input string to Doc Parser
    * @return - If it was able to retrieve Doc, then retrieved data, else
    *           exception with error message [[errMsg]]
    */
  def runMatched(input: String): Doc = run(input) match {
    case res(_, res.Success(v)) => v
    case _                      => throw new Exception(errMsg)
  }

  /**
    * Used to initialize Doc Parser with input string to get parsed Doc
    *
    * @param input - input string to Doc Parser
    * @return - unmatched result possibly containing Doc
    */
  def run(input: String): Result[Doc] = engine.run(new Reader(input))
}

object DocParser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(DocParserDef())

  /**
    * Doc Parser running methods, as described above, in class [[DocParser]]
    */
  def runMatched(input: String): Doc  = new DocParser().runMatched(input)
  def run(input: String): Result[Doc] = new DocParser().run(input)
}

////////////////////////////////////////////////////////////////////////////////
//// Doc Parser HTML Generator /////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/**
  * This is Doc Parser HTML Generator.
  *
  * Essentially it enables Doc Parser to create pretty HTML files from documented
  * code.
  *
  * When Doc Parser finishes its job user can invoke DocParserHTMLGenerator by
  * simply passing the output of Doc Parser onto function called
  * [[DocParserHTMLGenerator.generateHTML]] that will automatically traverse
  * through AST prepared by Doc Parser and generate HTML files in all
  * appropriate places.
  */
object DocParserHTMLGenerator {

  /**
    * This method is used for generation of HTML files from parsed [[Doc]]
    *
    * @param doc - parsed AST.Module and reformatted using Doc Parser
    * @param path - path to save file
    * @param cssFileName - name of file containing stylesheets for the HTML code
    */
  def generateHTML(
    doc: Doc,
    path: String,
    cssFileName: String,
    title: String,
    fileName: String
  ): Unit = {
    val code = renderHTML(doc, cssFileName, title)
    saveHTMLToFile(path, fileName, code)
  }

  /**
    * Saves HTML code to file
    *
    * @param path - path to file
    * @param name - file name
    * @param code - HTML code generated with Doc Parser
    */
  def saveHTMLToFile(
    path: String,
    name: String,
    code: TypedTag[String]
  ): Unit = {
    val writer = new PrintWriter(new File(path + name + ".html"))
    writer.write(code.toString)
    writer.close()
  }

  //////////////////////////////////////////////////////////////////////////////
  //// HTML Rendering of Documentation /////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////


  /**
    * Function invoked by [[onHTMLRendering]] to render HTML File
    *
    * @param doc - Doc from Doc Parser
    * @param cssLink - string containing CSS file name
    * @return - HTML Code from Doc
    */
  def renderHTML(
    doc: Doc,
    cssLink: String = "style.css",
    title: String
  ): TypedTag[String] = {
    val docClass = HTML.`class` := "Documentation"
    val documentation = HTML.div(docClass)(doc.html)
    HTML.html(createHTMLHead(title, cssLink), HTML.body(documentation))
  }

  /**
    * Function invoked by [[DocumentedToHtml]] to create HTML.Head part of file
    *
    * @param title - HTML page title
    * @param cssLink - string containing CSS file name
    * @return - HTML Head Code
    */
  def createHTMLHead(title: String, cssLink: String): TypedTag[String] = {
    val metaEquiv = HTML.httpEquiv := "Content-Type"
    val metaCont  = HTML.content := "text/html"
    val metaChar  = HTML.charset := "UTF-8"
    val meta      = HTML.meta(metaEquiv)(metaCont)(metaChar)
    val cssRel    = HTML.rel := "stylesheet"
    val cssHref   = HTML.href := cssLink
    val css       = HTML.link(cssRel)(cssHref)
    val fileTitle = scalatags.Text.tags2.title(title)
    HTML.head(meta, css)(fileTitle)
  }
}
