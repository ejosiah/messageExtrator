import MessagesExtractor.{args, currentTag, endTag, message, messages, output, outputPrefix, path, rootKey, sequence, tagSequence}

import java.beans.Expression
import java.util.Scanner
import java.nio.file.Path
import java.nio.file.Paths
import java.io.PrintWriter
import java.io.File
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.matching.Regex.Match

object MessageExtractor2 extends App{

  case class Tag(value: String = "", line: String = "") {
    def render(body: String): String = {
      val start = line.substring(0, line.indexOf(">")+1)
      val end = s"</$value>"
      s"$start$body$end"
    }
    override def toString: String = value
  }

  if(args.length < 2) {
    println("Usage: MessagesExtractor rootKey inputFilePath")
    System.exit(1)
  }

  val rootKey = args(0)
  val path = Paths.get(args(1))
  val fileName = path.getFileName.toString

  val twirlTemplateFilePattern = "(.+).scala.html".r

  val filenameMatch = twirlTemplateFilePattern.findFirstMatchIn(fileName)

  if(filenameMatch.isEmpty) {
    println("script only supports twirl template file format")
    System.exit(2)
  }

  val startTagPattern = "<([a-zA-z0-9]+).*>".r
  val inlineStartTagPattern = "^<([a-zA-z0-9]+).*>".r
  val inlineEndTagPattern = "</([a-zA-z0-9]+)>$".r
  val contentPattern = "<([a-zA-z0-9]+).*>(.+)</([a-zA-z0-9]+)>".r
  val inlineContentPattern = "^<([a-zA-z0-9]+).*>(.+)</([a-zA-z0-9]+)>".r
  val expressionPattern = "(@[a-zA-Z0-9.]+(\\(.*\\))*)".r
  val expressionLinePattern = "^@.*".r
  def createEndTagPattern(tag: String) = s"</$tag>".r

  val outputPrefix = filenameMatch.get.group(1)
  var messages = ListMap[String, String]()
  var outputs = List[String]()

  def sequence = () => {
    var next = -1
    () => {
      next = next + 1
      next
    }
  }

  def extractContent(tag: String, line: String): String = {
    val fullMatch = line
    val startIndex = fullMatch.indexOf(s">") + 1
    val endIndex = fullMatch.indexOf(s"</$tag")

    fullMatch.substring(startIndex, endIndex)
  }

  @tailrec
  def extractChildren(line: String, acc: List[String] = Nil, inTag: Boolean = false): List[String] = {
    if(line.isEmpty) acc
    else if(inTag) {
      val tag = startTagPattern.findFirstMatchIn(line).get.group(1)
      val end = createEndTagPattern(tag).findFirstMatchIn(line).get.group(0)
      val buffer = line.substring(0, line.indexOf(end)) + end
      extractChildren(line.substring(buffer.length, line.length), acc :+ buffer)
    }
    else {
      val buffer = line.takeWhile(_ != '<')
      val newLine = line.substring(buffer.length, line.length)
      extractChildren(newLine, if(buffer.nonEmpty) acc :+ buffer else acc, newLine.startsWith("<"))
    }

  }

  def renderContent(content: String, padding: String = "", mTag: Option[Tag] = None): String = {
    mTag.fold(s"$padding$content")(tag => tag.render(content))
  }

  def format(expression: String): String = {
    val m = contentPattern.findFirstMatchIn(expression)
    if(m.isDefined) {
      val content = m.get.group(2)
      if(content.startsWith("@")) {
        s"s\"\"\"${ expression.replace(content, s"$${ ${content.replace("@", "")} }")}\"\"\""
      }else {
        expression
      }
    }else {
      expression
    }
  }

  def format(key: String, expressions: Seq[String]): String = {
    val newExpressions = expressions.map(format)
    val args = newExpressions.foldLeft("")((acc, expression) => s"$acc, $expression")

    val hasInnerHtml = newExpressions.exists(contentPattern.findFirstMatchIn(_).isDefined)
    if(hasInnerHtml){
      s"""@{ Html(messages("$key"$args)) }"""
    }else {
      s"""@messages("$key"$args)"""
    }
  }

  def processContent(tag: Tag, pkey: String, line: String, render: String => String, parent: Option[Tag] = None): (ListMap[String, String], List[String], Option[Tag]) = {
    if(!tagSequence.contains(tag.value)) {
      tagSequence = tagSequence + (tag.value -> sequence())
    }

    val key = s"$pkey.${tag.value}.${tagSequence(tag.value)()}"
    val content = if(parent.nonEmpty) line.trim else extractContent(tag.value, line)
    val children = extractChildren(content).map{ child =>
      processLine(child, key)
    }

    if (expressionLinePattern.findFirstIn(content).isDefined) {
      (ListMap.empty[String, String], List(render(content)), parent)
    }else {

      val substitutionSeq = sequence()

      val statements: Seq[String] = children.map(_._2).foldLeft(List.empty[String])((acc, entry) => acc ++ entry)

      if(statements.forall(_.startsWith("<"))) {
        val messages = children.map(_._1).foldLeft(ListMap.empty[String, String])((acc, entry) => acc ++ entry)
        (messages , List(render(children.flatMap(_._2).mkString)), parent)
      }else {

        val expressions = statements.filter { statements => statements.contains("<") || statements.contains("@") }

        val expressions1 = expressions.flatMap { content =>
          val expressions = (for (m <- expressionPattern.findAllMatchIn(content) if !m.group(1).contains("message") && !content.contains("<")) yield {
            m.group(1)
          }).toSeq
          if (expressions.isEmpty) List(content) else expressions
        }

        var output = statements.mkString(" ")
        for (expression <- expressions1) {
          output = output.replace(expression, s"{${substitutionSeq()}}")
        }
        val messages = children.map(_._1).foldLeft(ListMap.empty[String, String])((acc, entry) => acc ++ entry) + (key -> output)

        (messages, List(render(format(key, expressions1))), parent)
      }
    }
  }

  def processLine(line: String, pkey: String, parent: Option[Tag] = None): (ListMap[String, String], List[String], Option[Tag]) = {
    // TODO reuse exiting messages
    val m1 = inlineContentPattern.findFirstMatchIn(line.trim())
    val m2 = inlineStartTagPattern.findFirstMatchIn(line.trim())
    val m3 = inlineEndTagPattern.findFirstMatchIn(line.trim())

    (m1, m2, m3, parent) match {
      case (None, Some(_), _, _)   =>
        (ListMap.empty[String, String] , List.empty[String], Some(Tag(m2.get.group(1), line)))
      case (Some(m), Some(_), _, _) =>
        val tag = Tag(m.group(1), line)
        processContent(tag, pkey, line, renderContent(_, "", Some(tag)))
      case (_, _, None, Some(tag)) if line.trim.nonEmpty =>
        processContent(tag, pkey, line, renderContent(_, line.replace(line.trim, "")), parent)
      case _ =>
        (ListMap.empty[String, String], List(line), None)
    }

  }
  var tagSequence: Map[String, () => Int ] = Map()

  val scanner = new Scanner(path)

  var tag: Option[Tag] = None

  while(scanner.hasNext()) {
    val line = scanner.nextLine()

    processLine(line, rootKey, tag) match {
      case (message, output, maybeTag) if message.nonEmpty && output.nonEmpty =>
        messages = messages ++ message
        outputs = outputs :+ output.mkString("")
        tag = maybeTag
      case (_, _, mayBeTag) =>
        outputs = outputs :+ line
        tag = mayBeTag
    }

  }

  val messagesFile = s"$outputPrefix.messages"
  val messageOutput = new PrintWriter(new File(messagesFile))
  messages.foreach{ case (key, value) => messageOutput.println(s"$key = ${value.trim()}")}
  messageOutput.close()
  println(s"messages file written to $messagesFile")

  val twirlTemplateFile = s"$outputPrefix.messages.scala.html"
  val twirlTemplateOutput = new PrintWriter(new File(twirlTemplateFile))
  outputs.foreach( line => twirlTemplateOutput.println(s"$line"))
  twirlTemplateOutput.close()
  println(s"twirlTemplate file written to $twirlTemplateFile")
}
