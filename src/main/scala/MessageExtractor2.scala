import MessagesExtractor.{args, currentTag, output, path, rootKey, sequence, tagSequence}

import java.util.Scanner
import java.nio.file.Path
import java.nio.file.Paths
import java.io.PrintWriter
import java.io.File
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

  val startTagPattern = "^<([a-zA-z0-9]+).*>".r
  val contentPattern = "<([a-zA-z0-9]+).*>(.+)</([a-zA-z0-9]+)>".r
  val expressionPattern = "(@[a-zA-Z0-9.]+(\\(.*\\))*)".r

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

  val processMessage: (String, () => Int, String, List[String]) => List[String] = (parentKey: String, seq: () => Int, message: String, content: List[String]) => {
    val substitutionSeq = sequence()
    var newMessage = message
    val expressions =  for( m <- expressionPattern.findAllMatchIn(message)) yield {
      val expression = m.group(1)
      newMessage = newMessage.replace(expression, s"{$substitutionSeq()}")
      expression
    }
    val key = s"$parentKey.${seq()}"
    messages = messages + (key -> newMessage)
    var newContent = content
    if(expressions.nonEmpty) {
      newContent = content :+ s"""messages($key, ${ expressions.mkString(", ") } )"""
    }else {
      newContent= content :+ s"""messages($key)"""
    }
    newContent
  }

  def processContent(key: String, seq: () => Int, tokenizer: Scanner, tokens: String, content: List[String]): List[String] = {
    if(tokenizer.hasNext()) {
      tokenizer.next() match {
        case "<" =>
          processContent(key, seq, tokenizer, "<", processMessage(key, seq, tokens, content))
        case token =>
          processContent(key, seq, tokenizer, tokens + token, content)
      }
    } else {
      processMessage(key, seq, tokens, content)
    }
  }

  def processLine(line: String, pkey: String): Option[((String, String), String)] = {
    val m1 = contentPattern.findFirstMatchIn(line.trim())

    m1 match {
      case Some(m) =>
        val tag = Tag(m.group(1), line)
        if(!tagSequence.contains(tag.value)) {
          tagSequence = tagSequence + (tag.value -> sequence())
        }
        val content = m.group(2)
        val key = s"$rootKey.${tag.value}.${tagSequence(tag.value)()}"
        Some(key -> content, tag.render(s"""@messages("$key")"""))
      case _ => None
    }

  }

  var tagSequence: Map[String, () => Int ] = Map()

  val scanner = new Scanner(path)


  while(scanner.hasNext()) {
    val line = scanner.nextLine()

    val m1 = startTagPattern.findFirstMatchIn(line.trim)

    if(m1.isDefined) {
      processLine(line, "").foreach {
        case (message, output) =>
          messages = messages + message
          outputs = outputs :+ output
      }
    }else {
      outputs = outputs :+ line
    }
  }

  messages.foreach(println)
  println("\n\n")
  outputs.foreach(println)
}
