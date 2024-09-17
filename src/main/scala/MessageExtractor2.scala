import MessagesExtractor.{args, currentTag, endTag, message, messages, output, outputPrefix, path, rootKey, sequence, tagSequence}

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

  val startTagPattern = "<([a-zA-z0-9]+).*>".r
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

  def extractContent(tag: String, line: String): String = {
    val fullMatch = line
    val startIndex = fullMatch.indexOf(s">") + 1
    val endIndex = fullMatch.indexOf(s"</$tag")

    fullMatch.substring(startIndex, endIndex)
  }

  def extractChildren(line: String, acc: List[String] = Nil): List[String] = {
    if(line.isEmpty) acc
    else {
      val startIndex = line.indexOf("<")
      if (startIndex == -1) {
        acc :+ line
      }else {
        val tag = startTagPattern.findFirstMatchIn(line).get.group(1)
        val endIndex = line.indexOf(s"/$tag>") + tag.length() + 2
        val head = line.substring(startIndex, endIndex)
        val tail = line.replace(head, "")
        extractChildren(tail, acc :+ head)
      }
    }

  }

  def processLine(line: String, pkey: String): (ListMap[String, String], List[String]) = {
    val m1 = contentPattern.findFirstMatchIn(line.trim())
    val m2 = startTagPattern.findFirstMatchIn(line.trim())

    (m1, m2) match {
      case (None, Some(_)) =>
        ListMap.empty[String, String] -> List.empty[String]
      case (Some(m), Some(_)) =>
        val tag = Tag(m.group(1), line)

        if(!tagSequence.contains(tag.value)) {
          tagSequence = tagSequence + (tag.value -> sequence())
        }

        val key = s"$pkey.${tag.value}.${tagSequence(tag.value)()}"
        val content = extractContent(tag.value, line)
        val children: Seq[(ListMap[String, String], List[String])] = extractChildren(content).map{ child =>
          processLine(child, key)
        }

        val substitutionSeq = sequence()

        val statements: Seq[String] = children.map(_._2).foldLeft(List.empty[String])((acc, entry) => acc ++ entry)

        val expressions = statements.filter{ statements => statements.contains("<") || statements.contains("@")}.flatMap{ content =>
          val expressions = for( m <- expressionPattern.findAllMatchIn(content) if !m.group(1).contains("message")) yield m.group(1)
          if(expressions.isEmpty) List(content) else expressions.toSeq
        }

        var output = statements.mkString(" ")
        for(expression <- expressions) {
          output = output.replace(expression, s"{${substitutionSeq()}}")
        }
        val messages = children.map(_._1).foldLeft(ListMap.empty[String, String])((acc, entry) => acc ++ entry) + (key -> output)

        val args = expressions.foldLeft("")((acc, expression) => s"$acc, ${expression.replace("@", "")}")
          messages -> List(tag.render(s"""@messages("$key"$args)"""))
      case _ =>
        (ListMap.empty[String, String], List(line))
    }

  }

  var tagSequence: Map[String, () => Int ] = Map()

  val scanner = new Scanner(path)


  while(scanner.hasNext()) {
    val line = scanner.nextLine()

    val m1 = startTagPattern.findFirstMatchIn(line.trim)

    if(m1.isDefined) {
      processLine(line, rootKey) match {
        case (message, output) if message.nonEmpty && output.nonEmpty =>
          messages = messages ++ message
          outputs = outputs :+ output.mkString("")
        case _ =>
          outputs = outputs :+ line
      }
    }else {
      outputs = outputs :+ line
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
