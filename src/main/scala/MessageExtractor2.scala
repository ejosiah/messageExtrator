import MessagesExtractor.{args, currentTag, messages, output, outputPrefix, path, rootKey, sequence, tagSequence}

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

  def extractContent(tag: String, line: String): String = {
    val fullMatch = line
    val startIndex = fullMatch.indexOf(s">") + 1
    val endIndex = fullMatch.indexOf(s"</$tag")

    fullMatch.substring(startIndex, endIndex)
  }

  def processLine(line: String, pkey: String): Option[((String, String), String)] = {
    val m1 = contentPattern.findFirstMatchIn(line.trim())

    m1 match {
      case Some(m) =>
        val tag = Tag(m.group(1), line)
        val substitutionSeq = sequence()

        if(!tagSequence.contains(tag.value)) {
          tagSequence = tagSequence + (tag.value -> sequence())
        }
        var content = extractContent(tag.value, line)
        val expressions =  (for( m <- expressionPattern.findAllMatchIn(content)) yield {
          val expression = m.group(1)
          content = content.replace(expression, s"{${substitutionSeq()}}")
          expression.replace("@", "")
        }).toSeq


        val key = s"$rootKey.${tag.value}.${tagSequence(tag.value)()}"
        Some(key -> content,
          if(expressions.isEmpty) {
            tag.render(s"""@messages("$key")""")
          }else {
            tag.render(s"""@messages("$key", ${ expressions.mkString(", ") })""")
          }
        )
      case _ => None
    }

  }

  var tagSequence: Map[String, () => Int ] = Map()

  val scanner = new Scanner(path)


  while(scanner.hasNext()) {
    val line = scanner.nextLine()

    val m1 = startTagPattern.findFirstMatchIn(line.trim)

    if(m1.isDefined) {
      processLine(line, "") match {
        case Some((message, output)) =>
          messages = messages + message
          outputs = outputs :+ output
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
