import java.util.Scanner
import java.nio.file.Path
import java.nio.file.Paths
import java.io.PrintWriter
import java.io.File
import scala.collection.immutable.ListMap
import scala.util.matching.Regex.Match

object MessagesExtractor extends App {

  case class Tag(value: String = "", line: String = "") {
    def render(body: String): String = {
      val start = line.substring(0, line.indexOf(">")+1)
      val end = s"</$value>"
      s"$start$body$end"
    }
    override def toString: String = value
  }

  val blockTags = List(
    "address", "article", "aside", "blockquote", "canvas", "dd", "div", "dl", "dt", "fieldset", "figcaption", "figure",
    "footer", "form", "header", "hr", "li", "main", "nav", "noscript", "ol", "p", "pre", "section", "table", "h1", "h2",
    "h3", "h4", "h5", "h6", "h7", "h8", "tr", "tfoot", "ul", "video")

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

  val outputPrefix = filenameMatch.get.group(1)

  def sequence = () => {
    var next = -1
    () => {
      next = next + 1
      next
    }
  }

  def extractMessage(m: Match): String = {
    val tag = m.group(1)
    val fullMatch = m.group(0)
    val startIndex = fullMatch.indexOf(s">") + 1;
    val endIndex = fullMatch.indexOf(s"</$tag")

    fullMatch.substring(startIndex, endIndex)
  }

  var tagSequence: Map[String, () => Int ] = Map()

  val scanner = new Scanner(path)
  var endTag = false

  val messagePattern = "<([a-zA-z0-9]+).*>(.+)</([a-zA-z0-9]+)>".r
  val emptyTagPattern = "<([a-zA-z0-9]+).*></([a-zA-z0-9]+)>".r
  val inlineTag = "<([a-zA-z0-9]+).* />".r
  val startTagPattern = "<([a-zA-z0-9]+).*>".r
  val expressionPattern = "(@[a-zA-Z0-9.]+(\\(.*\\))*)".r
  val expressionLinePattern = "^@.*".r
  def endTagPattern(tag: String) = s"</$tag>".r
  var currentTag = Tag()
  var messages = ListMap[String, String]()
  var output = List[String]()

  val processInlineMessages = (m1: Match, line: String) => {
    val tag = Tag(m1.group(1), line)

    if(!tagSequence.contains(tag.value)) {
      tagSequence = tagSequence + (tag.value -> sequence())
    }

    val message = extractMessage(m1)

    expressionPattern.findFirstIn(message) match {
      case Some(expression) if expression.length == message.length =>
        output = output :+ line
      case Some(_) =>
        val substitutionSeq = sequence()
        var newMessage = message
        var expressions = List[String]()
        for(m <- expressionPattern.findAllMatchIn(message)) {
          val expression = m.group(1).replace("@", "")
          expressions = expressions :+ expression
          newMessage = newMessage.replace(expression, s"{${substitutionSeq()}}")
        }
        val key = s"$rootKey.${tag}.${tagSequence(tag.value)()}"
        messages = messages + ( key -> newMessage.trim() )

        if(startTagPattern.findFirstIn(message).isDefined){
          output = output :+ tag.render(s"""@Html(messages("$key", ${expressions.mkString(", ")}))""")
        }else {
          output = output :+ tag.render(s"""@messages("$key", ${expressions.mkString(", ")})""")
        }
      case _ =>
        val key = s"$rootKey.${tag}.${tagSequence(tag.value)()}"
        messages = messages + ( key -> message.trim() )

        if(startTagPattern.findFirstIn(message).isDefined){
          output = output :+ tag.render(s"""@Html(messages("$key"))""")
        }else {
          output = output :+ tag.render(s"""@messages("$key")""")
        }

    }
  }

  var message = ""
  while(scanner.hasNext()) {
    val line = scanner.nextLine()

    if(!endTag) {
      val m1 = inlineTag.findFirstMatchIn(line)
      val m2 = messagePattern.findFirstMatchIn(line)
      val m3 = startTagPattern.findFirstMatchIn(line)
      val m4 = emptyTagPattern.findFirstMatchIn(line)
      if(m2.isDefined && m2.get.group(1) == m2.get.group(3)) {
        processInlineMessages(m2.get, line)
      }
      else if(m1.isEmpty && m4.isEmpty && m3.isDefined) {
        message = line.substring(line.indexOf(">") + 1).trim()
        currentTag = Tag(m3.get.group(1), line)
        endTag = true
      }
      else {
        output = output :+ line
      }
    }
    else {
      val m1 = expressionLinePattern.findFirstMatchIn(line.trim())
      val m2 = messagePattern.findFirstMatchIn(line)
      val m3 = startTagPattern.findFirstMatchIn(line)
      if(m1.isDefined){
        output = output :+ line
        currentTag = Tag()
        message = ""
        endTag = false
      }
      else if(m2.isDefined && m2.get.group(1) == m2.get.group(3)) {
        output = output :+ currentTag.line
        processInlineMessages(m2.get, line)
        currentTag = Tag()
        message = ""
        endTag = false
      } else if(m3.isDefined && blockTags.contains(m3.get.group(1)) && message.isEmpty) {
        output = output :+ currentTag.line
        output = output :+ line
        currentTag = Tag()
        message = ""
        endTag = false
      }else {
        val m4 = endTagPattern(currentTag.value).findFirstMatchIn(line)
        if(m4.isDefined) {

          if(!tagSequence.contains(currentTag.value)) {
            tagSequence = tagSequence + (currentTag.value -> sequence())
          }

          val key = s"$rootKey.${currentTag.value}.${tagSequence(currentTag.value)()}"
          message = message + " " + line.replace(s"</${currentTag}>", "")
          messages = messages + (key -> message.trim() )
          output = output :+ currentTag.render(s"""@messages("$key")""")
          currentTag = Tag()
          message = ""
          endTag = false
        }else {
          if(line.nonEmpty) {
            message = message + " " + line
          }
        }
      }

    }

  }

  val messagesFile = s"$outputPrefix.messages"
  val messageOutput = new PrintWriter(new File(messagesFile))
  messages.foreach{ case (key, value) => messageOutput.println(s"$key = ${value.trim()}")}
  messageOutput.close()
  println(s"messages file written to $messagesFile")

  val twirlTemplateFile = s"$outputPrefix.messages.scala.html"
  val twirlTemplateOutput = new PrintWriter(new File(twirlTemplateFile))
  output.foreach( line => twirlTemplateOutput.println(s"$line"))
  twirlTemplateOutput.close()
  println(s"twirlTemplate file written to $twirlTemplateFile")
}