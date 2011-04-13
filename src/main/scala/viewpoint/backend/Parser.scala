//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1356: * @file Parser.scala
//@@language Scala
package viewpoint.backend.crosswhite.parser

//@+<< Imports >>
//@+node:gcross.20110412144451.1398: ** << Imports >>
import java.io.Writer
import scala.collection.mutable.ListBuffer

import viewpoint.backend.crosswhite.model.Node
//@-<< Imports >>

//@+others
//@+node:gcross.20110412144451.1358: ** object Parser
object Parser {
  //@+<< Errors >>
  //@+node:gcross.20110412144451.1402: *3* << Errors >>
  object NoHeaderFound extends Exception
  case class UnsupportedFileVersion(version: String) extends Exception
  case class ParseErrorWithContext(line_number: Int, line: String, problem: ParseError) extends Exception {
    override def toString = "Error encountered while parsing line %s:\n\"%s\"\n%s\n".format(line_number,line,problem)
  }

  // Parse errors
  sealed abstract class ParseError extends Exception

  case class BadLevelNumber(level: Int) extends ParseError

  object ContentAfterEndOfFileSentinel extends ParseError

  case class InvalidSectionName(section_name: String) extends ParseError {
    override def toString = "Invalid section name: " + section_name
  }

  object MismatchedEndSection extends ParseError

  object MismatchedEndComment extends ParseError

  object NodeNotFoundImmediatelyAfterBeginSection extends ParseError

  object UnexpectedIndent extends ParseError

  object UnexpectedEndOfFile extends ParseError

  object UnexpectedUnindent extends ParseError

  object UnmatchedBeginComment extends ParseError

  object UnmatchedBeginSection extends ParseError

  object UnrecognizedSentinel extends ParseError
  //@-<< Errors >>
  //@+<< Line case classes >>
  //@+node:gcross.20110412144451.1399: *3* << Line case classes >>
  sealed abstract class Line

  object BeginCommentLine extends Line

  case class BeginSectionLine(indentation: Int, section_name: String) extends Line

  object EndCommentLine extends Line

  case class EndSectionLine(section_name: String) extends Line

  case class NodeLine(id: String, level: Int, heading: String) extends Line

  case class PropertyLine(name: String, value: String) extends Line

  case class TextLine(text: String) extends Line

  object VerbatimLine extends Line
  //@-<< Line case classes >>
  //@+<< Line parser >>
  //@+node:gcross.20110412144451.1406: *3* << Line parser >>
  class LineParser(val comment_marker: String) {
    val comment_line_starter = comment_marker + " "
    val quoted_comment_marker = "\\Q%s\\E".format(comment_marker)
    val sentinel = comment_marker + "@"

    val BeginCommentRegex = "%s@\\+at\\z".format(quoted_comment_marker).r
    val BeginSectionRegex = "(\\s*)%s@\\+(.*)".format(quoted_comment_marker).r
    val EndCommentRegex = "%s@@c\\z".format(quoted_comment_marker).r
    val EndSectionRegex = "%s@-(.*)".format(quoted_comment_marker).r
    val NodeRegex = "%s@\\+node:(.*?):\\s*(\\*?[0-9]*\\*) (.*)".format(quoted_comment_marker).r
    val PropertyRegex = "%s@@([^\\s]*) (.*)\\z".format(quoted_comment_marker).r
    val VerbatimText = "%s@verbatim".format(comment_marker)

    def apply(line: String): Line = line match {
      case BeginCommentRegex() => BeginCommentLine
      case EndCommentRegex() => EndCommentLine
      case NodeRegex(id,level,heading) => NodeLine(id,parseLevel(level),heading)
      case BeginSectionRegex(whitespace,section_name) => BeginSectionLine(whitespace.length,section_name)
      case EndSectionRegex(section_name) => EndSectionLine(section_name)
      case VerbatimText => VerbatimLine
      case PropertyRegex(key,value) => PropertyLine(key,value)
      case line =>
        if(line.startsWith(sentinel))
          throw UnrecognizedSentinel
        else
          TextLine(line)
    }

    def extractCommentLine(line: String): String =
      if(line.startsWith(comment_line_starter))
        line.substring(comment_line_starter.length)
      else
        throw UnmatchedBeginComment
  }
  //@-<< Line parser >>
  //@+<< Regular expressions >>
  //@+node:gcross.20110412144451.1410: *3* << Regular expressions >>
  val Header = "(\\s*)(.*?)@\\+leo-ver=(.*)".r
  //@-<< Regular expressions >>

  //@+others
  //@+node:gcross.20110412144451.1400: *3* countIndentation
  def countIndentation(text: String) : Int = {
    val first_non_space = text.indexWhere(!_.isSpaceChar)
    if(first_non_space < 0) text.length
    else first_non_space
  }
  //@+node:gcross.20110412144451.1407: *3* parse
  type ParseResult = Either[Exception,Node]
  def parse(lines: Iterator[String]) : ParseResult =
    try{
      Right(parseOrThrow(lines))
    } catch {
      case e: Exception => Left(e)
    }
  //@+node:gcross.20110412144451.1408: *3* parseAllAsynchronously
  object TerminateParseAll {}
  sealed abstract class ParseCompletionMessage[T]
  case class CompletedParse[T](id: T) extends ParseCompletionMessage[T]
  case class CompletedParseAll[T](results: Iterable[(T,ParseResult)]) extends ParseCompletionMessage[T]
  case class TerminatedParseAll[T](results: Iterable[(T,ParseResult)]) extends ParseCompletionMessage[T]
  def parseAllAsynchronously[T](node_sources: Iterator[(T,() => Iterator[String])], receiver: scala.actors.OutputChannel[ParseCompletionMessage[T]]): scala.actors.Actor = {
    import scala.actors.Actor.{actor,exit,loop,react,self}
    actor {
      var number_remaining = 0
      val master = self
      case class Done(id: T, result: ParseResult)
      for((id,getLines) <- node_sources) {
        val slave = actor {
          self.link(master)
          master ! Done(id,parse(getLines()))
        }
        number_remaining += 1
      }
      val results = new ListBuffer[(T,ParseResult)]
      loop {
        if(number_remaining == 0) {
          receiver ! CompletedParseAll(results)
          exit
        } else {
          react {
            case TerminateParseAll => {
              receiver ! TerminatedParseAll(results)
              exit
            }
            case Done(id,result) => {
              results += ((id,result))
              receiver ! CompletedParse(id)
              number_remaining -= 1
            }
          }
        }
      }
    }
  }
  //@+node:gcross.20110412144451.1401: *3* parseLevel
  def parseLevel(text: String) : Int = {
    text match {
      case "*" => 1
      case "**" => 2
      case _ => text.substring(1,text.length-1).toInt
    }
  }
  //@+node:gcross.20110412144451.1405: *3* parseOrThrow
  def parseOrThrow(lines: Iterator[String]) : Node = {
    var line_number = 0
    def nextLine : String = {
      line_number = line_number + 1
      if(!lines.hasNext) throw UnexpectedEndOfFile
      lines.next
    }
    var current_body : StringBuilder = new StringBuilder

    def parseHeader : (String,String) = {
      while(lines.hasNext) {
        lines.next match {
          case Header("",comment_marker,version) => return (comment_marker,version)
          case Header(_,_,_) => throw UnexpectedIndent
          case line => {
            current_body.append("@first ")
            current_body.append(line)
            current_body.append('\n')
          }
        }
      }
      throw NoHeaderFound
    }
    val (comment_marker,version) = parseHeader
    if(version != "5-thin") throw UnsupportedFileVersion(version)

    val parseLine = new LineParser(comment_marker)

    import scala.collection.mutable.Stack

    var current_level = 1
    var current_parent_node : Node = null
    var current_section_indentation = 0
    var current_section_level = 2
    var current_section_name = "leo"
    var currently_extracting_comment = false

    val body_stack = new Stack[StringBuilder]
    val node_stack = new Stack[Node]
    val section_indentation_stack = new Stack[Int]
    val section_level_stack = new Stack[Int]
    val section_name_stack = new Stack[String]

    def nextSectionLine : String = {
      val line = nextLine
      if(line.length >= 0) {
        val indentation = countIndentation(line)
        if(indentation < current_section_indentation) throw UnexpectedUnindent
        line.substring(indentation)
      } else line
    }

    var current_node = parseLine(nextSectionLine) match {
      case NodeLine(id,level,heading) => {
        if(level != 1) throw BadLevelNumber(level)
        new Node(id,heading,"")
      }
      case _ => throw NodeNotFoundImmediatelyAfterBeginSection
    }

    while(lines.hasNext) {
      if(current_section_level == 0) throw ContentAfterEndOfFileSentinel
      val line = nextSectionLine
      try { parseLine(line) match {
        case NodeLine(id,level,heading) => {
          currently_extracting_comment = false
          current_node.body = current_body.toString
          if(level < current_section_level) throw BadLevelNumber(level)
          while(level < current_level) {
            current_node = current_parent_node
            current_parent_node = node_stack.pop
            current_level -= 1
          }
          if(level > current_level) {
            if(level > current_level+1) throw BadLevelNumber(level)
            node_stack.push(current_parent_node)
            current_parent_node = current_node
            current_level += 1
          }
          current_body = new StringBuilder
          current_node = new Node(id,heading,"")
          current_parent_node.appendChild(current_node)
        }
        case BeginSectionLine(indentation,section_name) => {
          if(currently_extracting_comment) throw UnmatchedBeginComment
          for(_ <- 1 to indentation) current_body.append(' ')
          if(section_name.charAt(0) != '<')
            current_body.append('@')
          current_body.append(section_name)
          current_body.append('\n')
          if(!lines.hasNext) throw UnmatchedBeginSection
          val NodeLine(id,level,heading) = parseLine(lines.next.substring(indentation)) match {
            case (n : NodeLine) => n
            case _ => throw NodeNotFoundImmediatelyAfterBeginSection
          }
          if(level != current_level+1) throw BadLevelNumber(level)
          body_stack.push(current_body)
          node_stack.push(current_parent_node)
          current_parent_node = current_node

          current_body = new StringBuilder
          current_level = level
          current_node = new Node(id,heading,"")
          current_parent_node.appendChild(current_node)

          section_indentation_stack.push(current_section_indentation)
          section_level_stack.push(current_section_level)
          section_name_stack.push(current_section_name)

          current_section_indentation += indentation
          current_section_level = current_level
          current_section_name = section_name
        }
        case EndSectionLine(section_name) => {
          if(section_name != current_section_name) throw MismatchedEndSection
          currently_extracting_comment = false
          current_node.body = current_body.toString
          while(current_level > current_section_level) {
            current_node = current_parent_node
            current_parent_node = node_stack.pop
            current_level -= 1
          }
          if(current_level > 1) {
            current_node = current_parent_node
            current_parent_node = node_stack.pop
            current_body = body_stack.pop
            current_section_indentation = section_indentation_stack.pop
            current_section_level = section_level_stack.pop
            current_section_name = section_name_stack.pop
          }
          current_level -= 1
        }
        case BeginCommentLine => {
          if(currently_extracting_comment) throw UnmatchedBeginComment
          currently_extracting_comment = true
          current_body.append("@\n")
        }
        case EndCommentLine => {
          if(!currently_extracting_comment) throw MismatchedEndComment
          currently_extracting_comment = false
          current_body.append("@c\n")
        }
        case VerbatimLine => {
          if(currently_extracting_comment) throw UnmatchedBeginComment
          current_body.append("@verbatim\n")
          current_body.append(nextSectionLine)
        }
        case PropertyLine(name,value) => {
          if(currently_extracting_comment) throw UnmatchedBeginComment
          current_node.setProperty(name,value)
          current_body.append('@')
          current_body.append(name)
          current_body.append(' ')
          current_body.append(value)
          current_body.append('\n')
        }
        case TextLine(text) => {
          if(currently_extracting_comment)
            current_body.append(parseLine.extractCommentLine(text))
          else
            current_body.append(text)
          current_body.append('\n')
        }
      } } catch {
        case (e : ParseError) => throw ParseErrorWithContext(line_number,line,e)
      }
    }
    if(current_level > 0) throw UnexpectedEndOfFile
    current_node
  }
  //@+node:gcross.20110412144451.1404: *3* validSectionName
  //@@raw
def validSectionName(text: String) : Boolean =
  text == "others" || text.substring(0,2) == "<<" && text.substring(text.length-2) == ">>"
  //@@end_raw
  //@+node:gcross.20110412144451.1409: *3* writeAllAsynchronously
  object TerminateWriteAll {}
  sealed abstract class WriteCompletionMessage
  case class CompletedWrite(node: Node, writer: Writer) extends WriteCompletionMessage
  case class FailedWrite(node: Node, writer: Writer, problem: Exception) extends WriteCompletionMessage
  case class CompletedWriteAll(results: Iterable[(Node,Writer,Option[Exception])]) extends WriteCompletionMessage
  case class TerminatedWriteAll(results: Iterable[(Node,Writer,Option[Exception])]) extends WriteCompletionMessage
  def writeAllAsynchronously(nodes: Iterator[(Node,() => Writer)],receiver: scala.actors.OutputChannel[WriteCompletionMessage]): scala.actors.Actor = {
    import scala.actors.Actor.{actor,exit,loop,react,self}
    import scala.actors.Exit
    actor {
      var number_remaining = 0
      val master = self
      case class Done(node: Node, writer: Writer, problem: Option[Exception])
      master.trapExit = true
      for((node,getWriter) <- nodes) {
        val slave = actor {
          self.link(master)
          val writer = getWriter()
          val maybe_problem =
            try {
              node.writeTo(writer)
              None
            } catch {
              case problem: Exception => Some(problem)
            }
          master ! Done(node,writer,maybe_problem)
        }
        master.link(slave)
        number_remaining += 1
      }
      val results = new ListBuffer[(Node,Writer,Option[Exception])]
      loop {
        if(number_remaining == 0) {
          receiver ! CompletedWriteAll(results)
          exit
        } else {
          react {
            case Done(node,writer,maybe_problem) => {
              results += ((node,writer,maybe_problem))
              receiver !
                (maybe_problem match {
                  case None => CompletedWrite(node,writer)
                  case Some(problem) => FailedWrite(node,writer,problem)
                })
              number_remaining -= 1
            }
            case TerminateWriteAll => {
              receiver ! TerminatedWriteAll(results)
              exit
            }
          }
        }
      }
    }
  }
  //@-others
}
//@-others
//@-leo
