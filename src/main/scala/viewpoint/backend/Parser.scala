//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1356: * @file Parser.scala
//@@language Scala
package viewpoint.backend.crosswhite.parser

//@+<< Imports >>
//@+node:gcross.20110412144451.1398: ** << Imports >>
import java.io.Writer
import scala.annotation.tailrec
import scala.collection.BufferedIterator
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
  //@+<< Line sentinels >>
  //@+node:gcross.20110412144451.1406: *3* << Line sentinels >>
  class LineSentinels(val comment_marker: String) {
    val quoted_comment_marker = "\\Q%s\\E".format(comment_marker)

    val EndSectionSentinel = "^%s@-(.*)".format(quoted_comment_marker).r
    val PropertySentinel = "^%s@@([^\\s]*) (.*)".format(quoted_comment_marker).r
    val VerbatimSentinel = "^%s@verbatim$".format(quoted_comment_marker).r

    object BeginCommentSentinel {
      val Regex = "^%s@\\+at( .*)?".format(quoted_comment_marker).r
      def unapply(line: String): Option[String] =
        line match {
          case Regex() => Some("@")
          case Regex(text_or_null) => Some("@" + Option(text_or_null).getOrElse(""))
          case _ => None
        }
    }

    object BeginSectionSentinel {
      val Regex = "^(\\s*)%s@\\+(.*)".format(quoted_comment_marker).r
      def unapply(line: String): Option[(Int,String)] =
        line match {
          case Regex(indentation,name) => Some((indentation.length,name))
          case _ => None
        }
    }

    object EndCommentSentinel {
      val Regex = "^%s@@c( .*)?".format(quoted_comment_marker).r
      def unapply(line: String): Option[String] =
        line match {
          case Regex() => Some("@c")
          case Regex(text_or_null) => Some("@c" + Option(text_or_null).getOrElse(""))
          case _ => None
        }
    }

    object NodeSentinel {
      val Regex = "^%s@\\+node:(.*?):\\s*(\\*?[0-9]*\\*) (.*)".format(quoted_comment_marker).r
      def unapply(line: String): Option[(String,Int,String)] =
        line match {
          case Regex(id,level,heading) => Some((id,parseLevel(level),heading))
          case _ => None
        }
    }
  }
  //@-<< Line sentinels >>
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
  def parseOrThrow(unbuffered_lines: Iterator[String]) : Node = {
    val lines: BufferedIterator[String] = unbuffered_lines.buffered
    var line_number = 0
    def nextLine : String = {
      line_number = line_number + 1
      if(!lines.hasNext) throw UnexpectedEndOfFile
      lines.next
    }
    def peekLine : String = {
      if(!lines.hasNext) throw UnexpectedEndOfFile
      lines.head
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

    val sentinels = new LineSentinels(comment_marker)
    import sentinels._

    import scala.collection.mutable.Stack

    var current_level = 1
    var current_parent_node : Node = null
    var current_section_indentation = 0
    var current_section_level = 2
    var current_section_name = "leo"

    val body_stack = new Stack[StringBuilder]
    val node_stack = new Stack[Node]
    val section_indentation_stack = new Stack[Int]
    val section_level_stack = new Stack[Int]
    val section_name_stack = new Stack[String]

    def stripIndentation(line: String): String =
      if(line.length >= 0) {
        val indentation = countIndentation(line)
        if(indentation < current_section_indentation) throw UnexpectedUnindent
        line.substring(indentation)
      } else line

    def nextIndentedLine: String = stripIndentation(nextLine)
    def peekIndentedLine: String = stripIndentation(peekLine)

    var current_node = nextIndentedLine match {
      case NodeSentinel(id,level,heading) => {
        if(level != 1) throw BadLevelNumber(level)
        new Node(id,heading,"")
      }
      case _ => throw NodeNotFoundImmediatelyAfterBeginSection
    }

    def wrappingParseError[V](line_number: Int, line: String)(block: String => V): V =
      try {
        block(line)
      } catch {
        case (e : ParseError) => throw ParseErrorWithContext(line_number,line,e)
      }

    while(lines.hasNext) {
      if(current_section_level == 0) throw ContentAfterEndOfFileSentinel
      wrappingParseError(line_number,nextIndentedLine) {
        case BeginCommentSentinel(text) => {
          current_body.append(text)
          current_body.append('\n')

          @tailrec
          def parseComment {
            val keep_going =
              wrappingParseError(line_number+1,peekIndentedLine) {
                case EndCommentSentinel(text) =>
                  current_body.append(text)
                  current_body.append('\n')
                  nextLine
                  false
                case EndSectionSentinel(_) =>
                  false
                case NodeSentinel(_,_,_) =>
                  false
                case line =>
                  current_body.append(
                    if(line.startsWith(comment_marker))
                      line.substring(comment_marker.length+1)
                    else
                      throw UnmatchedBeginComment
                  )
                  current_body.append('\n')
                  nextLine
                  true
              }
            if(keep_going) parseComment
          }
          parseComment
        }
        case EndCommentSentinel(text) => throw MismatchedEndComment
        case NodeSentinel(id,level,heading) => {
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
        case BeginSectionSentinel(indentation,section_name) => {
          for(_ <- 1 to indentation) current_body.append(' ')
          if(section_name.charAt(0) != '<')
            current_body.append('@')
          current_body.append(section_name)
          current_body.append('\n')
          if(!lines.hasNext) throw UnmatchedBeginSection
          val (id,level,heading) = lines.next.substring(indentation) match {
            case NodeSentinel(id,level,heading) => (id,level,heading)
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
        case EndSectionSentinel(section_name) => {
          if(section_name != current_section_name) throw MismatchedEndSection
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
        case VerbatimSentinel() => {
          current_body.append("@verbatim\n")
          current_body.append(nextIndentedLine)
        }
        case PropertySentinel(name,value) => {
          current_body.append('@')
          current_body.append(name)
          current_body.append(' ')
          current_body.append(value)
          current_body.append('\n')
        }
        case line => {
          if(line(0) == '@') throw UnrecognizedSentinel
          current_body.append(line)
          current_body.append('\n')
        }
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
