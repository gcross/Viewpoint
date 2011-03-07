package Viewpoint {
  import scala.annotation.tailrec

  class Parent {
    var children = new scala.collection.mutable.DoubleLinkedList[Node]
    var properties = new scala.collection.mutable.HashMap[String,String]
    def getProperty(name: String) : Option[String] = properties.get(name)
  }

  class Node(val id: String, var heading: String, var body: String) extends Parent {
    var parents = new scala.collection.mutable.HashSet[Node]
    override def getProperty(name: String) : Option[String] =
      properties.get(name).orElse({
        parents.map({
          _.properties.get(name) match {
            case s @ Some(value) => return s
            case _ => ()
          }
        })
        None
      })
  }

  class Tree {
    val root = new Parent
    val nodemap = new scala.collection.mutable.HashMap[String,Node]
  }

  object Parser {
    object NoHeaderFound extends Exception
    case class UnsupportedFileVersion(version: String) extends Exception

    sealed abstract class ParseError extends Exception
    object UnexpectedIndent extends ParseError
    object UnexpectedUnindent extends ParseError
    object BadLevelNumber extends ParseError
    object UnmatchedBeginSection extends ParseError
    object MismatchedEndSection extends ParseError
    object ContentAfterEndOfFileSentinel extends ParseError
    object InvalidSectionName extends ParseError
    object NodeNotFoundImmediatelyAfterBeginSection extends ParseError
    object UnexpectedEndOfFile extends ParseError
    object BadCommentSectionLine extends ParseError

    case class ParseErrorWithContext(line_number: Int, line: String, problem: ParseError) extends Exception

    val header_regex = "(\\s*)(.*?)@\\+leo-ver=(.*)".r

    def countIndentation(text: String) : Int = {
      val first_non_space = text.indexWhere(!_.isSpaceChar)
      if(first_non_space < 0) text.length
      else first_non_space
    }

    def parseLevel(text: String) : Int = {
      text match {
        case "*" => 1
        case "**" => 2
        case _ => text.substring(1,text.length-1).toInt
      }
    }

    sealed abstract class Line
    case class NodeLine(id: String, level: Int, header: String) extends Line
    object BeginCommentLine extends Line
    case class BeginSectionLine(indentation: Int, section_name: Option[String]) extends Line
    case class EndSectionLine(section_nane: String) extends Line
    object VerbatimLine extends Line
    case class PropertyLine(name: String, value: String) extends Line
    case class TextLine(text: String) extends Line

    class LineParser(comment_marker: String) {
      val node_regex = "%s@+node:(.*?):\\s*(\\*?[0-9]*\\*) (.*)".format(comment_marker).r
      val begin_comment_regex = "%s@\\+at\\z".format(comment_marker).r
      val comment_line_regex = "%s (.*)".format(comment_marker).r
      val end_comment_regex = "%s@@c\\z".format(comment_marker).r
      val begin_section_regex = "(\\s)*%s@\\+(.*)".format(comment_marker).r
      val end_section_regex = "(\\s)*%s@-(.*)".format(comment_marker).r
      val property_regex = "%s@([^\\s])* (.*)".format(comment_marker).r
      def apply(line: String) : Line = {
        begin_comment_regex.findPrefixMatchOf(line).map({m =>
          return BeginCommentLine
        })
        node_regex.findPrefixMatchOf(line).map({m =>
          return NodeLine(m.group(1),parseLevel(m.group(2)),m.group(3))
        })
        begin_section_regex.findPrefixMatchOf(line).map({m =>
          val section_name = m.group(2)
          BeginSectionLine(m.group(1).toInt,
            if(section_name == "others")
              None
            else if(section_name.substring(0,2) == "<<" && section_name.substring(section_name.length-2) == ">>")
              Some(section_name)
            else
              throw InvalidSectionName
          )
        })
        end_section_regex.findPrefixMatchOf(line).map({m =>
          return EndSectionLine(m.group(1))
        })
        if(line == "@verbatim") return VerbatimLine
        property_regex.findPrefixMatchOf(line).map({m =>
          return PropertyLine(m.group(1),m.group(2))
        })
        TextLine(line)
      }

      def extractCommentFrom(line: String) : Option[String] = {
        if(end_comment_regex.findPrefixMatchOf(line).isEmpty) return None
        comment_line_regex.findPrefixMatchOf(line) match {
          case Some(m) => {
            if(m.group(1).length > 0) throw UnexpectedIndent
            return Some(m.group(2))
          }
          case None => throw BadCommentSectionLine
        }
      }
    }

    def parse(lines: Iterator[String]) : Node = {
      var line_number = 0
      def nextLine : String = {
        line_number = line_number + 1
        if(!lines.hasNext) throw UnexpectedEndOfFile
        lines.next
      }
      var body : StringBuilder = new StringBuilder

      def parseHeader : (String,String) = {
        while(lines.hasNext) {
          val line = nextLine
          header_regex.findPrefixMatchOf(line) match {
            case None => {
              body.append("@first ")
              body.append(line)
              body.append('\n')
            }
            case Some(m) => {
              if(m.group(1).length > 0) throw UnexpectedIndent
              return (m.group(2),m.group(3))
            }
          }
        }
        throw NoHeaderFound
      }
      val (comment_marker,version) = parseHeader
      if(version != "thin-5") throw UnsupportedFileVersion(version)

      val parseLine = new LineParser(comment_marker)

      import scala.collection.mutable.Stack

      var current_body = new StringBuilder
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

      def nextSectionLine : String = {
        val line = nextLine
        if(line.length >= 0) {
          val indentation = countIndentation(line)
          if(indentation < current_section_indentation) throw UnexpectedUnindent
          line.substring(indentation)
        } else line
      }

      var current_node = parseLine(nextSectionLine) match {
        case NodeLine(id,level,header) => {
          if(level != 1) throw BadLevelNumber
          new Node(id,header,"")
        }
        case _ => throw NodeNotFoundImmediatelyAfterBeginSection
      }
      
      while(lines.hasNext) {
        if(current_section_level == 0) throw ContentAfterEndOfFileSentinel
        val line = nextSectionLine
        parseLine(line) match {
          case NodeLine(id,level,header) => {
            current_node.body = current_body.toString
            if(level < current_section_level) throw BadLevelNumber
            while(level < current_level) {
              current_node = current_parent_node
              current_parent_node = node_stack.pop
              current_level -= 1
            }
            if(level > current_level) {
              if(level > current_level+1) throw BadLevelNumber
              node_stack.push(current_parent_node)
              current_parent_node = current_node
              current_level += 1
            }
            current_body = new StringBuilder
            current_node = new Node(id,header,"")
            current_node.parents += current_parent_node
            current_parent_node.children :+ current_node
          }
          case BeginSectionLine(indentation,maybe_section_name) => {
            for(_ <- 1 to indentation) current_body.append(' ')
            val section_name = maybe_section_name match {
              case Some(section_name) => { current_body.append(section_name); section_name }
              case None => { current_body.append("@others"); "others" }
            }
            current_body.append('\n')
            if(!lines.hasNext) throw UnmatchedBeginSection
            val NodeLine(id,level,header) = parseLine(lines.next.substring(indentation)) match {
              case (n : NodeLine) => n
              case _ => throw NodeNotFoundImmediatelyAfterBeginSection
            }
            if(level != current_level+1) throw BadLevelNumber
            body_stack.push(current_body)
            node_stack.push(current_parent_node)
            current_parent_node = current_node

            current_body = new StringBuilder
            current_level = current_level+1
            current_node = new Node(id,header,"")
            current_node.parents += current_parent_node
            current_parent_node.children :+ current_node

            section_indentation_stack.push(current_section_indentation)
            section_level_stack.push(current_section_level)
            section_name_stack.push(current_section_name)

            current_section_indentation += indentation
            current_section_level = current_level+2
            current_section_name = section_name
          }
          case EndSectionLine(section_name) => {
            if(section_name != current_section_name) throw MismatchedEndSection
            current_node.body = current_body.toString
            while(current_level >= current_section_level) {
              current_node = current_parent_node
              current_parent_node = node_stack.pop
              current_level -= 1
            }
            if(current_level > 1) {
              current_node = node_stack.pop
              current_body = body_stack.pop
              current_section_indentation = section_indentation_stack.pop
              current_section_level = section_level_stack.pop
              current_section_name = section_name_stack.pop
            }
            current_level -= 1
          }
          case BeginCommentLine => {
            current_body.append("@\n")
            @tailrec def parseCommentBody : Unit = {
              val line = nextSectionLine
              parseLine.extractCommentFrom(line) match {
                case Some(comment_line) => {
                  current_body.append(comment_line)
                  current_body.append('\n')
                  parseCommentBody
                }
                case None => {
                  current_body.append("@c\n")
                  return
                }
              }
            }
            parseCommentBody
          }
          case VerbatimLine => {
            current_body.append("@verbatim\n")
            current_body.append(nextSectionLine)
          }
          case PropertyLine(name,value) => {
            current_node.properties(name) = value
            current_body.append('@')
            current_body.append(name)
            current_body.append(' ')
            current_body.append(value)
            current_body.append('\n')
          }
          case TextLine(text) => {
            current_body.append(text)
            current_body.append('\n')
          }
        }
      }
      if(current_level > 0) throw UnexpectedEndOfFile
      current_node
    }
  }
}
