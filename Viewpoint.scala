package Viewpoint {
  import java.io.{PrintWriter,Writer}
  import scala.annotation.tailrec
  import scala.collection.mutable.{Buffer,HashMap,HashSet,ListBuffer}

  object Node {
    val section_regex = "(\\s*)(?:<<\\s*(.*?)\\s*>>|@others)\\s*\\z".r
    val named_section_regex = "\\s*<<\\s*(.*?)\\s*>>\\s*\\z".r
    val property_keys = new HashSet[String]
    property_keys.add("c")
    property_keys.add("comment")
    property_keys.add("language")
    property_keys.add("tabwidth")

    class TangleError extends Exception
    case class NoSectionFound(section_name: String) extends TangleError
    object OthersAppearsTwice extends TangleError

    def isPropertyKey(key: String): Boolean =
      return property_keys.contains(key)

    def levelToString(level: Int): String = {
      assert(level > 0)
      level match {
        case 1 => "*"
        case 2 => "**"
        case _ => "*%s*".format(level)
      }
    }
  }

  class Parent {
    var children : Buffer[Node] = new ListBuffer[Node]
    var properties = new HashMap[String,String]
    def appendChild(node: Node) {
      node.parents.add(this)
      children += node
    }
    def getProperty(key: String) : Option[String] = properties.get(key)
    def setProperty(key: String, value: String)  { properties(key) = value }
    def findChildWithSectionName(section_name: String): Node = {
      import scala.collection.JavaConversions._
      for(child <- children) {
        Node
        .named_section_regex
        .findPrefixMatchOf(child.heading)
        .map({m => if(m.group(1) == section_name) return child})
      }
      return null
    }
    def writeUnnamedChildrenTo(level: Int, indentation: String, comment_marker: String, printer: PrintWriter) {
      import scala.collection.JavaConversions._
      for(child <- children
          if Node.named_section_regex.findPrefixMatchOf(child.heading).isEmpty
        ) child.writeTo(level,indentation,comment_marker,printer)
    }
    def toYAML: String = {
      val builder = new StringBuilder
      appendYAML("",builder)
      builder.toString
    }
    def appendYAML(indentation: String, builder: StringBuilder) {
      import scala.collection.JavaConversions._
      builder.append(indentation)
      builder.append("properties:")
      builder.append('\n')
      val keys : Array[String] = properties.keySet.toArray
      scala.util.Sorting.quickSort(keys)
      for(key <- keys) {
        builder.append(indentation)
        builder.append("    ")
        builder.append(key)
        builder.append(": ")
        builder.append(properties(key))
        builder.append('\n')
      }

      builder.append(indentation)
      builder.append("children:")
      builder.append('\n')

      val child_indentation = indentation + " "*4
      for(child <- children) {
        builder.append(indentation)
        builder.append("  - ")
        child.appendYAML(child_indentation,builder)
      }
    }
  }

  class Node(val id: String, var heading: String, var body: String) extends Parent {
    import Node._
    var parents = new HashSet[Parent]
    def isPlaceholder: Boolean = heading eq null
    override def getProperty(key: String) : Option[String] = {
      properties.get(key).orElse({
        for {
          parent <- parents
          maybe_value = parent.getProperty(key)
        } if(maybe_value.isDefined) return maybe_value
        None
      })
    }
    override def appendYAML(indentation: String, builder: StringBuilder) {
      import org.apache.commons.lang.StringEscapeUtils

      builder.append("id: ")
      builder.append(id)
      builder.append('\n')

      builder.append(indentation)
      builder.append("heading: ")
      builder.append(heading)
      builder.append('\n')

      builder.append(indentation)
      builder.append("body: ")
      builder.append('"')
      builder.append(StringEscapeUtils.escapeJavaScript(body))
      builder.append('"')
      builder.append('\n')

      super.appendYAML(indentation,builder)
    }

    def writeTo(writer: Writer) {
      import scala.util.control.Breaks
      val breaks = new Breaks
      import breaks.{break,breakable}

      val printer = new PrintWriter(writer)
      val comment_marker = getProperty("comment").getOrElse("#")
      val lines = body.lines
      var number_of_first_lines = 0
      breakable {
        while(lines.hasNext) {
          val line = lines.next
          if(!line.startsWith("@first")) break;
          number_of_first_lines += 1
          printer.println(line.substring(7))
        }
      }

      printer.print(comment_marker)
      printer.println("@+leo-ver=5-thin")
      writeTo(1,"",comment_marker,printer,Some(body.lines.drop(number_of_first_lines)))
      printer.print(comment_marker)
      printer.println("@-leo")
    }

    def writeTo(level: Int, indentation: String, comment_marker: String, printer: PrintWriter, maybe_lines: Option[Iterator[String]]=None) {
      import scala.util.control.Breaks

      printer.print(indentation)
      printer.print(comment_marker)
      printer.print("@+node:")
      printer.print(id)
      printer.print(": ")
      printer.print(Node.levelToString(level))
      printer.print(' ')
      printer.println(heading)

      var seen_others = false
      val lines = maybe_lines.getOrElse({body.lines})
      while(lines.hasNext) {
        val line = lines.next
        section_regex.findPrefixMatchOf(line) match {
            case Some(m) => {
              val additional_indentation = m.group(1)
              val section_indentation = indentation + additional_indentation
              val given_section_name =
                Option(m.group(2)) match {
                  case Some(_) => line.substring(additional_indentation.length)
                  case None => "others"
                }
              printer.print(section_indentation)
              printer.print(comment_marker)
              printer.print("@+")
              printer.println(given_section_name)
              Option(m.group(2)) match {
                case Some(section_name) =>
                  Option(findChildWithSectionName(section_name)).getOrElse({
                    throw NoSectionFound(section_name)
                  }).writeTo(level+1,section_indentation,comment_marker,printer)
                case None => {
                  if(seen_others) throw OthersAppearsTwice
                  seen_others = true
                  writeUnnamedChildrenTo(level+1,section_indentation,comment_marker,printer)
                }
              }
              printer.print(section_indentation)
              printer.print(comment_marker)
              printer.print("@-")
              printer.println(given_section_name)
            }
            case None => {
              if(line.charAt(0) == '@') {
                printer.print(indentation)
                line.substring(1) match {
                  case "" => {
                    printer.print(comment_marker)
                    printer.println("@+at")
                    val breaks = new Breaks
                    import breaks.{break,breakable}
                    breakable {
                      while(lines.hasNext) {
                        printer.print(indentation)
                        printer.print(comment_marker)
                        val line = lines.next
                        if(line == "@c") {
                          printer.println("@@c")
                          break
                        } else {
                          printer.print(' ')
                          printer.println(line)
                        }
                      }
                    }
                  }
                  case "verbatim" => {
                    printer.print(comment_marker)
                    printer.println("@verbatim")
                    printer.print(indentation)
                    printer.println(lines.next)
                  }
                  case other => {
                    if(isPropertyKey(other.takeWhile(!_.isSpaceChar))) {
                      printer.print(comment_marker)
                      printer.print('@')
                      printer.println(line)
                    } else {
                      printer.println(line)
                    }
                  }
                }
              } else printer.println(line)
            }
        }
      }
      if(!seen_others)
        writeUnnamedChildrenTo(level+1,indentation,comment_marker,printer)
    }
    def writeToString: String = {
      val writer = new java.io.StringWriter
      writeTo(writer)
      writer.toString
    }
  }

  class Tree {
    val root = new Parent
    val nodemap = new HashMap[String,Node]
  }

  object Parser {
    object NoHeaderFound extends Exception
    case class UnsupportedFileVersion(version: String) extends Exception

    sealed abstract class ParseError extends Exception
    object UnexpectedIndent extends ParseError
    object UnexpectedUnindent extends ParseError
    case class BadLevelNumber(level: Int) extends ParseError
    object UnmatchedBeginSection extends ParseError
    object MismatchedEndSection extends ParseError
    object ContentAfterEndOfFileSentinel extends ParseError
    case class InvalidSectionName(section_name: String) extends ParseError {
      override def toString = "Invalid section name: " + section_name
    }
    object NodeNotFoundImmediatelyAfterBeginSection extends ParseError
    object UnexpectedEndOfFile extends ParseError
    object UnmatchedBeginComment extends ParseError
    object MismatchedEndComment extends ParseError
    object UnrecognizedSentinel extends ParseError

    case class ParseErrorWithContext(line_number: Int, line: String, problem: ParseError) extends Exception {
      override def toString = "Error encountered while parsing line %s:\n\"%s\"\n%s\n".format(line_number,line,problem)
    }

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

    def validSectionName(text: String) : Boolean =
      text == "others" || text.substring(0,2) == "<<" && text.substring(text.length-2) == ">>"

    sealed abstract class Line
    case class NodeLine(id: String, level: Int, heading: String) extends Line
    object BeginCommentLine extends Line
    object EndCommentLine extends Line
    case class BeginSectionLine(indentation: Int, section_name: String) extends Line
    case class EndSectionLine(section_name: String) extends Line
    object VerbatimLine extends Line
    case class PropertyLine(name: String, value: String) extends Line
    case class TextLine(text: String) extends Line

    class LineParser(val comment_marker: String) {
      val quoted_comment_marker = "\\Q%s\\E".format(comment_marker)
      val sentinel = comment_marker + "@"
      val node_regex = "%s@\\+node:(.*?):\\s*(\\*?[0-9]*\\*) (.*)".format(quoted_comment_marker).r
      val begin_comment_regex = "%s@\\+at\\z".format(quoted_comment_marker).r
      val comment_line_starter = comment_marker + " "
      val end_comment_regex = "%s@@c\\z".format(quoted_comment_marker).r
      val begin_section_regex = "(\\s*)%s@\\+(.*)".format(quoted_comment_marker).r
      val end_section_regex = "%s@-(.*)".format(quoted_comment_marker).r
      val property_regex = "%s@@([^\\s]*) (.*)\\z".format(quoted_comment_marker).r
      val verbatim_text = "%s@verbatim".format(comment_marker)
      def apply(line: String) : Line = {
        begin_comment_regex.findPrefixMatchOf(line).map({m =>
          return BeginCommentLine
        })
        end_comment_regex.findPrefixMatchOf(line).map({m =>
          return EndCommentLine
        })
        node_regex.findPrefixMatchOf(line).map({m =>
          return NodeLine(m.group(1),parseLevel(m.group(2)),m.group(3))
        })
        begin_section_regex.findPrefixMatchOf(line).map({m =>
          val section_name = m.group(2)
	  if(validSectionName(section_name))
            return BeginSectionLine(m.group(1).length,section_name)
	  else
	    throw InvalidSectionName(section_name)
        })
        end_section_regex.findPrefixMatchOf(line).map({m =>
          return EndSectionLine(m.group(1))
        })
        if(line == verbatim_text) return VerbatimLine
        property_regex.findPrefixMatchOf(line).map({m =>
          return PropertyLine(m.group(1),m.group(2))
        })
        if(line.startsWith(sentinel)) throw UnrecognizedSentinel
        TextLine(line)
      }
      def extractCommentLine(line: String): String =
        if(line.startsWith(comment_line_starter))
          line.substring(comment_line_starter.length)
        else
          throw UnmatchedBeginComment
    }

    def parse(lines: Iterator[String]) : Node = {
      var line_number = 0
      def nextLine : String = {
        line_number = line_number + 1
        if(!lines.hasNext) throw UnexpectedEndOfFile
        lines.next
      }
      var current_body : StringBuilder = new StringBuilder

      def parseHeader : (String,String) = {
        while(lines.hasNext) {
          val line = nextLine
          header_regex.findPrefixMatchOf(line) match {
            case None => {
              current_body.append("@first ")
              current_body.append(line)
              current_body.append('\n')
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
  }
  object XMLParser {
    import scala.collection.mutable.ListBuffer
    class ParseError extends Exception
    object NotALeoFile extends ParseError
    case class TooManyHeadings(id: String,n: Int) extends ParseError
    case class NodeDefinitionAppearsMultipleTimes(id: String) extends ParseError
    case class UnmatchedTNode(id: String) extends ParseError

    case class ParseResult(tree: Tree, expanded_nodes: List[String])

    def parse(xml: scala.xml.Node): ParseResult = {
      val tree = new Tree
      val expanded_nodes_builder = new ListBuffer[String]
      (xml \ "vnodes" \ "v").foreach(parseVNode(tree.nodemap,tree.root,expanded_nodes_builder))
      for (tnode <- xml \ "tnodes" \ "t"; id = (tnode \ "@tx").text)
        tree.nodemap.getOrElse(id,{throw UnmatchedTNode(id)}).body = tnode.text
      ParseResult(tree,expanded_nodes_builder.result)
    }

    def parseVNode(nodemap: HashMap[String,Node], parent: Parent, expanded_nodes_builder: ListBuffer[String])(vnode: scala.xml.Node) {
      val id = (vnode \ "@t").text
      val maybe_heading = {
        val heading_nodes = vnode \ "vh"
        heading_nodes.size match {
          case 0 => None
          case 1 => Some(heading_nodes.text)
          case _ => throw TooManyHeadings(id,heading_nodes.size)
        }
      }
      val node = nodemap.getOrElseUpdate(id,{new Node(id,null,"")})
      for(heading <- maybe_heading) {
        if(!node.isPlaceholder)
          throw NodeDefinitionAppearsMultipleTimes(id)
        else {
          node.heading = heading
          (vnode \ "v").foreach(parseVNode(nodemap,node,expanded_nodes_builder))
        }
      }
      if((vnode \ "@a").text.contains('E'))
        expanded_nodes_builder += id
      for(child_id <- (vnode \ "@expanded").text.split(',') if child_id.nonEmpty)
        expanded_nodes_builder += child_id
      parent.appendChild(node)
    }
  }
  package Testing {
    class ParserSpecification extends org.scalatest.Spec with org.scalatest.matchers.ShouldMatchers {
      import Parser._

      describe("The level parser should correctly parse") {
        it("*") { parseLevel("*") should be (1) }
        it("**") { parseLevel("**") should be (2) }
        it("*3*") { parseLevel("*3*") should be (3) }
        it("*4*") { parseLevel("*4*") should be (4) }
      }

      val empty_file =
        """|#@+leo-ver=5-thin
           |#@+node:gcross.20101205182001.1356: * @thin node.cpp
           |#@-leo
           |""".stripMargin
      val single_node_file_with_content =
        """|Hello,
           |world!
           |#@+leo-ver=5-thin
           |#@+node:namegoeshere: * @thin node.cpp
           |foo
           |Bar
           |#@-leo
           |""".stripMargin
      val single_node_file_with_explicitly_ended_comment =
        """|#@+leo-ver=5-thin
           |#@+node:namegoeshere: * @thin node.cpp
           |pre
           |#@+at
           |# comment
           |# goes
           |# here
           |#@@c
           |post
           |#@-leo
           |""".stripMargin
      val single_node_file_with_comment_ended_by_end_of_file =
        """|#@+leo-ver=5-thin
           |#@+node:namegoeshere: * @thin node.cpp
           |pre
           |#@+at
           |# comment
           |# goes
           |# here
           |#@-leo
           |""".stripMargin
      val single_node_file_with_properties =
        """|#@+leo-ver=5-thin
           |#@+node:namegoeshere: * @thin node.cpp
           |A
           |#@@language value1
           |#@@tabwidth value2
           |B
           |#@-leo
           |""".stripMargin
      val file_with_single_named_section =
        """|#@+leo-ver=5-thin
           |#@+node:name: * @thin node.cpp
           |foo
           |#@+<< Section >>
           |#@+node:nodeid: ** << Section >>
           |content
           |#@-<< Section >>
           |bar
           |#@-leo
           |""".stripMargin
      val file_with_single_named_section_with_properties =
        """|#@+leo-ver=5-thin
           |#@+node:name: * @thin node.cpp
           |foo
           |#@@language value
           |#@+<< Section >>
           |#@+node:nodeid: ** << Section >>
           |#@@language value
           |content
           |#@-<< Section >>
           |bar
           |#@-leo
           |""".stripMargin
      val file_with_nested_others_sections =
        """|#@+leo-ver=5-thin
           |#@+node:name: * @thin node.cpp
           |pre1
           |#@+others
           |#@+node:ay: ** A
           |content of A
           |#@+node:ay1: *3* 1
           |content of A1
           |#@+node:ay2: *3* 2
           |content of A2
           |#@+node:bee: ** B
           |content of B
           |#@+node:bee1: *3* 1
           |content of B1
           |#@+node:bee1a: *4* a
           |content of B1a
           |#@-others
           |post1
           |#@-leo
           |""".stripMargin
      val file_with_nested_others_sections_with_comments =
        """|#@+leo-ver=5-thin
           |#@+node:name: * @thin node.cpp
           |pre1
           |#@+at
           |# comment 1
           |#@@c
           |#@+others
           |#@+node:ay: ** A
           |content of A
           |#@+at
           |# comment A
           |#@+node:ay1: *3* 1
           |content of A1
           |#@+at
           |# comment A1
           |#@+node:ay2: *3* 2
           |content of A2
           |#@+at
           |# comment A2
           |#@+node:bee: ** B
           |content of B
           |#@+at
           |# comment B
           |#@+node:bee1: *3* 1
           |content of B1
           |#@+at
           |# comment B1
           |#@+node:bee1a: *4* a
           |content of B1a
           |#@+at
           |# comment B1a
           |#@-others
           |post1
           |#@-leo
           |""".stripMargin

      describe("The node parser should correctly parse") {
        it("an empty file") {
          parse(empty_file.lines).toYAML should be(
            """|id: gcross.20101205182001.1356
               |heading: @thin node.cpp
               |body: ""
               |properties:
               |children:
               |""".stripMargin
          )
        }
        it("a single-node file with content") {
          parse(single_node_file_with_content.lines).toYAML should be(
            """|id: namegoeshere
               |heading: @thin node.cpp
               |body: "@first Hello,\n@first world!\nfoo\nBar\n"
               |properties:
               |children:
               |""".stripMargin
          )
        }
        it("a single-node file with a comment ended by @c") {
          parse(single_node_file_with_explicitly_ended_comment.lines).toYAML should be(
            """|id: namegoeshere
               |heading: @thin node.cpp
               |body: "pre\n@\ncomment\ngoes\nhere\n@c\npost\n"
               |properties:
               |children:
               |""".stripMargin
          )
        }
        it("a single-node file with a comment ended by the end of file") {
          parse(single_node_file_with_comment_ended_by_end_of_file.lines).toYAML should be(
            """|id: namegoeshere
               |heading: @thin node.cpp
               |body: "pre\n@\ncomment\ngoes\nhere\n"
               |properties:
               |children:
               |""".stripMargin
          )
        }
        it("a single-node file with properties") {
          parse(single_node_file_with_properties.lines).toYAML should be(
            """|id: namegoeshere
               |heading: @thin node.cpp
               |body: "A\n@language value1\n@tabwidth value2\nB\n"
               |properties:
               |    language: value1
               |    tabwidth: value2
               |children:
               |""".stripMargin
          )
        }
        it("a file with a single named section") {
          parse(file_with_single_named_section.lines).toYAML should be(
            """|id: name
               |heading: @thin node.cpp
               |body: "foo\n<< Section >>\nbar\n"
               |properties:
               |children:
               |  - id: nodeid
               |    heading: << Section >>
               |    body: "content\n"
               |    properties:
               |    children:
               |""".stripMargin
          )
        }
        it("a file with a single named section with properties") {
          parse(file_with_single_named_section_with_properties.lines).toYAML should be(
            """|id: name
               |heading: @thin node.cpp
               |body: "foo\n@language value\n<< Section >>\nbar\n"
               |properties:
               |    language: value
               |children:
               |  - id: nodeid
               |    heading: << Section >>
               |    body: "@language value\ncontent\n"
               |    properties:
               |        language: value
               |    children:
               |""".stripMargin
          )
        }
        it("a file with nested others sections") {
          parse(file_with_nested_others_sections.lines).toYAML should be(
            """|id: name
               |heading: @thin node.cpp
               |body: "pre1\n@others\npost1\n"
               |properties:
               |children:
               |  - id: ay
               |    heading: A
               |    body: "content of A\n"
               |    properties:
               |    children:
               |      - id: ay1
               |        heading: 1
               |        body: "content of A1\n"
               |        properties:
               |        children:
               |      - id: ay2
               |        heading: 2
               |        body: "content of A2\n"
               |        properties:
               |        children:
               |  - id: bee
               |    heading: B
               |    body: "content of B\n"
               |    properties:
               |    children:
               |      - id: bee1
               |        heading: 1
               |        body: "content of B1\n"
               |        properties:
               |        children:
               |          - id: bee1a
               |            heading: a
               |            body: "content of B1a\n"
               |            properties:
               |            children:
               |""".stripMargin
          )
        }
        it("a file with nested others sections with comments") {
          parse(file_with_nested_others_sections_with_comments.lines).toYAML should be(
            """|id: name
               |heading: @thin node.cpp
               |body: "pre1\n@\ncomment 1\n@c\n@others\npost1\n"
               |properties:
               |children:
               |  - id: ay
               |    heading: A
               |    body: "content of A\n@\ncomment A\n"
               |    properties:
               |    children:
               |      - id: ay1
               |        heading: 1
               |        body: "content of A1\n@\ncomment A1\n"
               |        properties:
               |        children:
               |      - id: ay2
               |        heading: 2
               |        body: "content of A2\n@\ncomment A2\n"
               |        properties:
               |        children:
               |  - id: bee
               |    heading: B
               |    body: "content of B\n@\ncomment B\n"
               |    properties:
               |    children:
               |      - id: bee1
               |        heading: 1
               |        body: "content of B1\n@\ncomment B1\n"
               |        properties:
               |        children:
               |          - id: bee1a
               |            heading: a
               |            body: "content of B1a\n@\ncomment B1a\n"
               |            properties:
               |            children:
               |""".stripMargin
          )
        }
      }
      describe("The node tangler should correctly parse") {
        it("an empty file") {
          parse(empty_file.lines).writeToString should be(empty_file)
        }
        it("a single-node file with content") {
          parse(single_node_file_with_content.lines).writeToString should be(single_node_file_with_content)
        }
        it("a single-node file with a comment ended by @c") {
          parse(single_node_file_with_explicitly_ended_comment.lines).writeToString should be(single_node_file_with_explicitly_ended_comment)
        }
        it("a single-node file with a comment ended by the end of file") {
          parse(single_node_file_with_comment_ended_by_end_of_file.lines).writeToString should be(single_node_file_with_comment_ended_by_end_of_file)
        }
        it("a single-node file with properties") {
          parse(single_node_file_with_properties.lines).writeToString should be(single_node_file_with_properties)
        }
        it("a file with a single named section") {
          parse(file_with_single_named_section.lines).writeToString should be(file_with_single_named_section)
        }
        it("a file with a single named section with properties") {
          parse(file_with_single_named_section_with_properties.lines).writeToString should be(file_with_single_named_section_with_properties)
        }
        it("a file with nested others sections") {
          parse(file_with_nested_others_sections.lines).writeToString should be(file_with_nested_others_sections)
        }
        it("a file with nested others sections with comments") {
          parse(file_with_nested_others_sections_with_comments.lines).writeToString should be(file_with_nested_others_sections_with_comments)
        }
      }
    }
    object ParserSpecification extends org.scalacheck.Properties("Parser") {
      import org.scalacheck.Arbitrary
      import org.scalacheck.Arbitrary.arbitrary
      import org.scalacheck.Gen
      import org.scalacheck.Gen.{alphaStr,choose,listOf1,oneOf,posNum}
      import org.scalacheck.Prop.{=?,forAll}
      import Parser._

      case class Comment(val string: String) { override def toString = string }
      implicit def unwrapComment(c: Comment) : String = c.string
      implicit val arbComment = Arbitrary[Comment] {
        listOf1[Char](choose(33,39)).map(s => Comment(s.mkString))
      }

      implicit val arbChar: Arbitrary[Char] = Arbitrary(
        oneOf(Gen.choose(Char.MinValue,0xD800-1),Gen.choose(0xDFFF+1,Char.MaxValue))
      )
      implicit val arbString = Arbitrary[String](arbitrary[List[Char]].map(_.mkString))

      property("level") = forAll { i: Int => i == parseLevel("*%s*".format(i)) }
      property("begin comment") = forAll { (c: Comment) => =?(BeginCommentLine,new LineParser(c)("%s@+at".format(c))) }
      property("verbatim") = forAll { (c: Comment) => =?(VerbatimLine,new LineParser(c)("%s@verbatim".format(c))) }
      property("node") = forAll(arbitrary[Comment],alphaStr,choose(3,20),arbitrary[String]) { (c,name,level:Int,heading) => =?(NodeLine(name,level,heading),new LineParser(c)("%s@+node:%s: *%s* %s".format(c,name,level,heading))) }
      property("begin section (<<name>>)") = forAll(arbitrary[Comment],choose(0,20),arbitrary[String]) { (c,indentation:Int,section_name) => =?(BeginSectionLine(indentation,"<<%s>>".format(section_name)),new LineParser(c)("%s%s@+<<%s>>".format(" "*indentation,c,section_name))) }
      property("begin section (others)") = forAll(arbitrary[Comment],choose(0,20)) { (c,indentation:Int) => =?(BeginSectionLine(indentation,"others"),new LineParser(c)("%s%s@+others".format(" "*indentation,c))) }
      property("property") = forAll(arbitrary[Comment],alphaStr,alphaStr) { (c,key,value) => =?(PropertyLine(key,value),new LineParser(c)("%s@@%s %s".format(c,key,value))) }
      property("end section") = forAll(arbitrary[Comment],arbitrary[String]) { (c,section_name) => =?(EndSectionLine(section_name),new LineParser(c)("%s@-%s".format(c,section_name))) }
    }
    class XMLParserSpecification extends org.scalatest.Spec with org.scalatest.matchers.ShouldMatchers {
      import XMLParser._

      describe("The xml parser should correctly parse") {
        it("an empty file") {
          val ParseResult(tree,expanded_nodes) = parse(<leo_file/>)
          tree.root.toYAML should be(
            """|properties:
               |children:
               |""".stripMargin
          )
          expanded_nodes should be (List())
        }
        it("a file with a single vnode") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id"><vh>heading</vh></v>
            </vnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id
               |    heading: heading
               |    body: ""
               |    properties:
               |    children:
               |""".stripMargin
          )
        }
        it("a file with a single vnode with expanded nodes") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id" a="E" expanded="id2,id3,"><vh>heading</vh></v>
            </vnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id
               |    heading: heading
               |    body: ""
               |    properties:
               |    children:
               |""".stripMargin
          )
          expanded_nodes should be (List("id","id2","id3"))
        }
        it("a file with a single vnode with text") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id"><vh>heading</vh></v>
            </vnodes>
            <tnodes>
              <t tx="id">Body goes here</t>
            </tnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id
               |    heading: heading
               |    body: "Body goes here"
               |    properties:
               |    children:
               |""".stripMargin
          )
        }
        it("a file with two vnodes with text") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id1"><vh>heading1</vh></v>
              <v t="id2"><vh>heading2</vh></v>
            </vnodes>
            <tnodes>
              <t tx="id1">First body goes here</t>
              <t tx="id2">Second body goes here</t>
            </tnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id1
               |    heading: heading1
               |    body: "First body goes here"
               |    properties:
               |    children:
               |  - id: id2
               |    heading: heading2
               |    body: "Second body goes here"
               |    properties:
               |    children:
               |""".stripMargin
          )
          expanded_nodes should be (List())
        }
        it("a file with a single cloned vnode with text and definition before clone") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id"><vh>heading</vh></v>
              <v t="id"></v>
            </vnodes>
            <tnodes>
              <t tx="id">Body goes here</t>
            </tnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id
               |    heading: heading
               |    body: "Body goes here"
               |    properties:
               |    children:
               |  - id: id
               |    heading: heading
               |    body: "Body goes here"
               |    properties:
               |    children:
               |""".stripMargin
          )
        }
        it("a file with a single cloned vnode with text and definition after clone") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id"></v>
              <v t="id"><vh>heading</vh></v>
            </vnodes>
            <tnodes>
              <t tx="id">Body goes here</t>
            </tnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id
               |    heading: heading
               |    body: "Body goes here"
               |    properties:
               |    children:
               |  - id: id
               |    heading: heading
               |    body: "Body goes here"
               |    properties:
               |    children:
               |""".stripMargin
          )
        }
        it("a file with nested vnodes") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id1"><vh>heading1</vh></v>
              <v t="id2"><vh>heading2</vh>
                <v t="id2a"><vh>heading2a</vh>
                  <v t="id2a1"><vh>heading2a1</vh></v>
                  <v t="id2a2"><vh>heading2a2</vh></v>
                </v>
                <v t="id2b"><vh>heading2b</vh></v>
              </v>
            </vnodes>
            <tnodes>
              <t tx="id1">First body goes here</t>
              <t tx="id2">Second body goes here</t>
              <t tx="id2a2">Nested body goes here</t>
            </tnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id1
               |    heading: heading1
               |    body: "First body goes here"
               |    properties:
               |    children:
               |  - id: id2
               |    heading: heading2
               |    body: "Second body goes here"
               |    properties:
               |    children:
               |      - id: id2a
               |        heading: heading2a
               |        body: ""
               |        properties:
               |        children:
               |          - id: id2a1
               |            heading: heading2a1
               |            body: ""
               |            properties:
               |            children:
               |          - id: id2a2
               |            heading: heading2a2
               |            body: "Nested body goes here"
               |            properties:
               |            children:
               |      - id: id2b
               |        heading: heading2b
               |        body: ""
               |        properties:
               |        children:
               |""".stripMargin
          )
          expanded_nodes should be (List())
        }
        it("a file with nested vnodes including clones") {
          val ParseResult(tree,expanded_nodes) = parse(
            <leo_file>
            <vnodes>
              <v t="id1"><vh>heading1</vh>
                <v t="id2a"/>
              </v>
              <v t="id2"><vh>heading2</vh>
                <v t="id2a"><vh>heading2a</vh>
                  <v t="id2a1"><vh>heading2a1</vh></v>
                  <v t="id2a2"><vh>heading2a2</vh></v>
                </v>
                <v t="id2b"><vh>heading2b</vh>
                    <v t="id1"/>
                </v>
              </v>
            </vnodes>
            <tnodes>
              <t tx="id1">First body goes here</t>
              <t tx="id2">Second body goes here</t>
              <t tx="id2a2">Nested body goes here</t>
            </tnodes>
            </leo_file>
          )
          tree.root.toYAML should be(
            """|properties:
               |children:
               |  - id: id1
               |    heading: heading1
               |    body: "First body goes here"
               |    properties:
               |    children:
               |      - id: id2a
               |        heading: heading2a
               |        body: ""
               |        properties:
               |        children:
               |          - id: id2a1
               |            heading: heading2a1
               |            body: ""
               |            properties:
               |            children:
               |          - id: id2a2
               |            heading: heading2a2
               |            body: "Nested body goes here"
               |            properties:
               |            children:
               |  - id: id2
               |    heading: heading2
               |    body: "Second body goes here"
               |    properties:
               |    children:
               |      - id: id2a
               |        heading: heading2a
               |        body: ""
               |        properties:
               |        children:
               |          - id: id2a1
               |            heading: heading2a1
               |            body: ""
               |            properties:
               |            children:
               |          - id: id2a2
               |            heading: heading2a2
               |            body: "Nested body goes here"
               |            properties:
               |            children:
               |      - id: id2b
               |        heading: heading2b
               |        body: ""
               |        properties:
               |        children:
               |          - id: id1
               |            heading: heading1
               |            body: "First body goes here"
               |            properties:
               |            children:
               |              - id: id2a
               |                heading: heading2a
               |                body: ""
               |                properties:
               |                children:
               |                  - id: id2a1
               |                    heading: heading2a1
               |                    body: ""
               |                    properties:
               |                    children:
               |                  - id: id2a2
               |                    heading: heading2a2
               |                    body: "Nested body goes here"
               |                    properties:
               |                    children:
               |""".stripMargin
          )
          expanded_nodes should be (List())
        }
      }
    }
  }
}
