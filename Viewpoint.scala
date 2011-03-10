package Viewpoint {
  import java.util.HashMap
  import java.util.HashSet
  import java.util.LinkedList
  import scala.annotation.tailrec

  class Parent {
    var children = new LinkedList[Node]
    var properties = new HashMap[String,String]
    def getProperty(key: String) : String = properties.get(key)
    def setProperty(key: String, value: String) : String = properties.put(key,value)
  }

  class Node(val id: String, var heading: String, var body: String) extends Parent {
    import scala.collection.JavaConversions._
    var parents = new HashSet[Parent]
    override def getProperty(key: String) : String = {
      var value = properties.get(key)
      val iterator = parents.iterator
      while((value eq null) && iterator.hasNext) {
        value = iterator.next.getProperty(key)
      }
      value
    }
    def toYAML: String = {
      val builder = new StringBuilder
      appendYAML("",builder)
      builder.toString
    }
    def appendYAML(indentation: String, builder: StringBuilder): Unit = {
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

      builder.append(indentation)
      builder.append("properties:")
      builder.append('\n')
      val keys : Array[String] = properties.keySet.toArray(Array[String]())
      scala.util.Sorting.quickSort(keys)
      for(key <- keys) {
        builder.append(indentation)
        builder.append("    ")
        builder.append(key)
        builder.append(": ")
        builder.append(properties.get(key))
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
    case class BadLevelNumber(level: Int) extends ParseError
    object UnmatchedBeginSection extends ParseError
    object MismatchedEndSection extends ParseError
    object ContentAfterEndOfFileSentinel extends ParseError
    case class InvalidSectionName(section_name: String) extends ParseError {
      override def toString = "Invalid section name: " + section_name
    }
    object NodeNotFoundImmediatelyAfterBeginSection extends ParseError
    object UnexpectedEndOfFile extends ParseError
    object BadCommentSectionLine extends ParseError
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
    case class BeginSectionLine(indentation: Int, section_name: String) extends Line
    case class EndSectionLine(section_name: String) extends Line
    object VerbatimLine extends Line
    case class PropertyLine(name: String, value: String) extends Line
    case class TextLine(text: String) extends Line

    class LineParser(val comment_marker: String) {
      val quoted_comment_marker = "\\Q%s\\E".format(comment_marker)
      val node_regex = "%s@\\+node:(.*?):\\s*(\\*?[0-9]*\\*) (.*)".format(quoted_comment_marker).r
      val begin_comment_regex = "%s@\\+at\\z".format(quoted_comment_marker).r
      val comment_line_regex = "%s (.*)".format(quoted_comment_marker).r
      val end_comment_regex = "%s@@c\\z".format(quoted_comment_marker).r
      val begin_section_regex = "(\\s*)%s@\\+(.*)".format(quoted_comment_marker).r
      val end_section_regex = "%s@-(.*)".format(quoted_comment_marker).r
      val property_regex = "%s@@([^\\s]*) (.*)\\z".format(quoted_comment_marker).r
      val verbatim_text = "%s@verbatim".format(comment_marker)
      def apply(line: String) : Line = {
        begin_comment_regex.findPrefixMatchOf(line).map({m =>
          return BeginCommentLine
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
            current_node.parents.add(current_parent_node)
            current_parent_node.children.add(current_node)
          }
          case BeginSectionLine(indentation,section_name) => {
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
            current_node.parents.add(current_parent_node)
            current_parent_node.children.add(current_node)

            section_indentation_stack.push(current_section_indentation)
            section_level_stack.push(current_section_level)
            section_name_stack.push(current_section_name)

            current_section_indentation += indentation
            current_section_level = current_level
            current_section_name = section_name
          }
          case EndSectionLine(section_name) => {
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
            current_node.setProperty(name,value)
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
        } } catch {
          case (e : ParseError) => throw ParseErrorWithContext(line_number,line,e)
        }
      }
      if(current_level > 0) throw UnexpectedEndOfFile
      current_node
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

      describe("The node parser should correctly parse") {
        it("an empty file") {
          parse(
            """|#@+leo-ver=5-thin
               |#@+node:gcross.20101205182001.1356: * @thin node.cpp
               |#@-leo""".stripMargin.lines).toYAML should be(
            """|id: gcross.20101205182001.1356
               |heading: @thin node.cpp
               |body: ""
               |properties:
               |children:
               |""".stripMargin
          )
        }
        it("a single-node file with content") {
          parse(
            """|Hello,
               |world!
               |#@+leo-ver=5-thin
               |#@+node:namegoeshere: * @thin node.cpp
               |foo
               |Bar
               |#@-leo""".stripMargin.lines).toYAML should be(
            """|id: namegoeshere
               |heading: @thin node.cpp
               |body: "@first Hello,\n@first world!\nfoo\nBar\n"
               |properties:
               |children:
               |""".stripMargin
          )
        }
        it("a single-node file with properties") {
          parse(
            """|#@+leo-ver=5-thin
               |#@+node:namegoeshere: * @thin node.cpp
               |A
               |#@@key1 value1
               |#@@key2 value2
               |B
               |#@-leo""".stripMargin.lines).toYAML should be(
            """|id: namegoeshere
               |heading: @thin node.cpp
               |body: "A\n@key1 value1\n@key2 value2\nB\n"
               |properties:
               |    key1: value1
               |    key2: value2
               |children:
               |""".stripMargin
          )
        }
        it("a file with a single named section") {
          parse(
            """|#@+leo-ver=5-thin
               |#@+node:name: * @thin node.cpp
               |foo
               |#@+<< Section >>
               |#@+node:nodeid: ** << Section >>
               |content
               |#@-<< Section >>
               |bar
               |#@-leo""".stripMargin.lines).toYAML should be(
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
          parse(
            """|#@+leo-ver=5-thin
               |#@+node:name: * @thin node.cpp
               |foo
               |#@@key value
               |#@+<< Section >>
               |#@+node:nodeid: ** << Section >>
               |#@@key value
               |content
               |#@-<< Section >>
               |bar
               |#@-leo""".stripMargin.lines).toYAML should be(
            """|id: name
               |heading: @thin node.cpp
               |body: "foo\n@key value\n<< Section >>\nbar\n"
               |properties:
               |    key: value
               |children:
               |  - id: nodeid
               |    heading: << Section >>
               |    body: "@key value\ncontent\n"
               |    properties:
               |        key: value
               |    children:
               |""".stripMargin
          )
        }
        it("a file with nested others sections") {
          parse(
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
               |#@-leo""".stripMargin.lines).toYAML should be(
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
  }
}
