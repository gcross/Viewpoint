//@+leo-ver=5-thin
//@+node:gcross.20110408155929.1293: * @file Parser.scala
//@@language Scala
package viewpoint.backend.crosswhite.testing

//@+<< Imports >>
//@+node:gcross.20110412144451.1427: ** << Imports >>
import scala.collection.mutable.HashSet
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.backend.crosswhite.model._
import viewpoint.backend.crosswhite.parser._
//@-<< Imports >>

//@+others
//@+node:gcross.20110408155929.1294: ** ParserExamples
object ParserExamples {
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
//@@raw
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
//@@end_raw
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
}
//@+node:gcross.20110408155929.1295: ** ParserSpecification
class ParserSpecification extends Spec with ShouldMatchers {
  import Parser._
  import ParserExamples._

  describe("The level parser should correctly parse") {
    it("*") { parseLevel("*") should be (1) }
    it("**") { parseLevel("**") should be (2) }
    it("*3*") { parseLevel("*3*") should be (3) }
    it("*4*") { parseLevel("*4*") should be (4) }
  }

  describe("The node parser should correctly parse") {
    it("an empty file") {
      parseOrThrow(empty_file.lines).toYAML should be(
        """|id: gcross.20101205182001.1356
           |heading: @thin node.cpp
           |body: ""
           |properties:
           |children:
           |""".stripMargin
      )
    }
    it("a single-node file with content") {
      parseOrThrow(single_node_file_with_content.lines).toYAML should be(
        """|id: namegoeshere
           |heading: @thin node.cpp
           |body: "@first Hello,\n@first world!\nfoo\nBar\n"
           |properties:
           |children:
           |""".stripMargin
      )
    }
    it("a single-node file with a comment ended by @c") {
      parseOrThrow(single_node_file_with_explicitly_ended_comment.lines).toYAML should be(
        """|id: namegoeshere
           |heading: @thin node.cpp
           |body: "pre\n@\ncomment\ngoes\nhere\n@c\npost\n"
           |properties:
           |children:
           |""".stripMargin
      )
    }
    it("a single-node file with a comment ended by the end of file") {
      parseOrThrow(single_node_file_with_comment_ended_by_end_of_file.lines).toYAML should be(
        """|id: namegoeshere
           |heading: @thin node.cpp
           |body: "pre\n@\ncomment\ngoes\nhere\n"
           |properties:
           |children:
           |""".stripMargin
      )
    }
    it("a single-node file with properties") {
      parseOrThrow(single_node_file_with_properties.lines).toYAML should be(
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
//@@raw
    it("a file with a single named section") {
      parseOrThrow(file_with_single_named_section.lines).toYAML should be(
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
      parseOrThrow(file_with_single_named_section_with_properties.lines).toYAML should be(
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
//@@end_raw
    it("a file with nested others sections") {
      parseOrThrow(file_with_nested_others_sections.lines).toYAML should be(
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
      parseOrThrow(file_with_nested_others_sections_with_comments.lines).toYAML should be(
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
      parseOrThrow(empty_file.lines).writeToString should be(empty_file)
    }
    it("a single-node file with content") {
      parseOrThrow(single_node_file_with_content.lines).writeToString should be(single_node_file_with_content)
    }
    it("a single-node file with a comment ended by @c") {
      parseOrThrow(single_node_file_with_explicitly_ended_comment.lines).writeToString should be(single_node_file_with_explicitly_ended_comment)
    }
    it("a single-node file with a comment ended by the end of file") {
      parseOrThrow(single_node_file_with_comment_ended_by_end_of_file.lines).writeToString should be(single_node_file_with_comment_ended_by_end_of_file)
    }
    it("a single-node file with properties") {
      parseOrThrow(single_node_file_with_properties.lines).writeToString should be(single_node_file_with_properties)
    }
    it("a file with a single named section") {
      parseOrThrow(file_with_single_named_section.lines).writeToString should be(file_with_single_named_section)
    }
    it("a file with a single named section with properties") {
      parseOrThrow(file_with_single_named_section_with_properties.lines).writeToString should be(file_with_single_named_section_with_properties)
    }
    it("a file with nested others sections") {
      parseOrThrow(file_with_nested_others_sections.lines).writeToString should be(file_with_nested_others_sections)
    }
    it("a file with nested others sections with comments") {
      parseOrThrow(file_with_nested_others_sections_with_comments.lines).writeToString should be(file_with_nested_others_sections_with_comments)
    }
  }
}
object ParserSpecification extends org.scalacheck.Properties("Parser") {
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary
  import org.scalacheck.Gen
  import org.scalacheck.Gen.{alphaStr,choose,listOf,listOf1,oneOf,posNum}
  import org.scalacheck.Prop.{=?,all,forAll}
  import Parser._
  import ParserExamples._

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
//@@raw
  property("begin section (<<name>>)") = forAll(arbitrary[Comment],choose(0,20),arbitrary[String]) { (c,indentation:Int,section_name) => =?(BeginSectionLine(indentation,"<<%s>>".format(section_name)),new LineParser(c)("%s%s@+<<%s>>".format(" "*indentation,c,section_name))) }
//@@end_raw
  property("begin section (others)") = forAll(arbitrary[Comment],choose(0,20)) { (c,indentation:Int) => =?(BeginSectionLine(indentation,"others"),new LineParser(c)("%s%s@+others".format(" "*indentation,c))) }
  property("property") = forAll(arbitrary[Comment],alphaStr,alphaStr) { (c,key,value) => =?(PropertyLine(key,value),new LineParser(c)("%s@@%s %s".format(c,key,value))) }
  property("end section") = forAll(arbitrary[Comment],arbitrary[String]) { (c,section_name) => =?(EndSectionLine(section_name),new LineParser(c)("%s@-%s".format(c,section_name))) }
  property("parseAllAsynchronously") = forAll(
    listOf(oneOf(
      arbitrary[String],
      oneOf(Array(
        empty_file,
        single_node_file_with_content,
        single_node_file_with_explicitly_ended_comment,
        single_node_file_with_comment_ended_by_end_of_file,
        single_node_file_with_properties,
        file_with_single_named_section,
        file_with_single_named_section_with_properties,
        file_with_nested_others_sections,
        file_with_nested_others_sections_with_comments
      ))
    )).map(_.toArray)
  ) { sources =>
    val parse_list = (0 until sources.size) zip (sources.map(source => {() => source.lines}))
    val result_channel = new scala.actors.Channel[Parser.ParseCompletionMessage[Int]]
    Parser.parseAllAsynchronously[Int](parse_list.iterator,result_channel)
    val results = result_channel receive { case Parser.CompletedParseAll(results) => results }
    val seen = new HashSet[Int]
    def convertCorrectResultToString(result: Either[Exception,Node]) =
      result match {
        case Left(e) => Left(e)
        case Right(node) => Right(node.toYAML)
      }
    all(
      =?(sources.size,results.size),
      all(results.map({case (id,result) =>
        all(
          seen.add(id),
          =?(
            convertCorrectResultToString(parse(sources(id).lines)),
            convertCorrectResultToString(result)
          )
        )
      }).toSeq : _*)
    )
  }
  property("writeAllAsynchronously") = forAll { (bodies_and_flags : List[(String,Boolean)]) =>
    val write_list =
      for((body,flag) <- bodies_and_flags)
      yield (
//@@raw
        new Node("id","heading",body + (if(flag) "\n<< Bad >>\n" else "")),
//@@end_raw
        { () => new java.io.StringWriter }
      )
    val result_channel = new scala.actors.Channel[Parser.WriteCompletionMessage]
    Parser.writeAllAsynchronously(write_list.iterator,result_channel)
    val results = result_channel receive { case Parser.CompletedWriteAll(results) => results }
    all(
      =?(write_list.size,results.size),
      all(results.map({case (node,writer,maybe_problem) => {
        val actual_result =
          maybe_problem match {
            case None => Right(writer.toString)
            case Some(problem) => Left(problem.toString)
          }
        val correct_result =
          try {
            Right(node.writeToString)
          } catch {
            case e: Exception => Left(e.toString)
          }
        =?(correct_result,actual_result)
      }}).toSeq : _*)
    )
  }
}
//@+node:gcross.20110408155929.1296: ** XMLParserSpecification
class XMLParserSpecification extends Spec with ShouldMatchers {
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
//@-others
//@-leo
