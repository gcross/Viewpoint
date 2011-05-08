//@+leo-ver=5-thin
//@+node:gcross.20110408155929.1293: * @file Parser.scala
//@@language Scala
package viewpoint.backend.crosswhite.testing

//@+<< Imports >>
//@+node:gcross.20110412144451.1427: ** << Imports >>
import scala.collection.{immutable,Map}
import scala.collection.mutable.{ArrayBuffer,HashSet}
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.backend.crosswhite.model._
import viewpoint.backend.crosswhite.parser._
//@-<< Imports >>

//@+others
//@+node:gcross.20110505183655.1897: ** object ParseExamples
object ParserExamples {
  val examples_with_yaml = immutable.ListMap[String,(String,String)](
    //@+others
    //@+node:gcross.20110505183655.1898: *3* an empty file.
    "an empty file" ->
      ("""|#@+leo-ver=5-thin
          |#@+node:gcross.20101205182001.1356: * @thin node.cpp
          |#@-leo
          |""".stripMargin

      ,"""|id: gcross.20101205182001.1356
          |heading: @thin node.cpp
          |body: ""
          |properties:
          |children:
          |""".stripMargin
      ),
    //@+node:gcross.20110505163410.6428: *3* a single-node file with content.
    "a single-node file with content." ->
      ("""|Hello,
          |world!
          |#@+leo-ver=5-thin
          |#@+node:namegoeshere: * @thin node.cpp
          |foo
          |Bar
          |#@-leo
          |""".stripMargin

      ,"""|id: namegoeshere
          |heading: @thin node.cpp
          |body: "@first Hello,\n@first world!\nfoo\nBar\n"
          |properties:
          |    first: "Hello,\nworld!"
          |children:
          |""".stripMargin
      ),
    //@+node:gcross.20110505163410.6429: *3* a single-node file with a comment ended by @c.
    "a single-node file with a comment ended by @c." ->
      ("""|#@+leo-ver=5-thin
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

      ,"""|id: namegoeshere
          |heading: @thin node.cpp
          |body: "pre\n@\ncomment\ngoes\nhere\n@c\npost\n"
          |properties:
          |children:
          |""".stripMargin
      ),
    //@+node:gcross.20110505183655.1896: *3* a single-node file with a comment ended by @c with text after the sentinels.
    "a single-node file with a comment ended by @c with text after the sentinels." ->
      ("""|#@+leo-ver=5-thin
          |#@+node:namegoeshere: * @thin node.cpp
          |pre
          |#@+at fried
          |# comment
          |# goes
          |# here
          |#@@c mars bars
          |post
          |#@-leo
          |""".stripMargin

      ,"""|id: namegoeshere
          |heading: @thin node.cpp
          |body: "pre\n@ fried\ncomment\ngoes\nhere\n@c mars bars\npost\n"
          |properties:
          |children:
          |""".stripMargin
      ),
    //@+node:gcross.20110505163410.6430: *3* a single-node file with a comment ended by the end of file.
    "a single-node file with a comment ended by the end of file." ->
      ("""|#@+leo-ver=5-thin
          |#@+node:namegoeshere: * @thin node.cpp
          |pre
          |#@+at
          |# comment
          |# goes
          |# here
          |#@-leo
          |""".stripMargin

      ,"""|id: namegoeshere
          |heading: @thin node.cpp
          |body: "pre\n@\ncomment\ngoes\nhere\n"
          |properties:
          |children:
          |""".stripMargin
      ),
    //@+node:gcross.20110505163410.6431: *3* a single-node file with properties.
    "a single-node file with properties." ->
      ("""|#@+leo-ver=5-thin
          |#@+node:namegoeshere: * @thin node.cpp
          |A
          |#@@language value1
          |#@@tabwidth value2
          |B
          |#@-leo
          |""".stripMargin

      ,"""|id: namegoeshere
          |heading: @thin node.cpp
          |body: "A\n@language value1\n@tabwidth value2\nB\n"
          |properties:
          |    language: "value1"
          |    tabwidth: "value2"
          |children:
          |""".stripMargin
      ),
    //@+node:gcross.20110505163410.6432: *3* a file with a single named section.
    //@@raw
"a file with a single named section." ->
  ("""|#@+leo-ver=5-thin
      |#@+node:name: * @thin node.cpp
      |foo
      |#@+<< Section >>
      |#@+node:nodeid: ** << Section >>
      |content
      |#@-<< Section >>
      |bar
      |#@-leo
      |""".stripMargin

  ,"""|id: name
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
  ),
    //@+node:gcross.20110507151400.1902: *3* a file with a single named and indented section.
    //@@raw
"a file with a single named and indented section." ->
  ("""|#@+leo-ver=5-thin
      |#@+node:name: * @thin node.cpp
      |foo
      |  #@+<< Section >>
      |  #@+node:nodeid: ** << Section >>
      |  content
      |  #@-<< Section >>
      |bar
      |#@-leo
      |""".stripMargin

  ,"""|id: name
      |heading: @thin node.cpp
      |body: "foo\n  << Section >>\nbar\n"
      |properties:
      |children:
      |  - id: nodeid
      |    heading: << Section >>
      |    body: "content\n"
      |    properties:
      |    children:
      |""".stripMargin
  ),
    //@+node:gcross.20110505163410.6433: *3* a file with a single named section with properties.
    //@@raw
"a file with a single named section with properties." ->
  ("""|#@+leo-ver=5-thin
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

  ,"""|id: name
      |heading: @thin node.cpp
      |body: "foo\n@language value\n<< Section >>\nbar\n"
      |properties:
      |    language: "value"
      |children:
      |  - id: nodeid
      |    heading: << Section >>
      |    body: "@language value\ncontent\n"
      |    properties:
      |        language: "value"
      |    children:
      |""".stripMargin
  ),
    //@+node:gcross.20110505163410.6434: *3* a file with nested others sections.
    "a file with nested others sections." ->
      ("""|#@+leo-ver=5-thin
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

      ,"""|id: name
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
      ),
    //@+node:gcross.20110505163410.6435: *3* a file with nested others sections with comments.
    "a file with nested others sections with comments." ->
      ("""|#@+leo-ver=5-thin
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

      ,"""|id: name
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
      ),
    //@+node:gcross.20110507151400.1904: *3* a file with a raw block inside a nested section, explicitly ended.
    //@@raw
"a file with a raw block inside a nested section, explicitly ended." ->
  ("""|#@+leo-ver=5-thin
      |#@+node:name: * @thin node.cpp
      |foo
      |  #@+<< Section >>
      |  #@+node:nodeid: ** << Section >>
      |  #@@raw
      |mars
      |bars
      |  #@@end_raw
      |  #@-<< Section >>
      |bar
      |#@-leo
      |""".stripMargin

  ,"""|id: name
      |heading: @thin node.cpp
      |body: "foo\n  << Section >>\nbar\n"
      |properties:
      |children:
      |  - id: nodeid
      |    heading: << Section >>
      |    body: "@raw\nmars\nbars\n@end_raw\n"
      |    properties:
      |    children:
      |""".stripMargin
  ),
    //@+node:gcross.20110507151400.1920: *3* a file with a raw block inside a nested section, implicitly ended by end of section.
    //@@raw
"a file with a raw block inside a nested section, implicitly ended by end of section." ->
  ("""|#@+leo-ver=5-thin
      |#@+node:name: * @thin node.cpp
      |foo
      |  #@+<< Section >>
      |  #@+node:nodeid: ** << Section >>
      |  #@@raw
      |mars
      |bars
      |  #@-<< Section >>
      |bar
      |#@-leo
      |""".stripMargin

  ,"""|id: name
      |heading: @thin node.cpp
      |body: "foo\n  << Section >>\nbar\n"
      |properties:
      |children:
      |  - id: nodeid
      |    heading: << Section >>
      |    body: "@raw\nmars\nbars\n"
      |    properties:
      |    children:
      |""".stripMargin
  ),
    //@+node:gcross.20110507151400.1922: *3* a file with a raw block inside a nested section, implicitly ended by beginning of sibling.
    //@@raw
"a file with a raw block inside a nested section, implicitly ended by beginning of sibling." ->
  ("""|#@+leo-ver=5-thin
      |#@+node:name: * @thin node.cpp
      |foo
      |  #@+others
      |  #@+node:nodeid: ** Sibling 1
      |  #@@raw
      |mars
      |bars
      |  #@+node:nodeid: ** Sibling 2
      |  content
      |  #@-others
      |bar
      |#@-leo
      |""".stripMargin

  ,"""|id: name
      |heading: @thin node.cpp
      |body: "foo\n  @others\nbar\n"
      |properties:
      |children:
      |  - id: nodeid
      |    heading: Sibling 1
      |    body: "@raw\nmars\nbars\n"
      |    properties:
      |    children:
      |  - id: nodeid
      |    heading: Sibling 2
      |    body: "content\n"
      |    properties:
      |    children:
      |""".stripMargin
  )
    //@-others
  )
  val examples = examples_with_yaml.mapValues(_._1)
}
//@+node:gcross.20110505163410.6424: ** class ParserSpecification
class ParserSpecification extends Spec with ShouldMatchers {
  //@+<< Imports >>
  //@+node:gcross.20110505163410.6437: *3* << Imports >>
  import Parser._
  import ParserExamples._
  //@-<< Imports >>
  //@+others
  //@+node:gcross.20110505163410.6425: *3* The level parser should correctly parse
  describe("The level parser should correctly parse") {
    it("*") { parseLevel("*") should be (1) }
    it("**") { parseLevel("**") should be (2) }
    it("*3*") { parseLevel("*3*") should be (3) }
    it("*4*") { parseLevel("*4*") should be (4) }
  }
  //@+node:gcross.20110505163410.6426: *3* The node parser should correctly parse
  describe("The node parser should correctly parse") {
    for((label,(input,correct_yaml)) <- examples_with_yaml) {
      it(label) { parseOrThrow(input.lines).toYAML should be(correct_yaml) }
    }
  }
  //@+node:gcross.20110505163410.6436: *3* The node tangler should correctly parse
  describe("The node tangler should correctly parse") {
    for((label,input) <- examples) {
      it(label) { parseOrThrow(input.lines).writeToString should be(input) }
    }
  }
  //@-others
}
//@+node:gcross.20110408155929.1295: ** object ParserSpecification
object ParserSpecification extends org.scalacheck.Properties("Parser") {
  //@+<< Imports >>
  //@+node:gcross.20110505163410.6447: *3* << Imports >>
  import org.scalacheck.Arbitrary
  import org.scalacheck.Arbitrary.arbitrary
  import org.scalacheck.Gen
  import org.scalacheck.Gen.{alphaStr,choose,listOf,listOf1,oneOf,posNum}
  import org.scalacheck.Prop.{=?,all,forAll,propBoolean}
  import Parser._
  import ParserExamples._
  //@-<< Imports >>

  //@+<< Generators >>
  //@+node:gcross.20110505163410.6448: *3* << Generators >>
  case class Comment(val string: String) { override def toString = string }
  implicit def unwrapComment(c: Comment) : String = c.string
  implicit val arbComment = Arbitrary[Comment] {
    listOf1[Char](choose(33,39)).map(s => Comment(s.mkString))
  }

  implicit val arbChar: Arbitrary[Char] = Arbitrary(
    oneOf(Gen.choose(Char.MinValue,0xD800-1),Gen.choose(0xDFFF+1,Char.MaxValue))
  )
  implicit val arbString = Arbitrary[String](arbitrary[List[Char]].map(_.mkString))
  //@-<< Generators >>

  //@+others
  //@+node:gcross.20110505163410.6449: *3* parseLevel
  property("parseLevel") = forAll { i: Int => i == parseLevel("*%s*".format(i)) }
  //@+node:gcross.20110505163410.6414: *3* Sentinel parser tests
  //@+node:gcross.20110505183655.1889: *4* BeginCommentSentinel
  property("BeginCommentSentinel (with text)") = forAll(arbitrary[Comment],alphaStr) {
    (c,s) =>
    =?(
      Some("@ " + s),
      new LineSentinels(c).BeginCommentSentinel.unapply("%s@+at %s".format(c,s))
    )
  }

  property("BeginCommentSentinel (without text)") = forAll(arbitrary[Comment]) {
    (c) =>
    =?(
      Some("@"),
      new LineSentinels(c).BeginCommentSentinel.unapply("%s@+at".format(c))
    )
  }
  //@+node:gcross.20110505163410.6420: *4* BeginSectionSentinel
  //@@raw
property("BeginSectionSentinel (<<name>>)") = forAll(arbitrary[Comment],choose(0,20),alphaStr) {
  (c,indentation:Int,section_name) =>
  =?(
    Some((indentation,"<<%s>>".format(section_name))),
    new LineSentinels(c).BeginSectionSentinel.unapply("%s%s@+<<%s>>".format(" "*indentation,c,section_name))
  )
}
  //@@end_raw
  property("BeginSectionSentinel (others)") = forAll(arbitrary[Comment],choose(0,20)) {
    (c,indentation:Int) =>
    =?(
      Some((indentation,"others")),
      new LineSentinels(c).BeginSectionSentinel.unapply("%s%s@+others".format(" "*indentation,c))
    )
  }
  //@+node:gcross.20110505183655.1891: *4* EndCommentSentinel
  property("EndCommentSentinel (with text)") = forAll(arbitrary[Comment],alphaStr) {
    (c,s) =>
    =?(
      Some("@c " + s),
      new LineSentinels(c).EndCommentSentinel.unapply("%s@@c %s".format(c,s))
    )
  }

  property("EndCommentSentinel (without text)") = forAll(arbitrary[Comment]) {
    (c) =>
    =?(
      Some("@c"),
      new LineSentinels(c).EndCommentSentinel.unapply("%s@@c".format(c))
    )
  }
  //@+node:gcross.20110505163410.6423: *4* EndSectionSentinel
  property("EndSectionSentinel") = forAll(arbitrary[Comment],alphaStr) {
    (c,section_name) =>
    =?(
      Some(List(section_name)),
      new LineSentinels(c).EndSectionSentinel.unapplySeq("%s@-%s".format(c,section_name))
    )
  }
  //@+node:gcross.20110505163410.6419: *4* NodeSentinel
  property("NodeSentinel") = forAll(arbitrary[Comment],alphaStr,choose(3,20),alphaStr) {
    (c,name,level:Int,heading) =>
    =?(
      Some((name,level,heading)),
      new LineSentinels(c).NodeSentinel.unapply("%s@+node:%s: *%s* %s".format(c,name,level,heading))
    )
  }
  //@+node:gcross.20110505163410.6422: *4* PropertySentinel
  property("PropertySentinel") = forAll(arbitrary[Comment],alphaStr,alphaStr) {
    (c,key,value) =>
    =?(
      Some(List(key,value)),
      new LineSentinels(c).PropertySentinel.unapplySeq("%s@@%s %s".format(c,key,value))
    )
  }
  //@+node:gcross.20110505163410.6452: *3* Asynchronous I/O tests
  //@+node:gcross.20110505163410.6450: *4* parseAllAsynchronously
  property("parseAllAsynchronously") = forAll(
    listOf(oneOf(
      arbitrary[String],
      oneOf(examples.values.toSeq)
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
  //@+node:gcross.20110505163410.6451: *4* writeAllAsynchronously
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
  //@-others
}
//@+node:gcross.20110408155929.1296: ** class XMLParserSpecification
class XMLParserSpecification extends Spec with ShouldMatchers {
  //@+<< Imports >>
  //@+node:gcross.20110505163410.6453: *3* << Imports >>
  import XMLParser._
  //@-<< Imports >>
  //@+<< Examples >>
  //@+node:gcross.20110505163410.6454: *3* << Examples >>
  val examples = immutable.ListMap[String,(scala.xml.Node,String,scala.xml.Node)](
    //@+others
    //@+node:gcross.20110505163410.6455: *4* an empty file.
    "an empty file." ->
      (<leo_file/>
      ,"""|children:
          |""".stripMargin
      ,
    //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes/>
<tnodes>
</tnodes>
</leo_file>
    //@@end_raw
      )
    ,
    //@+node:gcross.20110505163410.6456: *4* a file with a single vnode.
    "a file with a single vnode." ->
      (<leo_file>
       <leo_header file_format="2"/>
       <vnodes>
        <v t="id"><vh>heading</vh></v>
       </vnodes>
       </leo_file>
      ,"""|children:
          |  - id: id
          |    heading: heading
          |    body: ""
          |    properties:
          |    children:
          |""".stripMargin
      ,
    //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes>
<v t="id"><vh>heading</vh></v>
</vnodes>
<tnodes>
<t tx="id"></t>
</tnodes>
</leo_file>
    //@@end_raw
      )
    ,
    //@+node:gcross.20110505163410.6458: *4* a file with a single vnode with text.
    "a file with a single vnode with text." -> {
      val xml =
    //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes>
<v t="id"><vh>heading</vh></v>
</vnodes>
<tnodes>
<t tx="id">Body goes here</t>
</tnodes>
</leo_file>
    //@@end_raw
      (xml
      ,"""|children:
          |  - id: id
          |    heading: heading
          |    body: "Body goes here"
          |    properties:
          |    children:
          |""".stripMargin
      ,xml
      )
    }
    ,
    //@+node:gcross.20110505163410.6459: *4* a file with two vnodes with text.
    "a file with two vnodes with text." -> {
      val xml =
    //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes>
<v t="id1"><vh>heading1</vh></v>
<v t="id2"><vh>heading2</vh></v>
</vnodes>
<tnodes>
<t tx="id1">First body goes here</t>
<t tx="id2">Second body goes here</t>
</tnodes>
</leo_file>
    //@@end_raw
      (xml
      ,"""|children:
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
      ,xml
      )
    }
    ,
    //@+node:gcross.20110505163410.6460: *4* a file with a single cloned vnode with text and definition before clone.
    "a file with a single cloned vnode with text and definition before clone." -> {
      val xml =
    //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes>
<v t="id"><vh>heading</vh></v>
<v t="id"></v>
</vnodes>
<tnodes>
<t tx="id">Body goes here</t>
</tnodes>
</leo_file>
    //@@end_raw
      (xml
      ,"""|children:
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
      ,xml
      )
    }
    ,
    //@+node:gcross.20110505163410.6461: *4* a file with a single cloned vnode with text and definition after clone.
    "a file with a single cloned vnode with text and definition after clone." ->
      (<leo_file>
       <leo_header file_format="2"/>
       <vnodes>
        <v t="id"/>
        <v t="id"><vh>heading</vh></v>
       </vnodes>
       <tnodes>
        <t tx="id">Body goes here</t>
       </tnodes>
       </leo_file>
      ,"""|children:
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
      ,
    //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes>
<v t="id"><vh>heading</vh></v>
<v t="id"></v>
</vnodes>
<tnodes>
<t tx="id">Body goes here</t>
</tnodes>
</leo_file>
    //@@end_raw
      )
    ,
    //@+node:gcross.20110505163410.6462: *4* a file with nested vnodes.
    "a file with nested vnodes." -> {
      val xml =
    //@@raw
<leo_file>
<leo_header file_format="2"/>
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
<t tx="id2a"></t>
<t tx="id2a1"></t>
<t tx="id2a2">Nested body goes here</t>
<t tx="id2b"></t>
</tnodes>
</leo_file>
    //@@end_raw
      (xml
      ,"""|children:
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
      ,xml
      )
    }
    ,
    //@+node:gcross.20110505163410.6463: *4* a file with nested vnodes including clones.
    "a file with nested vnodes including clones." ->
      (<leo_file>
       <leo_header file_format="2"/>
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
      ,"""|children:
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
      ,
    //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes>
<v t="id1"><vh>heading1</vh>
<v t="id2a"><vh>heading2a</vh>
<v t="id2a1"><vh>heading2a1</vh></v>
<v t="id2a2"><vh>heading2a2</vh></v>
</v>
</v>
<v t="id2"><vh>heading2</vh>
<v t="id2a"/>
<v t="id2b"><vh>heading2b</vh>
<v t="id1"/>
</v>
</v>
</vnodes>
<tnodes>
<t tx="id1">First body goes here</t>
<t tx="id2">Second body goes here</t>
<t tx="id2a"></t>
<t tx="id2a1"></t>
<t tx="id2a2">Nested body goes here</t>
<t tx="id2b"></t>
</tnodes>
</leo_file>
    //@@end_raw
      )
    //@-others
  )
  //@-<< Examples >>
  //@+others
  //@+node:gcross.20110507151400.1885: *3* The xml parser should correctly parse
  describe("The xml parser should correctly parse") {
    for((label,(xml,yaml,_)) <- examples) {
      it(label) {
        parse(xml).root.toYAML should be (yaml)
      }
    }
  }
  //@+node:gcross.20110507151400.1887: *3* The xml serializer should correctly reserialize
  describe("The xml serializer should correctly reserialize") {
    for((label,(input_xml,_,correct_output_xml)) <- examples) {
      it(label) {
        serialize(parse(input_xml).root) should be (correct_output_xml)
      }
    }
  }
  //@-others
}
//@-others
//@-leo
