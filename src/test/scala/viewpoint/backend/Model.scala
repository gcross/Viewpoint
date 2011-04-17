//@+leo-ver=5-thin
//@+node:gcross.20110414153139.1487: * @file Model.scala
//@@language Scala
package viewpoint.backend.crosswhite.testing

//@+<< Imports >>
//@+node:gcross.20110414153139.1488: ** << Imports >>
import scala.collection.mutable.HashSet
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.backend.crosswhite.model._
import viewpoint.backend.crosswhite.parser._
import viewpoint.model.testing._
//@-<< Imports >>

//@+others
//@+node:gcross.20110414153139.1500: ** InterfaceSpecification
class InterfaceSpecification extends ModelSpecification({new Tree}) {}
//@+node:gcross.20110414153139.1492: ** NodeSpecification
class NodeSpecification extends Spec with ShouldMatchers {
  describe("The node comparer should work for") {
    def test(correct_result: Boolean, n1: scala.xml.Node, n2: scala.xml.Node) {
      import XMLParser._
      val ParseResult(tree1,_) = parse(n1)
      val ParseResult(tree2,_) = parse(n2)
      (tree1.root.children(0) === tree2.root.children(0)) should be (correct_result)
    }
    it("identical singletons") {
      test(true,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>
         )
    }
    it("singletons with different ids") {
      test(false,
           <leo_file>
           <vnodes>
           <v t="id1"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id2"><vh>heading</vh></v>
           </vnodes>
           </leo_file>
         )
    }
    it("singletons with different headings") {
      test(false,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading1</vh></v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading2</vh></v>
           </vnodes>
           </leo_file>
         )
    }
    it("singletons with different bodies") {
      test(false,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           <tnodes>
           <t tx="id">body1</t>
           </tnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           <tnodes>
           <t tx="id">body2</t>
           </tnodes>
           </leo_file>
         )
    }
    it("identical nodes with identical children") {
      test(true,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh></v>
           </v>
           </vnodes>
           </leo_file>
         )
    }
    it("identical nodes with different children") {
      test(false,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading2</vh></v>
           <v t="id2"><vh>heading1</vh></v>
           </v>
           </vnodes>
           </leo_file>
         )
    }
    it("identical nodes with identical clone children") {
      test(true,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id1"/>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id1"/>
           </v>
           </vnodes>
           </leo_file>
         )
    }
    it("identical nodes with different clone children") {
      test(false,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id1"/>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading2</vh></v>
           <v t="id1"/>
           </v>
           </vnodes>
           </leo_file>
         )
    }
    it("identical cyclic nodes") {
      test(true,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id"/>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id"/>
           </v>
           </vnodes>
           </leo_file>
         )
    }
    it("identical multiply nested nodes") {
      test(true,
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
           </leo_file>,
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
           </leo_file>
         )
    }
    it("different multiply nested nodes") {
      test(false,
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
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id1"><vh>heading1</vh>
           <v t="id2a"/>
           </v>
           <v t="id2"><vh>heading2</vh>
           <v t="id2a"><vh>heading2a</vh>
           <v t="id2a1"><vh>heading2a2</vh></v>
           <v t="id2a2"><vh>heading2a2</vh></v>
           </v>
           <v t="id2b"><vh>heading2b</vh>
           <v t="id1"/>
           </v>
           </v>
           </vnodes>
           </leo_file>
         )
    }
  }
}
//@+node:gcross.20110414153139.1494: ** NodeVisitorSpecification
class NodeVisitorSpecification extends Spec with ShouldMatchers {
  describe("The visitor should work for") {
    def test(xml: scala.xml.Node, correct_result: Array[String]) {
      import scala.collection.mutable.ArrayBuilder
      import XMLParser._
      val ParseResult(tree,_) = parse(xml)
      val builder = ArrayBuilder.make[String]
      tree.root.accept(new NodeVisitor {
        def visit(node: Node) = {
          builder += node.id
          true
        }
      })
      builder.result should be (correct_result)
    }
    it("singleton") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           Array("id")
         )
    }
    it("node with children") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           Array("id","id1","id2")
         )
    }
    it("node with grandchildren") {
      test(<leo_file>
           <vnodes>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh>
           <v t="id2a"><vh>heading2a</vh>
           <v t="id2a1"><vh>heading2a1</vh></v>
           <v t="id2a2"><vh>heading2a2</vh></v>
           </v>
           <v t="id2b"><vh>heading2b</vh>
           <v t="id2b1"><vh>heading2b1</vh></v>
           <v t="id2b2"><vh>heading2b2</vh></v>
           </v>
           </v>
           </vnodes>
           </leo_file>,
           Array("id1","id2","id2a","id2a1","id2a2","id2b","id2b1","id2b2")
         )
    }
    it("node with cloned grandchildren") {
      test(<leo_file>
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
           </leo_file>,
           Array("id1","id2a","id2a1","id2a2","id2","id2a","id2a1","id2a2","id2b","id1","id2a","id2a1","id2a2")
         )
    }
  }
}
//@+node:gcross.20110414153139.1495: ** NodeVisitorWithMemorySpecification
class NodeVisitorWithMemorySpecification extends Spec with ShouldMatchers {
  describe("The visitor should work for") {
    def test(xml: scala.xml.Node, correct_result: Array[String]) {
      import scala.collection.mutable.ArrayBuilder
      import XMLParser._
      val ParseResult(tree,_) = parse(xml)
      val builder = ArrayBuilder.make[String]
      tree.root.accept(new NodeVisitorWithMemory {
        def visit(node: Node, seen: Boolean) = {
          builder += node.id
          !seen
        }
      })
      builder.result should be (correct_result)
    }
    it("singleton") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           Array("id")
         )
    }
    it("node with children") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           Array("id","id1","id2")
         )
    }
    it("node with grandchildren") {
      test(<leo_file>
           <vnodes>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh>
           <v t="id2a"><vh>heading2a</vh>
           <v t="id2a1"><vh>heading2a1</vh></v>
           <v t="id2a2"><vh>heading2a2</vh></v>
           </v>
           <v t="id2b"><vh>heading2b</vh>
           <v t="id2b1"><vh>heading2b1</vh></v>
           <v t="id2b2"><vh>heading2b2</vh></v>
           </v>
           </v>
           </vnodes>
           </leo_file>,
           Array("id1","id2","id2a","id2a1","id2a2","id2b","id2b1","id2b2")
         )
    }
    it("node with cloned grandchildren") {
      test(<leo_file>
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
           </leo_file>,
           Array("id1","id2a","id2a1","id2a2","id2","id2a","id2b","id1")
         )
    }
  }
}
//@+node:gcross.20110414153139.1493: ** TreeSpecification
class TreeSpecification extends Spec with ShouldMatchers {
  describe("The merger should work for") {
    def test(n1: scala.xml.Node, n2: scala.xml.Node, n3: scala.xml.Node) {
      import scala.collection.mutable.HashMap
      import XMLParser._
      val ParseResult(tree1,_) = parse(n1)
      val ParseResult(tree2,_) = parse(n2)
      val ParseResult(tree3,_) = parse(n3)
      val substitute = tree2.root.children(0)
      tree1.mergeAndReplaceStub(tree1.lookupNode(substitute.id).get,substitute)
      (tree1.root.children(0) === tree3.root.children(0)) should be (true)
      def checkValidity(node: Node, seen: HashMap[String,Node] = new HashMap[String,Node]) {
        seen.get(node.id) match {
          case None =>
            seen(node.id) = node
          case Some(other_node) =>
            other_node should be (node)
        }
        for(child <- node.children) checkValidity(child)
      }
      checkValidity(tree1.root.children(0))
    }
    it("an empty node.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>
        )
    }
    it("a node with some children.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id2"><vh>heading2</vh></v>
           </v>
           </vnodes>
           </leo_file>
        )
    }
    it("a cloned node.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id1"/>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           <v t="id1"/>
           </v>
           </vnodes>
           </leo_file>
        )
    }
    it("a cloned node outside the substituted node.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id1a"/>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           <v t="id1a"/>
           </v>
           </vnodes>
           </leo_file>
        )
    }
    it("an identical cloned node outside the substituted node.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           <v t="id1a"/>
           </v>
           </vnodes>
           </leo_file>
        )
    }
    it("a conflicting cloned node outside the substituted node.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh></v>
           <v t="id1a"><vh>heading1b</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a"><vh>heading1a</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh>
           <v t="id1"><vh>heading1</vh>
           <v t="id1a.0"><vh>heading1a</vh></v>
           </v>
           <v t="id1a"><vh>heading1b</vh></v>
           </v>
           </vnodes>
           </leo_file>
        )
    }
  }
  describe("The parse result merger should work for") {
    def test(n1: scala.xml.Node, n2: Either[Exception,scala.xml.Node], n3: scala.xml.Node) {
      import scala.collection.mutable.HashMap
      import XMLParser._
      val ParseResult(tree1,_) = parse(n1)
      val result: Parser.ParseResult =
        n2 match {
          case Left(e) => Left(e)
          case Right(node) => {
            val ParseResult(tree2,_) = parse(node)
            Right(tree2.root.children(0))
          }
        }
      val ParseResult(tree3,_) = parse(n3)
      tree1.mergeParseResultWithStub(tree1.lookupNode("id").get,result)
      tree1.root.children(0).toYAML should be (tree3.root.children(0).toYAML)
    }
    it("a successful parse.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           Right(
             <leo_file>
             <vnodes>
             <v t="id"><vh>heading</vh></v>
             </vnodes>
             </leo_file>
           ),
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>
        )
    }
    it("a failed parse.") {
      test(
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           <tnodes>
           <t tx="id">old body</t>
           </tnodes>
           </leo_file>,
           Left(new Exception("Hello, world!")),
           <leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           <tnodes>
           <t tx="id">{
           """|@ignore
              |@
              |Error parsing file:
              |
              |java.lang.Exception: Hello, world!
              |@c
              |old body""".stripMargin('|')
           }</t>
           </tnodes>
           </leo_file>
        )
    }
//@@end_raw
  }
  describe("The file node finder should work for") {
    import java.io.File
    import scala.collection.{Map,Set}
    def test(xml: scala.xml.Node, correct_files_associated_with_node: Map[String,Set[File]], correct_nodes_associated_with_file: Map[File,Set[String]]) {
      import XMLParser._
      val ParseResult(tree,_) = parse(xml)
      val (observed_files_associated_with_node,observed_nodes_associated_with_file) = tree.findFileNodes(new File("."))
      observed_files_associated_with_node.map({case (node,files) => (node.id,files)}) should be (correct_files_associated_with_node)
      observed_nodes_associated_with_file.map({case (file,nodes) => (file,nodes.map(_.id))}) should be (correct_nodes_associated_with_file)
    }
    it("a single non-file node") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>heading</vh></v>
           </vnodes>
           </leo_file>,
           Map(),
           Map()
          )
    }
    it("a single file node") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>@file foo.bar</vh></v>
           </vnodes>
           </leo_file>,
           Map("id" -> Set(new File("./foo.bar"))),
           Map(new File("./foo.bar") -> Set("id"))
          )
    }
    it("a single thin node") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh> @thin  foo.bar </vh></v>
           </vnodes>
           </leo_file>,
           Map("id" -> Set(new File("./foo.bar"))),
           Map(new File("./foo.bar") -> Set("id"))
          )
    }
    it("an ignored file node") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>@file foo.bar</vh></v>
           </vnodes>
           <tnodes>
           <t tx="id">@ignore</t>
           </tnodes>
           </leo_file>,
           Map(),
           Map()
          )
    }
    it("an ignored root") {
      test(<leo_file>
           <vnodes>
           <v t="root"><vh>@ignore</vh>
           <v t="id"><vh>@file foo.bar</vh></v>
           </v>
           </vnodes>
           </leo_file>,
           Map(),
           Map()
          )
    }
    it("a cloned file node") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>@file foo.bar</vh></v>
           <v t="id"/>
           </vnodes>
           </leo_file>,
           Map("id" -> Set(new File("./foo.bar"))),
           Map(new File("./foo.bar") -> Set("id"))
          )
    }
    it("an aliased file node") {
      test(<leo_file>
           <vnodes>
           <v t="id1"><vh>@file foo.bar</vh></v>
           <v t="id2"><vh>@file foo.bar</vh></v>
           </vnodes>
           </leo_file>,
           Map("id1" -> Set(new File("./foo.bar")),
               "id2" -> Set(new File("./foo.bar"))
              ),
           Map(new File("./foo.bar") -> Set("id1","id2"))
          )
    }
    it("a cloned ignored file node") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>@file foo.bar</vh></v>
           <v t="id"/>
           </vnodes>
           <tnodes>
           <t tx="id">@ignore</t>
           </tnodes>
           </leo_file>,
           Map(),
           Map()
          )
    }
    it("a cloned file node in another directory") {
      test(<leo_file>
           <vnodes>
           <v t="id"><vh>@file foo.bar</vh></v>
           <v t="id1"><vh>@path dir</vh>
           <v t="id"/>
           </v>
           </vnodes>
           </leo_file>,
           Map("id" -> Set(new File("./foo.bar"),new File("./dir/foo.bar"))),
           Map(new File("./foo.bar") -> Set("id"),new File("./dir/foo.bar") -> Set("id"))
          )
    }
    it("a single file node with children") {
      test(<leo_file>
           <vnodes>
           <v t="idA"><vh>not a file A</vh></v>
           <v t="id"><vh>@file foo.bar</vh>
           <v t="id1"><vh>child 1</vh>
           <v t="id1a"><vh>child 1a</vh></v>
           <v t="id1b"><vh>child 1b</vh></v>
           </v>
           <v t="id2"><vh>child 2</vh></v>
           </v>
           <v t="idB"><vh>not a file B</vh></v>
           </vnodes>
           </leo_file>,
           Map("id" -> Set(new File("./foo.bar"))),
           Map(new File("./foo.bar") -> Set("id"))
          )
    }
    it("file nodes nested under paths") {
      test(<leo_file>
           <vnodes>
           <v t="id1"><vh>@file foo1.bar</vh></v>
           <v t="id2"><vh>@path dir</vh>
           <v t="id2a"><vh>@file foo2a.bar</vh>
           <v t="id2a1"><vh>blarg</vh></v>
           </v>
           <v t="id2b"><vh>@file foo2b.bar</vh></v>
           </v>
           <v t="id3"><vh>@file foo3.bar</vh></v>
           </vnodes>
           </leo_file>,
           Map("id1" -> Set(new File("./foo1.bar")),
               "id2a" -> Set(new File("./dir/foo2a.bar")),
               "id2b" -> Set(new File("./dir/foo2b.bar")),
               "id3" -> Set(new File("./foo3.bar"))
              ),
           Map(new File("./foo1.bar") -> Set("id1"),
               new File("./dir/foo2a.bar") -> Set("id2a"),
               new File("./dir/foo2b.bar") -> Set("id2b"),
               new File("./foo3.bar") -> Set("id3")
              )
          )
    }
  }
}
//@-others
//@-leo
