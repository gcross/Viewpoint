//@+leo-ver=5-thin
//@+node:gcross.20110422115402.2030: * @file RichInterfaceModelSpecification.scala
//@@language Scala
package viewpoint.util.testing

//@+<< Imports >>
//@+node:gcross.20110422115402.2031: ** << Imports >>
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.model._
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.2032: ** RichInterfaceModelSpecification
abstract class RichInterfaceModelSpecification(createEmptyTree: => Tree) extends Spec with ShouldMatchers {
  //@+others
  //@+node:gcross.20110422202916.1655: *3* The === method gets the correct answer for
  describe("The === method gets the correct answer for") {
    //@+others
    //@+node:gcross.20110422202916.1656: *4* identical singletons.
    it("identical singletons.") {
      val node1 = createEmptyTree.createNode("id","heading","body")
      val node2 = createEmptyTree.createNode("id","heading","body")
      (node1 === node2) should be (true)
    }
    //@+node:gcross.20110422204808.1656: *4* singletons with different ids.
    it("singletons with different ids.") {
      val node1 = createEmptyTree.createNode("id1","heading","body")
      val node2 = createEmptyTree.createNode("id2","heading","body")
      (node1 === node2) should be (false)
    }
    //@+node:gcross.20110422204808.1658: *4* singletons with different headings.
    it("singletons with different headings.") {
      val node1 = createEmptyTree.createNode("id","heading1","body")
      val node2 = createEmptyTree.createNode("id","heading2","body")
      (node1 === node2) should be (false)
    }
    //@+node:gcross.20110422204808.1660: *4* singletons with different bodies.
    it("singletons with different bodies.") {
      val node1 = createEmptyTree.createNode("id","heading","body1")
      val node2 = createEmptyTree.createNode("id","heading","body2")
      (node1 === node2) should be (false)
    }
    //@+node:gcross.20110422204808.1661: *4* identical nodes with identical children.
    it("identical nodes with identical children.") {
      val tree1 = createEmptyTree
      val node1 = tree1.createNode("id","heading","body")
      tree1.appendChildrenTo(node1,
        tree1.createNode("ida","heading","body"),
        tree1.createNode("idb","heading","body")
      )
      val tree2 = createEmptyTree
      val node2 = tree2.createNode("id","heading","body")
      tree2.appendChildrenTo(node2,
        tree2.createNode("ida","heading","body"),
        tree2.createNode("idb","heading","body")
      )
      (node1 === node2) should be (true)
    }
    //@+node:gcross.20110422204808.1663: *4* identical nodes with different children.
    it("identical nodes with different children.") {
      val tree1 = createEmptyTree
      val node1 = tree1.createNode("id","heading","body")
      tree1.appendChildrenTo(node1,
        tree1.createNode("ida","heading1","body"),
        tree1.createNode("idb","heading2","body")
      )
      val tree2 = createEmptyTree
      val node2 = tree2.createNode("id","heading","body")
      tree2.appendChildrenTo(node2,
        tree2.createNode("ida","heading2","body"),
        tree2.createNode("idb","heading1","body")
      )
      (node1 === node2) should be (false)
    }
    //@+node:gcross.20110422204808.1665: *4* identical nodes with identical clone children.
    it("identical nodes with identical clone children.") {
      val tree1 = createEmptyTree
      val node1 = tree1.createNode("id","heading","body")
      val node1a = tree1.createNode("ida","heading","body")
      tree1.appendChildrenTo(node1,node1a,node1a)
      val tree2 = createEmptyTree
      val node2 = tree2.createNode("id","heading","body")
      val node2a = tree2.createNode("ida","heading","body")
      tree2.appendChildrenTo(node2,node2a,node2a)
      (node1 === node2) should be (true)
    }
    //@+node:gcross.20110422204808.1668: *4* identical nodes with different clone children.
    it("identical nodes with different clone children.") {
      val tree1 = createEmptyTree
      val node1 = tree1.createNode("id","heading","body")
      val node1a = tree1.createNode("ida","heading1","body")
      tree1.appendChildrenTo(node1,node1a,node1a)
      val tree2 = createEmptyTree
      val node2 = tree2.createNode("id","heading","body")
      val node2a = tree2.createNode("ida","heading2","body")
      tree2.appendChildrenTo(node2,node2a,node2a)
      (node1 === node2) should be (false)
    }
    //@+node:gcross.20110422204808.1670: *4* identical cyclic nodes.
    it("identical cyclic nodes.") {
      val tree1 = createEmptyTree
      val node1 = tree1.createNode("id","heading","body")
      tree1.appendChildrenTo(node1,
        tree1.createNode("ida","heading","body"),
        node1
      )
      val tree2 = createEmptyTree
      val node2 = tree2.createNode("id","heading","body")
      tree2.appendChildrenTo(node2,
        tree2.createNode("ida","heading","body"),
        node2
      )
      (node1 === node2) should be (true)
    }
    //@+node:gcross.20110422204808.1672: *4* identical multiply nested nodes.
    it("identical multiply nested nodes.") {
      val tree1 = createEmptyTree
      val node1 = tree1.createNode("id","heading","body")
      val node1a = tree1.createNode("ida","heading","a")
      val node1b = tree1.createNode("idb","heading","b")
      val node1b1 = tree1.createNode("idb1","heading","b1")
      val node1b1a = tree1.createNode("idb1a","heading","b1a")
      val node1b1b = tree1.createNode("idb1b","heading","b1b")
      val node1b2 = tree1.createNode("idb2","heading","b2")
      tree1.appendChildrenTo(node1,node1a,node1b)
      tree1.appendChildrenTo(node1a,node1b1)
      tree1.appendChildrenTo(node1b,node1b1,node1b2,node1a)
      tree1.appendChildrenTo(node1b1,node1b1a,node1b1b)

      val tree2 = createEmptyTree
      val node2 = tree2.createNode("id","heading","body")
      val node2a = tree2.createNode("ida","heading","a")
      val node2b = tree2.createNode("idb","heading","b")
      val node2b1 = tree2.createNode("idb1","heading","b1")
      val node2b1a = tree2.createNode("idb1a","heading","b1a")
      val node2b1b = tree2.createNode("idb1b","heading","b1b")
      val node2b2 = tree2.createNode("idb2","heading","b2")
      tree2.appendChildrenTo(node2,node2a,node2b)
      tree2.appendChildrenTo(node2a,node2b1)
      tree2.appendChildrenTo(node2b,node2b1,node2b2,node2a)
      tree2.appendChildrenTo(node2b1,node2b1a,node2b1b)

      (node1 === node2) should be (true)
    }
    //@+node:gcross.20110422204808.1674: *4* different multiply nested nodes.
    it("different multiply nested nodes.") {
      val tree1 = createEmptyTree
      val node1 = tree1.createNode("id","heading","body")
      val node1a = tree1.createNode("ida","heading","a")
      val node1b = tree1.createNode("idb","heading","b")
      val node1b1 = tree1.createNode("idb1","heading","b1")
      val node1b1a = tree1.createNode("idb1a","heading","b1a")
      val node1b1b = tree1.createNode("idb1b","heading","b1b")
      val node1b2 = tree1.createNode("idb2","heading","b2")
      tree1.appendChildrenTo(node1,node1a,node1b)
      tree1.appendChildrenTo(node1a,node1b1)
      tree1.appendChildrenTo(node1b,node1b1,node1b2,node1a)
      tree1.appendChildrenTo(node1b1,node1b1a,node1b1b)

      val tree2 = createEmptyTree
      val node2 = tree2.createNode("id","heading","body")
      val node2a = tree2.createNode("ida","heading","a")
      val node2b = tree2.createNode("idb","heading","b")
      val node2b1 = tree2.createNode("idb1","heading","b1")
      val node2b1a = tree2.createNode("idb1a","heading","b1b")
      val node2b1b = tree2.createNode("idb1b","heading","b1b")
      val node2b2 = tree2.createNode("idb2","heading","b2")
      tree2.appendChildrenTo(node2,node2a,node2b)
      tree2.appendChildrenTo(node2a,node2b1)
      tree2.appendChildrenTo(node2b,node2b1,node2b2,node2a)
      tree2.appendChildrenTo(node2b1,node2b1a,node2b1b)

      (node1 === node2) should be (false)
    }
    //@-others
  }
  //@+node:gcross.20110422115402.2822: *3* The getAllNodesInSubtree method iteratres or
  describe("The getChildren method iterates correctly over all nodes in the subtree.") {
    val tree = createEmptyTree
    val node = tree.createNode("id","heading","body")
    val nodea = tree.createNode("ida","heading","a")
    val nodeb = tree.createNode("idb","heading","b")
    val nodeb1 = tree.createNode("idb1","heading","b1")
    val nodeb1a = tree.createNode("idb1a","heading","b1a")
    val nodeb1b = tree.createNode("idb1b","heading","b1b")
    val nodeb2 = tree.createNode("idb2","heading","b2")
    tree.appendChildrenTo(node,nodea,nodeb)
    tree.appendChildrenTo(nodea,nodeb1)
    tree.appendChildrenTo(nodeb,nodeb1,nodeb2,nodea)
    tree.appendChildrenTo(nodeb1,nodeb1a,nodeb1b)
    node.getAllNodesInSubtree.map(_.getId).toSet should be (Set("id","ida","idb","idb1","idb1","idb1a","idb1b","idb2"))
  }
  //@+node:gcross.20110422115402.2033: *3* The getChildren method iterates correctly over children.
  describe("The getChildren method iterates correctly over children.") {
    val tree = createEmptyTree
    val root = tree.getRoot
    tree.appendChildTo(root,tree.createNode())
    tree.appendChildTo(root,tree.createNode())
    tree.appendChildTo(root,tree.createNode())
    val children = root.getChildren.toSeq
    for(index <- 0 until 3) children(index) should be (root.getChild(index))
  }
  //@-others
}
//@-others
//@-leo
