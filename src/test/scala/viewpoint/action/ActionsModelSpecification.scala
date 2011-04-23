//@+leo-ver=5-thin
//@+node:gcross.20110417144805.2774: * @file ActionsModelSpecification.scala
//@@language Scala
package viewpoint.action.testing

//@+<< Imports >>
//@+node:gcross.20110414153139.2616: ** << Imports >>
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.action._
import viewpoint.model._
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110414153139.2617: ** ActionsModelSpecification
abstract class ActionsModelSpecification(createEmptyTree: => Tree) extends Spec with ShouldMatchers {
  //@+others
  //@+node:gcross.20110414153139.2619: *3* ChangeNodeBody
  describe("ChangeNodeBody") {
    //@+others
    //@+node:gcross.20110414153139.2620: *4* should perform the correct action.
    it("should perform the correct action.") {
      val tree = createEmptyTree
      val node = tree.createNode()
      ChangeNodeBody(node,"body").actOn(tree)
      node.getBody should be ("body")
    }
    //@+node:gcross.20110414153139.2622: *4* should return the correct undo action.
    it("should return the correct undo action.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,"body")
      ChangeNodeBody(node,"soul").actOn(tree) should be (ChangeNodeBody(node,"body"))
    }
    //@-others
  }
  //@+node:gcross.20110414153139.5165: *3* ChangeNodeHeading
  describe("ChangeNodeHeading") {
    //@+others
    //@+node:gcross.20110414153139.5166: *4* should perform the correct action.
    it("should perform the correct action.") {
      val tree = createEmptyTree
      val node = tree.createNode()
      ChangeNodeHeading(node,"heading").actOn(tree)
      node.getHeading should be ("heading")
    }
    //@+node:gcross.20110414153139.5167: *4* should return the correct undo action.
    it("should return the correct undo action.") {
      val tree = createEmptyTree
      val node = tree.createNode("heading")
      ChangeNodeHeading(node,"footing").actOn(tree) should be (ChangeNodeHeading(node,"heading"))
    }
    //@-others
  }
  //@+node:gcross.20110417144805.1565: *3* MoveChildDownOne
  describe("MoveChildDownOne") {
    //@+others
    //@+node:gcross.20110417144805.1566: *4* should perform the correct action.
    it("should perform the correct action.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val tags = tree.appendChildrenTo(root,node1,node2)
      MoveChildDownOne(root,tags(0)).actOn(tree)
      root.getChildCount should be (2)
      root.getChild(0).getNode should be (node2)
      root.getChild(1).getNode should be (node1)
    }
    //@+node:gcross.20110417144805.1567: *4* should return the correct undo action.
    it("should return the correct undo action.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val tags = tree.appendChildrenTo(root,node1,node2)
      MoveChildDownOne(root,tags(0)).actOn(tree) should be (MoveChildUpOne(root,tags(0)))
    }
    //@+node:gcross.20110417144805.1568: *4* should throw the correct exception upon child change.
    it("should throw the correct exception upon child change.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val tags = tree.appendChildrenTo(root,node1,node2)
      var tag = 10
      while(tags.contains(tag)) tag += 1
      val action = MoveChildDownOne(root,tag)
      val thrown_exception = try { action.actOn(tree) } catch { case (e : Exception) => e }
      val expected_exception = TargetDisappearedBeforeActionCommenced(action)
      thrown_exception should be (expected_exception)
    }
    //@-others
  }
  //@+node:gcross.20110417144805.1573: *3* MoveChildUpOne
  describe("MoveChildUpOne") {
    //@+others
    //@+node:gcross.20110417144805.1574: *4* should perform the correct action.
    it("should perform the correct action.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val tags = tree.appendChildrenTo(root,node1,node2)
      MoveChildUpOne(root,tags(1)).actOn(tree)
      root.getChildCount should be (2)
      root.getChild(0).getNode should be (node2)
      root.getChild(1).getNode should be (node1)
    }
    //@+node:gcross.20110417144805.1575: *4* should return the correct undo action.
    it("should return the correct undo action.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val tags = tree.appendChildrenTo(root,node1,node2)
      MoveChildUpOne(root,tags(1)).actOn(tree) should be (MoveChildDownOne(root,tags(1)))
    }
    //@+node:gcross.20110417144805.1576: *4* should throw the correct exception upon child change.
    it("should throw the correct exception upon child change.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val tags = tree.appendChildrenTo(root,node1,node2)
      var tag = 10
      while(tags.contains(tag)) tag += 1
      val action = MoveChildUpOne(root,tag)
      val thrown_exception = try { action.actOn(tree) } catch { case (e : Exception) => e }
      val expected_exception = TargetDisappearedBeforeActionCommenced(action)
      thrown_exception should be (expected_exception)
    }
    //@-others
  }
  //@-others
}
//@-others
//@-leo
