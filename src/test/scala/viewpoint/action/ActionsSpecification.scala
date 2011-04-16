//@+leo-ver=5-thin
//@+node:gcross.20110414153139.2614: * @file ActionsSpecification.scala
//@@language Scala
package viewpoint.action.testing

//@+<< Imports >>
//@+node:gcross.20110414153139.2616: ** << Imports >>
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.action._
import viewpoint.model._
//@-<< Imports >>

//@+others
//@+node:gcross.20110414153139.2617: ** ModelActionsSpecification
abstract class ModelActionsSpecification(createEmptyTree: => Tree) extends Spec with ShouldMatchers {
  //@+others
  //@+node:gcross.20110414153139.2619: *3* ChangeNodeBody
  describe("ChangeNodeBody") {
    //@+others
    //@+node:gcross.20110414153139.2620: *4* should perform the correct action.
    it("should perform the correct action.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,null)
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
      val node = tree.createNode(null,null)
      ChangeNodeHeading(node,"heading").actOn(tree)
      node.getHeading should be ("heading")
    }
    //@+node:gcross.20110414153139.5167: *4* should return the correct undo action.
    it("should return the correct undo action.") {
      val tree = createEmptyTree
      val node = tree.createNode("heading",null)
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
      val child1 = tree.createNode(null,null)
      val child2 = tree.createNode(null,null)
      tree.insertChildInto(root,child1,0)
      tree.insertChildInto(root,child2,1)
      MoveChildDownOne(root,child1,0).actOn(tree)
      root.getChildCount should be (2)
      root.getChild(0) should be (child2)
      root.getChild(1) should be (child1)
    }
    //@+node:gcross.20110417144805.1567: *4* should return the correct undo action.
    it("should return the correct undo action.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val child1 = tree.createNode(null,null)
      val child2 = tree.createNode(null,null)
      tree.insertChildInto(root,child1,0)
      tree.insertChildInto(root,child2,1)
      MoveChildDownOne(root,child1,0).actOn(tree) should be (MoveChildUpOne(root,child1,1))
    }
    //@+node:gcross.20110417144805.1568: *4* should throw the correct exception upon child change.
    it("should throw the correct exception upon child change.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val child1 = tree.createNode(null,null)
      val child2 = tree.createNode(null,null)
      tree.insertChildInto(root,child1,0)
      tree.insertChildInto(root,child2,1)
      val action = MoveChildDownOne(root,child2,0)
      val thrown_exception = try { action.actOn(tree) } catch { case (e : Exception) => e }
      val expected_exception = UnexpectedChildAtIndexWhenExecutingAction(root,child2,0,action)
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
      val child1 = tree.createNode(null,null)
      val child2 = tree.createNode(null,null)
      tree.insertChildInto(root,child1,0)
      tree.insertChildInto(root,child2,1)
      MoveChildUpOne(root,child2,1).actOn(tree)
      root.getChildCount should be (2)
      root.getChild(0) should be (child2)
      root.getChild(1) should be (child1)
    }
    //@+node:gcross.20110417144805.1575: *4* should return the correct undo action.
    it("should return the correct undo action.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val child1 = tree.createNode(null,null)
      val child2 = tree.createNode(null,null)
      tree.insertChildInto(root,child1,0)
      tree.insertChildInto(root,child2,1)
      MoveChildUpOne(root,child2,1).actOn(tree) should be (MoveChildDownOne(root,child2,0))
    }
    //@+node:gcross.20110417144805.1576: *4* should throw the correct exception upon child change.
    it("should throw the correct exception upon child change.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val child1 = tree.createNode(null,null)
      val child2 = tree.createNode(null,null)
      tree.insertChildInto(root,child1,0)
      tree.insertChildInto(root,child2,1)
      val action = MoveChildUpOne(root,child1,1)
      val thrown_exception = try { action.actOn(tree) } catch { case (e : Exception) => e }
      val expected_exception = UnexpectedChildAtIndexWhenExecutingAction(root,child1,1,action)
      thrown_exception should be (expected_exception)
    }
    //@-others
  }
  //@-others
}
//@-others
//@-leo
