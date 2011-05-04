//@+leo-ver=5-thin
//@+node:gcross.20110422115402.5179: * @file TransactionModelSpecification.scala
//@@language Scala
package viewpoint.util.testing

//@+<< Imports >>
//@+node:gcross.20110422115402.5180: ** << Imports >>
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.event._
import viewpoint.model._
import viewpoint.util._
import viewpoint.util.RichInterface._
import viewpoint.util.Transaction.wrapInTransaction
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.5181: ** TransactionModelSpecification
abstract class TransactionModelSpecification(createEmptyTree: => Tree) extends Spec with ShouldMatchers {
  //@+others
  //@+node:gcross.20110425115406.1716: *3* The unwind method correctly unwinds
  describe("The unwind method correctly unwinds") {
    //@+others
    //@+node:gcross.20110425115406.1721: *4* setBodyOf.
    it("setBodyOf.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,"body")

      val listener = new TreeChangeEventRecorder
      val events = listener.events
      tree.addTreeChangeListener(listener)

      val logger = new LoggedMutator(tree)
      logger.setBodyOf(node,"soul")
      logger.unwind()

      node.getBody should be ("body")

      events.size should be (2)
      events(1).getClass should be (classOf[NodeBodyChangedEvent])
      val event = events(1).asInstanceOf[NodeBodyChangedEvent]
      event.getTree should be (tree)
      event.getNode should be (node)
      event.getBody should be (node.getBody)
    }
    //@+node:gcross.20110425120511.1724: *4* setHeadingOf.
    it("setHeadingOf.") {
      val tree = createEmptyTree
      val node = tree.createNode("heading",null)

      val listener = new TreeChangeEventRecorder
      val events = listener.events
      tree.addTreeChangeListener(listener)

      val logger = new LoggedMutator(tree)
      logger.setHeadingOf(node,"footing")
      logger.unwind()

      node.getHeading should be ("heading")

      events.size should be (2)
      events(1).getClass should be (classOf[NodeHeadingChangedEvent])
      val event = events(1).asInstanceOf[NodeHeadingChangedEvent]
      event.getTree should be (tree)
      event.getNode should be (node)
      event.getHeading should be (node.getHeading)
    }
    //@+node:gcross.20110425120511.1726: *4* insertChildInto.
    it("insertChildInto.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()

      val listener = new TreeChangeEventRecorder
      val events = listener.events
      tree.addTreeChangeListener(listener)

      val logger = new LoggedMutator(tree)
      val tag = logger.insertChildInto(root,node,0)
      logger.unwind()

      root.getChildCount should be (0)

      events.size should be (2)
      events(1).getClass should be (classOf[ChildRemovedEvent])
      val event = events(1).asInstanceOf[ChildRemovedEvent]
      event.getTree should be (tree)
      event.getChildIndex should be (0)
      event.getChildNode should be (node)
      event.getChildTag should be (tag)
    }
    //@+node:gcross.20110425120511.1728: *4* removeChildFrom.
    it("removeChildFrom.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()
      val tag = tree.appendChildTo(root,node)

      val listener = new TreeChangeEventRecorder
      val events = listener.events
      tree.addTreeChangeListener(listener)

      val logger = new LoggedMutator(tree)
      logger.removeChildFrom(root,0)
      logger.unwind()

      root.getChildCount should be (1)
      val child = root.getChild(0)
      child.getNode should be (node)
      child.getTag should be (tag)

      events.size should be (2)
      events(1).getClass should be (classOf[ChildInsertedEvent])
      val event = events(1).asInstanceOf[ChildInsertedEvent]
      event.getTree should be (tree)
      event.getChild should be (child)
      event.getChildIndex should be (0)
    }
    //@-others
  }
  //@+node:gcross.20110425121514.1728: *3* The wrapInTransaction method
  describe("The wrapInTransaction method") {
    //@+others
    //@+node:gcross.20110425121514.1729: *4* returns the value returned by the callback.
    it("returns the value returned by the callback.") {
      wrapInTransaction(null,{(mutator) => 42}).result should be (42)
    }
    //@+node:gcross.20110425121514.1730: *4* throws the value thrown by the callback.
    it("throws the value thrown by the callback.") {
      val correct_exception = new Exception
      val thrown_exception: Exception =
        try {
          wrapInTransaction(null,{(mutator) => throw correct_exception})
          fail("Exception was not thrown.")
        } catch {
          case (e: Exception) => e
        }
      assert(thrown_exception eq correct_exception)
    }
    //@+node:gcross.20110425121514.1731: *4* multi-step transactions are undone.
    it("multi-step transactions are completely undone.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val node3 = tree.createNode()
      val node4 = tree.createNode()
      tree.appendChildrenTo(root,node1,node3)

      val E = new Exception
      try {
        wrapInTransaction(tree,{ (mutator) =>
          mutator.insertChildInto(root,node2,1)
          mutator.removeChildFrom(root,0)
          mutator.insertChildInto(root,node1,1)
          mutator.removeChildFrom(root,1)
          mutator.insertChildInto(root,node4,2)
          throw E
        })
      } catch {
        case E =>
      }

      root.getChildCount should be (2)
      root.getChild(0).getNode should be (node1)
      root.getChild(1).getNode should be (node3)
    }
    //@-others
  }
  //@-others
}
//@-others
//@-leo
