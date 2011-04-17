//@+leo-ver=5-thin
//@+node:gcross.20110414153139.1472: * @file ModelSpecification.scala
//@@language Scala
package viewpoint.model.testing

//@+<< Imports >>
//@+node:gcross.20110414153139.1473: ** << Imports >>
import scala.collection.JavaConversions._
import scala.collection.Seq
import scala.collection.mutable.{ArrayBuffer}
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.event._
import viewpoint.model._
//@-<< Imports >>

//@+others
//@+node:gcross.20110414153139.1474: ** ModelSpecification
abstract class ModelSpecification(createEmptyTree: => Tree) extends Spec with ShouldMatchers {
  //@+<< Helpers >>
  //@+node:gcross.20110414153139.1549: *3* << Helpers >>
  //@+others
  //@+node:gcross.20110414153139.2064: *4* class EventRecorderTreeChangeListener
  class EventRecorderTreeChangeListener extends TreeChangeListener {
    val events = new ArrayBuffer[TreeChangeEvent]
    def treeNodeBodyChanged(event: NodeBodyChangedEvent) { events += event }
    def treeNodeChildInserted(event: ChildInsertedEvent) { events += event }
    def treeNodeChildRemoved(event: ChildRemovedEvent) { events += event }
    def treeNodeHeadingChanged(event: NodeHeadingChangedEvent){ events += event }
    def treeNodeStructureChanged(event: StructureChangedEvent) { events += event }
  }
  //@+node:gcross.20110414153139.1550: *4* recordEventsDuring
  def recordEventsDuring(tree: Tree)(block: () => Unit): Seq[TreeChangeEvent] = {
    val listener = new EventRecorderTreeChangeListener
    tree.addTreeChangeListener(listener)
    block ()
    tree.removeTreeChangeListener(listener)
    listener.events
  }
  //@-others
  //@-<< Helpers >>
  //@+others
  //@+node:gcross.20110414153139.1496: *3* An empty tree has no children.
  describe("An empty tree has no children.") {
    createEmptyTree.getRoot.getChildCount should be (0)
  }
  //@+node:gcross.20110414153139.1503: *3* lookupNode returns the correct node.
  describe("lookupNode returns the correct node.") {
    val tree = createEmptyTree
    val node1 = tree.createNode(null,null)
    val node2 = tree.lookupNode(node1.getId)
    node1 should be (node2)
  }
  //@+node:gcross.20110414153139.1557: *3* removeTreeChangeListener removes the listener.
  describe("removeTreeChangeListener removes the listener.") {
    val listener = new TreeChangeAdapter {
      override def treeNodeBodyChanged(event: NodeBodyChangedEvent) {
        fail("Listener was not removed.")
      }
    }
    val tree = createEmptyTree
    val node = tree.createNode(null,null)
    tree.addTreeChangeListener(listener)
    tree.removeTreeChangeListener(listener)
    tree.setBodyOf(node,"body")
  }
  //@+node:gcross.20110414153139.1506: *3* The equality comparison
  describe("The equals operation") {
    //@+others
    //@+node:gcross.20110414153139.1514: *4* on nodes
    describe("on nodes") {
      //@+others
      //@+node:gcross.20110414153139.1515: *5* is correct for equal nodes.
      it("is correct for equal nodes.") {
        val node = createEmptyTree.createNode(null,null)
        node should be (node)
      }
      //@+node:gcross.20110414153139.1516: *5* is correct for non-equal nodes.
      it("is correct for non-equal nodes.") {
        val tree = createEmptyTree
        tree.createNode(null,null) should not be tree.createNode(null,null)
      }
      //@-others
    }
    //@+node:gcross.20110414153139.1507: *4* on roots
    describe("on roots") {
      //@+others
      //@+node:gcross.20110414153139.1508: *5* is correct for equal roots.
      it("is correct for equal roots.") {
        val root = createEmptyTree.getRoot
        root should be (root)
      }
      //@+node:gcross.20110414153139.1510: *5* is correct for non-equal roots.
      it("is correct for non-equal roots.") {
        createEmptyTree.getRoot should not be createEmptyTree.getRoot
      }
      //@-others
    }
    //@+node:gcross.20110414153139.1517: *4* is correct when comparing nodes and roots.
    it("is correct when comparing nodes and roots.") {
      val tree = createEmptyTree
      tree.getRoot should not be (tree.createNode(null,null))
    }
    //@-others
  }
  //@+node:gcross.20110414153139.1540: *3* The setBodyOf method
  describe("The setBodyOf method") {
    //@+others
    //@+node:gcross.20110414153139.1518: *4* works correctly.
    it("works correctly.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,null)
      tree.setBodyOf(node,"body")
      node.getBody should be ("body")
    }
    //@+node:gcross.20110414153139.1546: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,null)
      val events = recordEventsDuring(tree) { () => tree.setBodyOf(node,"body") }

      events.size should be (1)
      events(0).getClass should be (classOf[NodeBodyChangedEvent])
      val event = events(0).asInstanceOf[NodeBodyChangedEvent]
      event.getTree should be (tree)
      event.getNode should be (node)
      event.getBody should be (node.getBody)
    }
    //@-others
  }
  //@+node:gcross.20110414153139.1541: *3* The setHeadingOf method
  describe("The setHeadingOf method") {
    //@+others
    //@+node:gcross.20110414153139.1521: *4* works correctly.
    it("works correctly.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,null)
      tree.setHeadingOf(node,"heading")
      node.getHeading should be ("heading")
    }
    //@+node:gcross.20110414153139.1552: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,null)
      val events = recordEventsDuring(tree) { () => tree.setHeadingOf(node,"heading") }

      events.size should be (1)
      events(0).getClass should be (classOf[NodeHeadingChangedEvent])
      val event = events(0).asInstanceOf[NodeHeadingChangedEvent]
      event.getTree should be (tree)
      event.getNode should be (node)
      event.getHeading should be (node.getHeading)
    }
    //@-others
  }
  //@+node:gcross.20110414153139.1542: *3* The insertChildInto method
  describe("The insertChildInto method") {
    //@+others
    //@+node:gcross.20110414153139.1522: *4* works for adding a child
    describe("works for adding a child") {
      //@+others
      //@+node:gcross.20110414153139.1523: *5* to an empty parent.
      it("to an empty parent.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val child = tree.createNode(null,null)
        tree.insertChildInto(root,child,0)
        root.getChildCount should be (1)
        root.getChild(0) should be (child)
        child.getParents.toSet should be (Set(root))
      }
      //@+node:gcross.20110414153139.1527: *5* after a sibling.
      it("after a sibling.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val child1 = tree.createNode(null,null)
        val child2 = tree.createNode(null,null)
        tree.insertChildInto(root,child1,0)
        tree.insertChildInto(root,child2,1)
        root.getChildCount should be (2)
        root.getChild(0) should be (child1)
        root.getChild(1) should be (child2)
        child1.getParents.toSet should be (Set(root))
        child2.getParents.toSet should be (Set(root))
      }
      //@+node:gcross.20110414153139.1525: *5* before a sibling.
      it("before a sibling.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val child1 = tree.createNode(null,null)
        val child2 = tree.createNode(null,null)
        tree.insertChildInto(root,child1,0)
        tree.insertChildInto(root,child2,0)
        root.getChildCount should be (2)
        root.getChild(0) should be (child2)
        root.getChild(1) should be (child1)
        child1.getParents.toSet should be (Set(root))
        child2.getParents.toSet should be (Set(root))
      }
      //@-others
    }
    //@+node:gcross.20110414153139.1554: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val child = tree.createNode(null,null)
      val events = recordEventsDuring(tree) { () => tree.insertChildInto(tree.getRoot,child,0) }

      events.size should be (1)
      events(0).getClass should be (classOf[ChildInsertedEvent])
      val event = events(0).asInstanceOf[ChildInsertedEvent]
      event.getTree should be (tree)
      event.getChild should be (child)
      event.getChildIndex should be (0)
    }
    //@-others
  }
  //@+node:gcross.20110414153139.1519: *3* The removeChildFrom method
  describe("The removeChildFrom method") {
    //@+others
    //@+node:gcross.20110414153139.1532: *4* works for removing
    describe("works for removing") {
      //@+others
      //@+node:gcross.20110414153139.1533: *5* an only child.
      it("an only child.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val child = tree.createNode(null,null)
        tree.insertChildInto(root,child,0)
        tree.removeChildFrom(root,0)
        root.getChildCount should be (0)
        child.getParents.toSet should be (Set())
      }
      //@+node:gcross.20110414153139.1534: *5* the first sibling of two.
      it("the first sibling of two.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val child1 = tree.createNode(null,null)
        val child2 = tree.createNode(null,null)
        tree.insertChildInto(root,child1,0)
        tree.insertChildInto(root,child2,1)
        tree.removeChildFrom(root,0)
        root.getChildCount should be (1)
        root.getChild(0) should be (child2)
        child1.getParents.toSet should be (Set())
        child2.getParents.toSet should be (Set(root))
      }
      //@+node:gcross.20110414153139.1539: *5* the second sibling of two.
      it("the second sibling of two.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val child1 = tree.createNode(null,null)
        val child2 = tree.createNode(null,null)
        tree.insertChildInto(root,child1,0)
        tree.insertChildInto(root,child2,1)
        tree.removeChildFrom(root,1)
        root.getChildCount should be (1)
        root.getChild(0) should be (child1)
        child1.getParents.toSet should be (Set(root))
        child2.getParents.toSet should be (Set())
      }
      //@-others
    }
    //@+node:gcross.20110414153139.1556: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val child = tree.createNode(null,null)
      tree.insertChildInto(root,child,0)
      val events = recordEventsDuring(tree) { () => tree.removeChildFrom(root,0) }

      events.size should be (1)
      events(0).getClass should be (classOf[ChildRemovedEvent])
      val event = events(0).asInstanceOf[ChildRemovedEvent]
      event.getTree should be (tree)
      event.getChild should be (child)
      event.getChildIndex should be (0)
    }
    //@-others
  }
  //@-others
}
//@-others
//@-leo
