//@+leo-ver=5-thin
//@+node:gcross.20110414153139.1472: * @file ModelSpecification.scala
//@@language Scala
package viewpoint.model.testing

//@+<< Imports >>
//@+node:gcross.20110414153139.1473: ** << Imports >>
import java.util.concurrent.Callable
import scala.collection.JavaConversions._
import scala.collection.Seq
import scala.collection.mutable.{ArrayBuffer,HashSet}
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.event._
import viewpoint.model._
import viewpoint.util.JavaConversions._
import viewpoint.util.RichInterface._
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
  //@+node:gcross.20110422115402.2028: *3* createNode creates a node with the correct properties.
  describe("createNode creates a node with the correct properties.") {
    val node = createEmptyTree.createNode("id","heading","body")
    node.getId should be ("id")
    node.getHeading should be ("heading")
    node.getBody should be ("body")
  }
  //@+node:gcross.20110414153139.1503: *3* lookupNode returns the correct node.
  describe("lookupNode returns the correct node.") {
    val tree = createEmptyTree
    val node1 = tree.createNode()
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
    val node = tree.createNode()
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
        val node = createEmptyTree.createNode()
        node should be (node)
      }
      //@+node:gcross.20110414153139.1516: *5* is correct for non-equal nodes.
      it("is correct for non-equal nodes.") {
        val tree = createEmptyTree
        tree.createNode() should not be tree.createNode()
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
      tree.getRoot should not be (tree.createNode())
    }
    //@-others
  }
  //@+node:gcross.20110414153139.1540: *3* The setBodyOf method
  describe("The setBodyOf method") {
    //@+others
    //@+node:gcross.20110414153139.1518: *4* works correctly.
    it("works correctly.") {
      val tree = createEmptyTree
      val node = tree.createNode()
      tree.setBodyOf(node,"body")
      node.getBody should be ("body")
    }
    //@+node:gcross.20110414153139.1546: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val node = tree.createNode()
      val events = recordEventsDuring(tree) { () => tree.setBodyOf(node,"body") }

      events.size should be (1)
      events(0).getClass should be (classOf[NodeBodyChangedEvent])
      val event = events(0).asInstanceOf[NodeBodyChangedEvent]
      event.getTree should be (tree)
      event.getNode should be (node)
      event.getBody should be (node.getBody)
    }
    //@+node:gcross.20110414153139.2058: *4* logs the correct undo for the transaction.
    it("logs the correct undo for the transaction.") {
      val tree = createEmptyTree
      val node = tree.createNode(null,"body")

      val listener = new EventRecorderTreeChangeListener
      val events = listener.events
      tree.addTreeChangeListener(listener)

      val E = new Exception
      try {
        tree.withinTransaction({ () =>
          tree.setBodyOf(node,"soul")
          throw E
        })
      } catch { case E => }

      node.getBody should be ("body")

      events.size should be (2)
      events(1).getClass should be (classOf[NodeBodyChangedEvent])
      val event = events(1).asInstanceOf[NodeBodyChangedEvent]
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
      val node = tree.createNode()
      tree.setHeadingOf(node,"heading")
      node.getHeading should be ("heading")
    }
    //@+node:gcross.20110414153139.1552: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val node = tree.createNode()
      val events = recordEventsDuring(tree) { () => tree.setHeadingOf(node,"heading") }

      events.size should be (1)
      events(0).getClass should be (classOf[NodeHeadingChangedEvent])
      val event = events(0).asInstanceOf[NodeHeadingChangedEvent]
      event.getTree should be (tree)
      event.getNode should be (node)
      event.getHeading should be (node.getHeading)
    }
    //@+node:gcross.20110414153139.2060: *4* logs the correct undo for the transaction.
    it("logs the correct undo for the transaction.") {
      val tree = createEmptyTree
      val node = tree.createNode("heading",null)
      val E = new Exception

      val listener = new EventRecorderTreeChangeListener
      val events = listener.events
      tree.addTreeChangeListener(listener)

      try {
        tree.withinTransaction({ () =>
          tree.setHeadingOf(node,"footing")
          throw E
        })
      } catch { case E => }

      node.getHeading should be ("heading")

      events.size should be (2)
      events(1).getClass should be (classOf[NodeHeadingChangedEvent])
      val event = events(1).asInstanceOf[NodeHeadingChangedEvent]
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
        val node = tree.createNode()
        val tag = tree.insertChildInto(root,node,0)
        root.getChildCount should be (1)
        val child = root.getChild(0)
        child.getNode should be (node)
        node.getParents.toSet should be (Set(root))
        child.getTag should be (tag)
      }
      //@+node:gcross.20110414153139.1527: *5* after a sibling.
      it("after a sibling.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val node1 = tree.createNode()
        val node2 = tree.createNode()
        val tag1 = tree.insertChildInto(root,node1,0)
        val tag2 = tree.insertChildInto(root,node2,1)
        root.getChildCount should be (2)
        val child1 = root.getChild(0)
        child1.getNode should be (node1)
        node1.getParents.toSet should be (Set(root))
        child1.getTag should be (tag1)
        val child2 = root.getChild(1)
        child2.getNode should be (node2)
        node2.getParents.toSet should be (Set(root))
        child2.getTag should be (tag2)
      }
      //@+node:gcross.20110414153139.1525: *5* before a sibling.
      it("before a sibling.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val node1 = tree.createNode()
        val node2 = tree.createNode()
        val tag1 = tree.insertChildInto(root,node1,0)
        val tag2 = tree.insertChildInto(root,node2,0)
        root.getChildCount should be (2)
        val child1 = root.getChild(1)
        child1.getNode should be (node1)
        node1.getParents.toSet should be (Set(root))
        child1.getTag should be (tag1)
        val child2 = root.getChild(0)
        child2.getNode should be (node2)
        node2.getParents.toSet should be (Set(root))
        child2.getTag should be (tag2)
      }
      //@+node:gcross.20110418122658.2102: *5* with a valid tag
      it("with a valid tag.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val node = tree.createNode()
        val tag = tree.insertChildInto(root,node,0)
        val child = tree.removeChildFrom(root,0)
        tree.insertChildInto(root,child,0)
        root.getChildCount should be (1)
        root.getChild(0) should be (child)
        child.getNode should be (node)
        node.getParents.toSet should be (Set(root))
        child.getTag should be (tag)
      }
      //@-others
    }
    //@+node:gcross.20110414153139.1554: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val node = tree.createNode()
      var tag: Long = 0
      val events = recordEventsDuring(tree) { () => tag = tree.insertChildInto(tree.getRoot,node,0) }
      events.size should be (1)
      events(0).getClass should be (classOf[ChildInsertedEvent])
      val event = events(0).asInstanceOf[ChildInsertedEvent]
      event.getTree should be (tree)
      event.getChildIndex should be (0)
      event.getChildNode should be (node)
      event.getChildTag should be (tag)
    }
    //@+node:gcross.20110414153139.2066: *4* logs the correct undo for the transaction.
    it("logs the correct undo for the transaction.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()
      var tag: Long = 0

      val listener = new EventRecorderTreeChangeListener
      val events = listener.events
      tree.addTreeChangeListener(listener)

      val E = new Exception
      try {
        tree.withinTransaction({ () =>
          tag = tree.insertChildInto(root,node,0)
          throw E
        })
      } catch { case E => }

      root.getChildCount should be (0)

      events.size should be (2)
      events(1).getClass should be (classOf[ChildRemovedEvent])
      val event = events(1).asInstanceOf[ChildRemovedEvent]
      event.getTree should be (tree)
      event.getChildIndex should be (0)
      event.getChildNode should be (node)
      event.getChildTag should be (tag)
    }
    //@+node:gcross.20110418122658.2107: *4* returns the correct value.
    it("returns the correct value.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()
      val tag = tree.insertChildInto(root,node,0)
      root.getChild(0).getTag should be (tag)
    }
    //@+node:gcross.20110418122658.2103: *4* throws an exception when adding a child with
    describe("throws an exception when adding a child with") {
      //@+others
      //@+node:gcross.20110418122658.2101: *5* an invalid tag.
      it("an invalid tag.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val node = tree.createNode()
        try {
          tree.insertChildInto(root,new Child { def getNode: Node = node; def getTag: Long = 100 },0)
          fail("Exception was not thrown.")
        } catch {
          case (e: InvalidChildTagException) => {
            e.getParent should be (root)
            e.getTag should be (100)
          }
        }
      }
      //@+node:gcross.20110418122658.2105: *5* an existing tag.
      it("an existing tag.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val node = tree.createNode()
        val tag = tree.insertChildInto(root,node,0)
        try {
          tree.insertChildInto(root,root.getChild(0),0)
          fail("Exception was not thrown.")
        } catch {
          case (e: InvalidChildTagException) => {
            e.getParent should be (root)
            e.getTag should be (tag)
          }
        }
      }
      //@-others
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
        val node = tree.createNode()
        val tag = tree.insertChildInto(root,node,0)
        val child = tree.removeChildFrom(root,0)
        child.getNode should be (node)
        child.getTag should be (tag)
        root.getChildCount should be (0)
        node.getParents.toSet should be (Set())
      }
      //@+node:gcross.20110414153139.1534: *5* the first sibling of two.
      it("the first sibling of two.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val node1 = tree.createNode()
        val node2 = tree.createNode()
        val tag1 = tree.insertChildInto(root,node1,0)
        val tag2 = tree.insertChildInto(root,node2,1)
        val child1 = tree.removeChildFrom(root,0)
        val child2 = root.getChild(0)
        root.getChildCount should be (1)
        child1.getNode should be (node1)
        child1.getTag should be (tag1)
        child2.getNode should be (node2)
        child2.getTag should be (tag2)
        node1.getParents.toSet should be (Set())
        node2.getParents.toSet should be (Set(root))
      }
      //@+node:gcross.20110414153139.1539: *5* the second sibling of two.
      it("the second sibling of two.") {
        val tree = createEmptyTree
        val root = tree.getRoot
        val node1 = tree.createNode()
        val node2 = tree.createNode()
        val tag1 = tree.insertChildInto(root,node1,0)
        val tag2 = tree.insertChildInto(root,node2,1)
        val child1 = root.getChild(0)
        val child2 = tree.removeChildFrom(root,1)
        root.getChildCount should be (1)
        child1.getNode should be (node1)
        child1.getTag should be (tag1)
        child2.getNode should be (node2)
        child2.getTag should be (tag2)
        node1.getParents.toSet should be (Set(root))
        node2.getParents.toSet should be (Set())
      }
      //@-others
    }
    //@+node:gcross.20110414153139.1556: *4* fires the correct event.
    it("fires the correct event.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()
      val tag = tree.insertChildInto(root,node,0)
      var child: Child = null
      val events = recordEventsDuring(tree) { () => child = tree.removeChildFrom(root,0) }

      events.size should be (1)
      events(0).getClass should be (classOf[ChildRemovedEvent])
      val event = events(0).asInstanceOf[ChildRemovedEvent]
      event.getTree should be (tree)
      event.getChild should be (child)
      event.getChildIndex should be (0)
    }
    //@+node:gcross.20110414153139.2071: *4* logs the correct undo for the transaction.
    it("logs the correct undo for the transaction.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()
      val tag = tree.insertChildInto(root,node,0)

      val listener = new EventRecorderTreeChangeListener
      val events = listener.events
      tree.addTreeChangeListener(listener)

      val E = new Exception
      try {
        tree.withinTransaction({ () =>
          tree.removeChildFrom(root,0)
          throw E
        })
      } catch { case E => }

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
    //@+node:gcross.20110414153139.5151: *4* returns the correct value.
    it("returns the correct value.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()
      tree.insertChildInto(root,node,0)
      val child = root.getChild(0)
      tree.removeChildFrom(root,0) should be (child)
    }
    //@-others
  }
  //@+node:gcross.20110414153139.2313: *3* The withinTransaction method
  describe("The withinTransaction method") {
    //@+others
    //@+node:gcross.20110414153139.2314: *4* returns the value returned by the callback.
    it("returns the value returned by the callback.") {
      createEmptyTree.withinTransaction({ () =>
        42
      }) should be (42)
    }
    //@+node:gcross.20110414153139.2316: *4* throws the value thrown by the callback.
    it("throws the value thrown by the callback.") {
      val correct_exception = new Exception
      val thrown_exception: Exception =
        try {
          createEmptyTree.withinTransaction({ () =>
            throw correct_exception
          })
          fail("Exception was not thrown.")
        } catch {
          case (e: Exception) => e
        }
      assert(thrown_exception eq correct_exception)
    }
    //@+node:gcross.20110414153139.2318: *4* multi-step transactions are undone.
    it("multi-step transactions are completely undone.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val node3 = tree.createNode()
      val node4 = tree.createNode()
      tree.insertChildInto(root,node1,0)
      tree.insertChildInto(root,node3,1)

      val E = new Exception
      try {
        tree.withinTransaction({ () =>
          tree.insertChildInto(root,node2,1)
          tree.removeChildFrom(root,0)
          tree.insertChildInto(root,node1,1)
          tree.removeChildFrom(root,1)
          tree.insertChildInto(root,node4,2)
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
  //@+node:gcross.20110414153139.5154: *3* The getIndexOfChildTag method
  describe("The getIndexOfChildTag method") {
    //@+others
    //@+node:gcross.20110414153139.5155: *4* works for multiple cloned children.
    it("works for multiple cloned children.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      val tags = new ArrayBuffer[Long]
      tags += tree.insertChildInto(root,node1,0)
      tags += tree.insertChildInto(root,node2,1)
      tags += tree.insertChildInto(root,node1,2)
      tags += tree.insertChildInto(root,node2,3)
      tags += tree.insertChildInto(root,node1,4)
      tags += tree.insertChildInto(root,node2,5)
      tags += tree.insertChildInto(root,node1,6)
      for(index <- 0 to 6) { root.getIndexOfChild(tags(index)) should be (index) }
    }
    //@+node:gcross.20110414153139.5161: *4* returns -1 when the child is not present.
    it("returns -1 when the child is not present.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val child1 = tree.createNode()
      val child2 = tree.createNode()
      val child3 = tree.createNode()
      val tags = new HashSet[Long]
      tags += tree.insertChildInto(root,child1,0)
      tags += tree.insertChildInto(root,child2,1)
      tags += tree.insertChildInto(root,child1,2)
      tags += tree.insertChildInto(root,child2,3)
      tags += tree.insertChildInto(root,child1,4)
      tags += tree.insertChildInto(root,child2,5)
      tags += tree.insertChildInto(root,child1,6)
      var tag = 10
      while(tags(tag)) tag += 1
      root.getIndexOfChild(tag) should be (-1)
    }
    //@-others
  }
  //@+node:gcross.20110422115402.1649: *3* The forgetNode method
  describe("The forgetNode method") {
    //@+others
    //@+node:gcross.20110422115402.1650: *4* causes a node to no longer appear in a tree.
    it("causes a node to no longer appear in a tree.") {
      val tree = createEmptyTree
      val node = tree.createNode()
      tree.forgetNode(node)
      tree.lookupNode(node.getId) should be (null)
    }
    //@+node:gcross.20110422115402.1651: *4* throws an exception when a node still has parents.
    it("throws an exception when a node still has parents.") {
      val tree = createEmptyTree
      val root = tree.getRoot
      val node = tree.createNode()
      tree.insertChildInto(root,node,0)
      node.getParents.size should be (1)
      try {
        tree.forgetNode(node)
        fail("Failed to throw exception.")
      } catch {
        case (e: NodeStillHasAncestorsException) => e.getNode should be (node)
      }
      tree.lookupNode(node.getId) should be (node)
    }
    //@+node:gcross.20110422115402.1653: *4* clears the children of the forgotten node.
    it("clears the children of the forgotten node.") {
      val tree = createEmptyTree
      val node1 = tree.createNode()
      val node2 = tree.createNode()
      tree.insertChildInto(node1,node2,0)
      node2.getParents.size should be (1)
      tree.forgetNode(node1)
      node1.getChildren.size should be (0)
      node2.getParents.size should be (0)
    }
    //@-others
  }
  //@-others
}
//@-others
//@-leo
