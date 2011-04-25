//@+leo-ver=5-thin
//@+node:gcross.20110422115402.4686: * @file TreeLogger.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110422115402.4687: ** << Imports >>
import java.util.concurrent.Callable
import scala.collection.mutable.ArrayBuffer

import viewpoint.event.TreeChangeListener
import viewpoint.model.{Child,Node,Parent,Tree}
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.4707: ** class TreeLogger
class TreeLogger(tree: Tree) extends Tree {
  //@+<< Imports >>
  //@+node:gcross.20110422115402.4711: *3* << Imports >>
  import TreeLogger._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110422115402.4710: *3* << Fields >>
  protected val log = new ArrayBuffer[LogItem]
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110422115402.4708: *3* addTreeChangeListener
  def addTreeChangeListener(listener: TreeChangeListener) {
    tree.addTreeChangeListener(listener)
  }
  //@+node:gcross.20110422115402.4709: *3* createNode
  def createNode(id: String, heading: String, body: String): Node = {
    val node = tree.createNode(id,heading,body)
    log += new CreateNode(id,heading,body)
    node
  }
  //@+node:gcross.20110422115402.4712: *3* forgetNode
  def forgetNode(node: Node) {
    tree.forgetNode(node)
    log += new ForgetNode(node)
  }
  //@+node:gcross.20110422115402.4721: *3* getRoot
  def getRoot: Parent = tree.getRoot
  //@+node:gcross.20110422115402.4713: *3* insertChildInto
  def insertChildInto(parent: Parent, node: Node, index: Int): Long = {
    val tag = tree.insertChildInto(parent,node,index)
    log += new InsertChildInto(parent,node,tag,index)
    tag
  }

  def insertChildInto(parent: Parent, child: Child, index: Int) {
    tree.insertChildInto(parent,child,index)
    log += new InsertChildInto(parent,child,index)
  }
  //@+node:gcross.20110422115402.4720: *3* lookupNode
  def lookupNode(id: String) = tree.lookupNode(id)
  //@+node:gcross.20110422115402.4714: *3* removeChildFrom
  def removeChildFrom(parent: Parent, index: Int): Child = {
    val child = tree.removeChildFrom(parent,index)
    log += new RemoveChildFrom(parent,child,index)
    child
  }
  //@+node:gcross.20110422115402.4715: *3* setBodyOf
  def setBodyOf(node: Node, new_body: String) {
    val old_body = node.getBody
    tree.setBodyOf(node,new_body)
    log += new SetBodyOf(node,old_body,new_body)
  }
  //@+node:gcross.20110422115402.4717: *3* setHeadingOf
  def setHeadingOf(node: Node, new_heading: String) {
    val old_heading = node.getHeading
    tree.setHeadingOf(node,new_heading)
    log += new SetHeadingOf(node,old_heading,new_heading)
  }
  //@+node:gcross.20110422115402.4718: *3* removeTreeChangeListener
  def removeTreeChangeListener(listener: TreeChangeListener) {
    tree.removeTreeChangeListener(listener)
  }
  //@+node:gcross.20110422115402.4719: *3* withinTransaction
  def withinTransaction[V](transaction: Callable[V]): V =
    tree.withinTransaction(transaction)
  //@-others
}
//@+node:gcross.20110422115402.4688: ** object TreeLogger
object TreeLogger {
  //@+<< Log Items >>
  //@+node:gcross.20110422115402.4689: *3* << Log Items >>
  sealed abstract class LogItem {
    def inverse: LogItem
    def apply(tree: Tree): Unit
  }
  //@+others
  //@+node:gcross.20110422115402.4690: *4* CreateNode
  case class CreateNode(id: String, heading: String, body: String) extends LogItem {
    def inverse = new ForgetNode(id,heading,body)
    def apply(tree: Tree) { tree.createNode(id,heading,body) }
  }
  //@+node:gcross.20110422115402.4691: *4* ForgetNode
  case class ForgetNode(id: String, heading: String, body: String) extends LogItem {
    def this(node: Node) = this(node.getId,node.getHeading,node.getBody)
    def inverse = new CreateNode(id,heading,body)
    def apply(tree: Tree) { tree.forgetNode(tree.lookupNode(id)) }
  }
  //@+node:gcross.20110422115402.4692: *4* InsertChildInto
  case class InsertChildInto(
    maybe_parent_id: Option[String],
    child_id: String,
    child_tag: Long,
    index: Int
  ) extends LogItem {
    def this(parent: Parent, node: Node, tag: Long, index: Int) =
      this(parent.getId,node.getId,tag,index)
    def this(parent: Parent, child: Child, index: Int) =
      this(parent.getId,child.getNode.getId,child.getTag,index)
    def inverse = new RemoveChildFrom(maybe_parent_id,child_id,child_tag,index)
    def apply(tree: Tree) {
      tree.insertChildInto(
        tree.lookupParent(maybe_parent_id),
        tree.lookupChild(child_id,child_tag),
        index
      )
    }
  }
  //@+node:gcross.20110422115402.4703: *4* RemoveChildFrom
  case class RemoveChildFrom(
    maybe_parent_id: Option[String],
    child_id: String,
    child_tag: Long,
    index: Int
  ) extends LogItem {
    def this(parent: Parent, child: Child, index: Int) =
      this(parent.getId,child.getNode.getId,child.getTag,index)
    def inverse = new InsertChildInto(maybe_parent_id,child_id,child_tag,index)
    def apply(tree: Tree) {
      val parent = tree.lookupParent(maybe_parent_id)
      val child = tree.removeChildFrom(parent,parent.getIndexOfChild(child_tag))
      assert(child.getNode.getId == child_id)
    }
  }
  //@+node:gcross.20110422115402.4704: *4* SetBodyOf
  case class SetBodyOf(id: String, old_body: String, new_body: String) extends LogItem {
    def this(node: Node, old_body: String, new_body: String) =
      this(node.getId,old_body,new_body)
    def inverse = new SetBodyOf(id,new_body,old_body)
    def apply(tree: Tree) { tree.setBodyOf(tree.lookupNode(id),new_body) }
  }
  //@+node:gcross.20110422115402.4706: *4* SetHeadingOf
  case class SetHeadingOf(id: String, old_heading: String, new_heading: String) extends LogItem {
    def this(node: Node, old_heading: String, new_heading: String) =
      this(node.getId,old_heading,new_heading)
    def inverse = new SetHeadingOf(id,new_heading,old_heading)
    def apply(tree: Tree) { tree.setHeadingOf(tree.lookupNode(id),new_heading) }
  }
  //@-others
  //@-<< Log Items >>
  //@+others
  //@-others
}
//@-others
//@-leo
