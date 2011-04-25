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
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.4707: ** class TreeLogger
class TreeLogger(tree: Tree) extends Tree {
  //@+<< Imports >>
  //@+node:gcross.20110422115402.4711: *3* << Imports >>
  import TreeLog._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110422115402.4710: *3* << Fields >>
  protected val log = new ArrayBuffer[TreeLog.Item]
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
  //@+node:gcross.20110422115402.5170: *3* getLog
  def getLog: TreeLog = new TreeLog(log.toIndexedSeq)
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
  //@+node:gcross.20110422115402.4718: *3* removeTreeChangeListener
  def removeTreeChangeListener(listener: TreeChangeListener) {
    tree.removeTreeChangeListener(listener)
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
  //@+node:gcross.20110422115402.4719: *3* withinTransaction
  def withinTransaction[V](transaction: Callable[V]): V =
    tree.withinTransaction(transaction)
  //@-others
}
//@-others
//@-leo
