//@+leo-ver=5-thin
//@+node:gcross.20110422115402.4686: * @file MutatorLogger.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110422115402.4687: ** << Imports >>
import java.util.concurrent.Callable
import scala.collection.mutable.ArrayBuffer

import viewpoint.event.TreeChangeListener
import viewpoint.model.{Child,Mutator,Node,Parent}
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.4707: ** class MutatorLogger
class MutatorLogger(self: Mutator) extends Mutator {
  //@+<< Imports >>
  //@+node:gcross.20110422115402.4711: *3* << Imports >>
  import MutatorLog._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110422115402.4710: *3* << Fields >>
  protected val log = new ArrayBuffer[MutatorLog.Item]
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110422115402.4709: *3* createNode
  def createNode(id: String, heading: String, body: String): Node = {
    val node = self.createNode(id,heading,body)
    log += new CreateNode(id,heading,body)
    node
  }
  //@+node:gcross.20110422115402.4712: *3* forgetNode
  def forgetNode(node: Node) {
    self.forgetNode(node)
    log += new ForgetNode(node)
  }
  //@+node:gcross.20110422115402.5170: *3* getLog
  def getLog: MutatorLog = new MutatorLog(log.toIndexedSeq)
  //@+node:gcross.20110422115402.4721: *3* getRoot
  def getRoot: Parent = self.getRoot
  //@+node:gcross.20110422115402.4713: *3* insertChildInto
  def insertChildInto(parent: Parent, node: Node, index: Int): Long = {
    val tag = self.insertChildInto(parent,node,index)
    log += new InsertChildInto(parent,node,tag,index)
    tag
  }

  def insertChildInto(parent: Parent, child: Child, index: Int) {
    self.insertChildInto(parent,child,index)
    log += new InsertChildInto(parent,child,index)
  }
  //@+node:gcross.20110422115402.4720: *3* lookupNode
  def lookupNode(id: String) = self.lookupNode(id)
  //@+node:gcross.20110422115402.4714: *3* removeChildFrom
  def removeChildFrom(parent: Parent, index: Int): Child = {
    val child = self.removeChildFrom(parent,index)
    log += new RemoveChildFrom(parent,child,index)
    child
  }
  //@+node:gcross.20110422115402.4715: *3* setBodyOf
  def setBodyOf(node: Node, new_body: String) {
    val old_body = node.getBody
    self.setBodyOf(node,new_body)
    log += new SetBodyOf(node,old_body,new_body)
  }
  //@+node:gcross.20110422115402.4717: *3* setHeadingOf
  def setHeadingOf(node: Node, new_heading: String) {
    val old_heading = node.getHeading
    self.setHeadingOf(node,new_heading)
    log += new SetHeadingOf(node,old_heading,new_heading)
  }
  //@+node:gcross.20110422115402.5176: *3* unwind
  def unwind() {
    log.reverseIterator.foreach(_.inverse.apply(self))
    log.clear()
  }
  //@-others
}
//@-others
//@-leo
