//@+leo-ver=5-thin
//@+node:gcross.20110425115406.1726: * @file TreeChangeEventRecorder.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110425115406.1729: ** << Imports >>
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

import viewpoint.event._
import viewpoint.model.Tree
//@-<< Imports >>

//@+others
//@+node:gcross.20110425115406.1727: ** class TreeChangeEventRecorder
class TreeChangeEventRecorder extends TreeChangeListener {
  val events = new ArrayBuffer[TreeChangeEvent]
  def treeNodeBodyChanged(event: NodeBodyChangedEvent) { events += event }
  def treeNodeChildInserted(event: ChildInsertedEvent) { events += event }
  def treeNodeChildRemoved(event: ChildRemovedEvent) { events += event }
  def treeNodeHeadingChanged(event: NodeHeadingChangedEvent){ events += event }
  def treeNodePropertyChanged(event: NodePropertyChangedEvent) { events += event }
  def treeNodeStructureChanged(event: StructureChangedEvent) { events += event }
}
//@+node:gcross.20110425120511.1722: ** object TreeChangeEventRecorder
object TreeChangeEventRecorder {
  //@+others
  //@+node:gcross.20110425115406.1728: *3* recordEventsDuring
  def recordEventsDuring(tree: Tree)(block: () => Unit): Seq[TreeChangeEvent] = {
    val listener = new TreeChangeEventRecorder
    tree.addTreeChangeListener(listener)
    block ()
    tree.removeTreeChangeListener(listener)
    listener.events
  }
  //@-others
}
//@-others
//@-leo
