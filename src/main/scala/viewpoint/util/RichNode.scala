//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1624: * @file RichNode.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110420231854.1625: ** << Imports >>
import scala.collection.mutable.{Queue,Set}

import viewpoint.model.{Child,Node,Parent}
//@-<< Imports >>

//@+others
//@+node:gcross.20110420231854.1626: ** class RichNode
class RichNode(node: Node) extends RichParent(node) {
  //@+<< Imports >>
  //@+node:gcross.20110420231854.1679: *3* << Imports >>
  import RichNode._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110420231854.1678: *3* << Fields >>
  override val self: Node = node
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110420231854.1638: *3* ===
  def ===(other: Node): Boolean = {
    if(this eq other) return true
    val id = self.getId
    if(other.getId != id) return false
    val observed_node_ids = Set[String](id)
    val remaining_nodes = Queue[(Node,Node)]((this,other))
    while(!remaining_nodes.isEmpty) {
      val (node1,node2) = remaining_nodes.dequeue
      if(node1.getHeading != node2.getHeading) return false
      if(node1.getBody != node2.getBody) return false
      val number_of_children = node1.getChildCount
      if(number_of_children != node2.getChildCount) return false
      for(index <- 0 until number_of_children) {
        val child_node1 = node1.getChild(index).getNode
        val child_node2 = node2.getChild(index).getNode
        val child_node_id = child_node1.getId
        if(child_node_id != child_node2.getId) return false
        if(!observed_node_ids(child_node_id)) {
          observed_node_ids += child_node_id
          if(child_node1 ne child_node2) remaining_nodes.enqueue((child_node1,child_node2))
        }
      }
    }
    true
  }
  //@+node:gcross.20110420231854.1636: *3* getAllNodesInSubtree
  def getAllNodesInSubtree: Iterator[Node] = {
    val observed_nodes = Set[String]()
    val enqueued_nodes = Queue[Node](this)
    new Iterator[Node] {
      def hasNext = !enqueued_nodes.isEmpty
      def next(): Node = {
        val node = enqueued_nodes.dequeue()
        observed_nodes += node.getId
        for(child_node <- node.getChildNodes) {
          if(!observed_nodes(child_node.getId))
            enqueued_nodes.enqueue(child_node)
        }
        node
      }
    }
  }
  //@-others
}
//@+node:gcross.20110420231854.1633: ** object RichNode
object RichNode {
  //@+others
  //@+node:gcross.20110420231854.1634: *3* nodeWrapper/Unwrapper
  implicit def nodeWrapper(node: Node): RichNode = new RichNode(node)
  implicit def nodeUnwrapper(node: RichNode): Node = node.self
  //@-others
}
//@-others
//@-leo
