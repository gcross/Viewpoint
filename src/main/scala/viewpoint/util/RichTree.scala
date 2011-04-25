//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1734: * @file RichTree.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110420231854.1735: ** << Imports >>
import java.util.UUID
import scala.collection.IndexedSeq
import scala.collection.mutable.{ArrayBuffer,Queue,Set}

import viewpoint.model.{Child,Node,Parent,Tree}
//@-<< Imports >>

//@+others
//@+node:gcross.20110420231854.1736: ** class RichTree
class RichTree(tree: Tree) extends Proxy {
  //@+<< Imports >>
  //@+node:gcross.20110420231854.1737: *3* << Imports >>
  import RichTree._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110420231854.1738: *3* << Fields >>
  override val self: Tree = tree
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110422204808.1666: *3* appendChildrenTo
  def appendChildrenTo(parent: Parent, nodes: Node*): IndexedSeq[Long] = {
    var index = parent.getChildCount
    val tags = new ArrayBuffer[Long]
    for(node <- nodes) {
      tags += self.insertChildInto(parent,node,index)
      index += 1
    }
    tags
  }
  //@+node:gcross.20110422115402.2035: *3* appendChildTo
  def appendChildTo(parent: Parent, node: Node): Long =
    self.insertChildInto(parent,node,parent.getChildCount)
  //@+node:gcross.20110420231854.1740: *3* createNode
  def createNode(heading: String, body: String): Node =
    self.createNode(UUID.randomUUID.toString,heading,body)
  def createNode(heading: String): Node = createNode(heading,null)
  def createNode(): Node = createNode(null)
  //@+node:gcross.20110422115402.4698: *3* lookupChild
  def lookupChild(id: String, tag: Long): Child =
    new Child with ChildEqualityPolicy {
      lazy val node = self.lookupNode(id)
      def getNode = node
      def getTag = tag
    }
  //@+node:gcross.20110422115402.4697: *3* lookupParent
  def lookupParent(maybe_id: Option[String]): Parent =
    maybe_id match {
      case Some(id) => self.lookupNode(id)
      case None => self.getRoot
    }
  //@-others
}
//@+node:gcross.20110420231854.1742: ** object RichTree
object RichTree {
  //@+others
  //@+node:gcross.20110420231854.1743: *3* treeWrapper/Unwrapper
  implicit def treeWrapper(tree: Tree): RichTree = new RichTree(tree)
  implicit def treeUnwrapper(tree: RichTree): Tree = tree.self
  //@-others
}
//@-others
//@-leo
