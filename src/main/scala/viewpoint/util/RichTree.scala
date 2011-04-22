//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1734: * @file RichTree.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110420231854.1735: ** << Imports >>
import java.util.UUID
import scala.collection.mutable.{Queue,Set}

import viewpoint.model.{Node,Tree}
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
  //@+node:gcross.20110420231854.1740: *3* createNode
  def createNode(heading: String, body: String): Node =
    self.createNode(UUID.randomUUID.toString,heading,body)
  def createNode(heading: String): Node = createNode(heading,null)
  def createNode(): Node = createNode(null)
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
