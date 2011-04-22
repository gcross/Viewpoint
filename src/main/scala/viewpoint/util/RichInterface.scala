//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1748: * @file RichInterface.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110420231854.1749: ** << Imports >>
import viewpoint.model.{Node,Parent,Tree}
//@-<< Imports >>

//@+others
//@+node:gcross.20110420231854.1750: ** object RichInterface
object RichInterface {
  implicit def asRichNode(node: Node): RichNode = new RichNode(node)
  implicit def asRichParent(parent: Parent): RichParent = new RichParent(parent)
  implicit def asRichTree(tree: Tree): RichTree = new RichTree(tree)
}
//@-others
//@-leo
