//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1655: * @file RichParent.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110420231854.1656: ** << Imports >>
import scala.collection.mutable.{Queue,Set}

import viewpoint.model.{Child,Node,Parent}
//@-<< Imports >>

//@+others
//@+node:gcross.20110420231854.1657: ** class RichParent
class RichParent(parent: Parent) extends Proxy {
  //@+<< Imports >>
  //@+node:gcross.20110420231854.1681: *3* << Imports >>
  import RichParent._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110420231854.1672: *3* << Fields >>
  override val self: Parent = parent
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110420231854.1661: *3* getChildNodes
  def getChildNodes: Iterator[Node] = getChildren.map(_.getNode)
  //@+node:gcross.20110420231854.1660: *3* getChildren
  def getChildren: Iterator[Child] = {
    val number_of_children = self.getChildCount
    var index = 0
    new Iterator[Child] {
      def hasNext = index < number_of_children
      def next() = {
        val child = self.getChild(index)
        index += 1
        child
      }
    }
  }
  //@+node:gcross.20110420231854.1662: *3* getChildTags
  def getChildTags: Iterator[Long] = getChildren.map(_.getTag)
  //@+node:gcross.20110422115402.4701: *3* getId
  def getId: Option[String] =
    self match {
      case (node: Node) => Some(node.getId)
      case _ => None
    }
  //@-others
}
//@+node:gcross.20110420231854.1666: ** object RichParent
object RichParent {
  //@+others
  //@+node:gcross.20110420231854.1667: *3* parentWrapper/Unwrapper
  implicit def parentWrapper(parent: Parent): RichParent = new RichParent(parent)
  implicit def parentUnwrapper(parent: RichParent): Parent = parent.self
  //@-others
}
//@-others
//@-leo
