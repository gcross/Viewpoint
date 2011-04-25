//@+leo-ver=5-thin
//@+node:gcross.20110422115402.5144: * @file TreeLog.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110422115402.5145: ** << Imports >>
import scala.collection.immutable.Seq

import viewpoint.model.{Child,Node,Parent,Tree}
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.5162: ** class TreeLog
class TreeLog protected (replay_log: Seq[TreeLog.Item], unwind_log: Seq[TreeLog.Item]) {
  //@+<< Imports >>
  //@+node:gcross.20110422115402.5164: *3* << Imports >>
  import TreeLog._
  //@-<< Imports >>
  //@+<< Constructors >>
  //@+node:gcross.20110422115402.5169: *3* << Constructors >>
  def this(log: Seq[TreeLog.Item]) = this(log,log.reverseMap(_.inverse))
  //@-<< Constructors >>
  //@+others
  //@+node:gcross.20110422115402.5168: *3* inverse
  def inverse: TreeLog = new TreeLog(unwind_log,replay_log)
  //@+node:gcross.20110422115402.5165: *3* replay
  def replay(tree: Tree) { replay_log.foreach(_.apply(tree)) }
  //@+node:gcross.20110422115402.5167: *3* unwind
  def unwind(tree: Tree) { unwind_log.foreach(_.apply(tree)) }
  //@-others
}
//@+node:gcross.20110422115402.5154: ** object TreeLog
object TreeLog {
  //@+<< Log Items >>
  //@+node:gcross.20110422115402.5155: *3* << Log Items >>
  sealed abstract class Item {
    def inverse: Item
    def apply(tree: Tree): Unit
  }
  //@+others
  //@+node:gcross.20110422115402.5156: *4* CreateNode
  case class CreateNode(id: String, heading: String, body: String) extends Item {
    def inverse = new ForgetNode(id,heading,body)
    def apply(tree: Tree) { tree.createNode(id,heading,body) }
  }
  //@+node:gcross.20110422115402.5157: *4* ForgetNode
  case class ForgetNode(id: String, heading: String, body: String) extends Item {
    def this(node: Node) = this(node.getId,node.getHeading,node.getBody)
    def inverse = new CreateNode(id,heading,body)
    def apply(tree: Tree) { tree.forgetNode(tree.lookupNode(id)) }
  }
  //@+node:gcross.20110422115402.5158: *4* InsertChildInto
  case class InsertChildInto(
    maybe_parent_id: Option[String],
    child_id: String,
    child_tag: Long,
    index: Int
  ) extends Item {
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
  //@+node:gcross.20110422115402.5159: *4* RemoveChildFrom
  case class RemoveChildFrom(
    maybe_parent_id: Option[String],
    child_id: String,
    child_tag: Long,
    index: Int
  ) extends Item {
    def this(parent: Parent, child: Child, index: Int) =
      this(parent.getId,child.getNode.getId,child.getTag,index)
    def inverse = new InsertChildInto(maybe_parent_id,child_id,child_tag,index)
    def apply(tree: Tree) {
      val parent = tree.lookupParent(maybe_parent_id)
      val child = tree.removeChildFrom(parent,parent.getIndexOfChild(child_tag))
      assert(child.getNode.getId == child_id)
    }
  }
  //@+node:gcross.20110422115402.5160: *4* SetBodyOf
  case class SetBodyOf(id: String, old_body: String, new_body: String) extends Item {
    def this(node: Node, old_body: String, new_body: String) =
      this(node.getId,old_body,new_body)
    def inverse = new SetBodyOf(id,new_body,old_body)
    def apply(tree: Tree) { tree.setBodyOf(tree.lookupNode(id),new_body) }
  }
  //@+node:gcross.20110422115402.5161: *4* SetHeadingOf
  case class SetHeadingOf(id: String, old_heading: String, new_heading: String) extends Item {
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
