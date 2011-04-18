//@+leo-ver=5-thin
//@+node:gcross.20110417144805.1873: * @file MoveChildUpOne.scala
//@@language Scala
package viewpoint.action

import viewpoint.model._
import viewpoint.util.JavaConversions._

case class MoveChildUpOne(parent: Parent, tag: Long) extends Action {
  def actOn(tree: Tree) = {
    val index = parent.getIndexOfChildTag(tag)
    if(index < 0)
      throw TargetDisappearedBeforeActionCommenced(this)
    tree.withinTransaction({ () =>
      val child = tree.removeChildFrom(parent,index)
      tree.insertChildInto(parent,child,index-1)
      MoveChildDownOne(parent,tag)
    })
  }
}
//@-leo
