//@+leo-ver=5-thin
//@+node:gcross.20110417144805.1872: * @file MoveChildDownOne.scala
//@@language Scala
package viewpoint.action

import viewpoint.model._
import viewpoint.util.JavaConversions._

case class MoveChildDownOne(parent: Parent, child: Node, index: Int) extends Action {
  def actOn(tree: Tree) = {
    if(parent.getChild(index) != child)
      throw UnexpectedChildAtIndexWhenExecutingAction(parent,child,index,this)
    tree.withinTransaction({ () =>
      val child = tree.removeChildFrom(parent,index)
      tree.insertChildInto(parent,child,index+1)
      MoveChildUpOne(parent,child,index+1)
    })
  }
}
//@-leo
