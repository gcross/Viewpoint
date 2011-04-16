//@+leo-ver=5-thin
//@+node:gcross.20110417144805.1871: * @file ChangeNodeHeading.scala
//@@language Scala
package viewpoint.action

import viewpoint.model._

case class ChangeNodeHeading(node: Node, new_heading: String) extends Action {
  def actOn(tree: Tree) = {
    val old_heading = node.getHeading
    tree.setHeadingOf(node,new_heading)
    ChangeNodeHeading(node,old_heading)
  }
}
//@-leo
