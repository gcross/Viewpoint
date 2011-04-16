//@+leo-ver=5-thin
//@+node:gcross.20110417144805.1870: * @file ChangeNodeBody.scala
//@@language Scala
package viewpoint.action

import viewpoint.model._

case class ChangeNodeBody(node: Node, new_body: String) extends Action {
  def actOn(tree: Tree) = {
    val old_body = node.getBody
    tree.setBodyOf(node,new_body)
    ChangeNodeBody(node,old_body)
  }
}
//@-leo
