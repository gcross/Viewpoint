//@+leo-ver=5-thin
//@+node:gcross.20110417144805.1874: * @file UnexpectedChildAtIndexWhenExecutingAction.scala
//@@language Scala
package viewpoint.action

import viewpoint.model._

case class UnexpectedChildAtIndexWhenExecutingAction(parent: Parent, child: Node, index: Int, action: Action) extends Exception {
  override val toString = "The child at index %s was wrong when executing action %s.".format(index,action)
}
//@-leo
