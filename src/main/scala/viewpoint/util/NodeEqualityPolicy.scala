//@+leo-ver=5-thin
//@+node:gcross.20110425121514.1737: * @file NodeEqualityPolicy.scala
//@@language Scala
package viewpoint.util

import viewpoint.model.Node

trait NodeEqualityPolicy extends Node {
  override def equals(other: Any): Boolean =
    other match {
      case (node: Node) if node.getId == getId => true
      case _ => false
    }
  override def hashCode: Int = getId.hashCode
}
//@-leo
