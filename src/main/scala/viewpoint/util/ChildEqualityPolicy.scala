//@+leo-ver=5-thin
//@+node:gcross.20110425121514.1735: * @file ChildEqualityPolicy.scala
//@@language Scala
package viewpoint.util

import viewpoint.model.Child

trait ChildEqualityPolicy extends Child {
  override def equals(other: Any): Boolean =
    other match {
      case (child: Child)
        if child.getNode == getNode
        && child.getTag == getTag
        => true
      case _ => false
    }
  override def hashCode: Int = {
    val builder = new StringBuilder
    builder.append(getNode.getId)
    builder.append('#')
    builder.append(getTag)
    builder.toString.hashCode
  }
}
//@-leo
