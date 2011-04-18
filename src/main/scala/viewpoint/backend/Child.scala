//@+leo-ver=5-thin
//@+node:gcross.20110418093501.1580: * @file Child.scala
//@@language Scala
package viewpoint.backend.crosswhite.model

//@+<< Imports >>
//@+node:gcross.20110418093501.1581: ** << Imports >>
import viewpoint.{model => interface}
//@-<< Imports >>

//@+others
//@+node:gcross.20110418093501.1582: ** class Child
case class Child(val node: Node, val tag: Long) {
  //@+<< Imports >>
  //@+node:gcross.20110418122658.2108: *3* << Imports >>
  import Child._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110418093501.1584: *3* << Fields >>
  private[model] val delegate = Delegate(this)
  //@-<< Fields >>
}
//@+node:gcross.20110418093501.1585: ** object Child
object Child {
  //@+<< Delegate >>
  //@+node:gcross.20110418093501.1586: *3* << Delegate >>
  case class Delegate(child: Child) extends interface.Child {
    //@+others
    //@+node:gcross.20110418122658.2112: *4* getNode
    def getNode: interface.Node = child.node
    //@+node:gcross.20110418122658.2111: *4* getTag
    def getTag: Long = child.tag
    //@-others
  }
  //@-<< Delegate >>
  //@+others
  //@+node:gcross.20110418093501.1588: *3* getChildDelegate
  implicit def getChildDelegate(child: Child): interface.Child = child.delegate
  //@-others
}
//@-others
//@-leo
