//@+leo-ver=5-thin
//@+node:gcross.20110413224016.1842: * @file Root.scala
//@@language Scala
package viewpoint.backend.crosswhite.model

//@+<< Imports >>
//@+node:gcross.20110413224016.1843: ** << Imports >>
import viewpoint.{model => interface}
//@-<< Imports >>

//@+others
//@+node:gcross.20110413224016.1844: ** class Root
class Root(val tree: Tree) extends Parent {
  //@+<< Imports >>
  //@+node:gcross.20110413224016.1846: *3* << Imports >>
  import Root._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110413224016.1850: *3* << Fields >>
  val delegate = new Delegate(this)
  //@-<< Fields >>
  //@+others
  //@-others
}
//@+node:gcross.20110413224016.1851: ** object Root
object Root {
  //@+<< Delegate >>
  //@+node:gcross.20110413224016.1852: *3* << Delegate >>
  case class Delegate(root: Root) extends Parent.Delegate(root) with interface.Parent {
    //@+others
    //@+node:gcross.20110413224016.1854: *4* equals
    override def equals(other: Any): Boolean =
      other match {
        case Delegate(other_root) => root eq other_root
        case _ => false
      }
    //@+node:gcross.20110413224016.1856: *4* hashCode
    override def hashCode: Int = root.hashCode
    //@-others
  }
  //@-<< Delegate >>
  //@+others
  //@+node:gcross.20110414153139.2333: *3* getRootDelegate
  implicit def getRootDelegate(root: Root): interface.Parent = root.delegate
  //@-others
}
//@-others
//@-leo
