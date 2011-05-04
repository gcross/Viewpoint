//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1748: * @file RichInterface.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110420231854.1749: ** << Imports >>
import viewpoint.model.{Librarian,Mutator,Node,Parent}
//@-<< Imports >>

//@+others
//@+node:gcross.20110420231854.1750: ** object RichInterface
object RichInterface {
  implicit def asRichLibrarian(librarian: Librarian): RichLibrarian = new RichLibrarian(librarian)
  implicit def asRichMutator(mutator: Mutator): RichMutator = new RichMutator(mutator)
  implicit def asRichNode(node: Node): RichNode = new RichNode(node)
  implicit def asRichParent(parent: Parent): RichParent = new RichParent(parent)
}
//@-others
//@-leo
