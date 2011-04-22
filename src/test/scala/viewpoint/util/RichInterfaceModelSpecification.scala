//@+leo-ver=5-thin
//@+node:gcross.20110422115402.2030: * @file RichInterfaceModelSpecification.scala
//@@language Scala
package viewpoint.model.testing

//@+<< Imports >>
//@+node:gcross.20110422115402.2031: ** << Imports >>
import org.scalatest.Spec
import org.scalatest.matchers.ShouldMatchers

import viewpoint.model._
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.2032: ** RichInterfaceModelSpecification
abstract class RichInterfaceModelSpecification(createEmptyTree: => Tree) extends Spec with ShouldMatchers {
  //@+others
  //@+node:gcross.20110422115402.2033: *3* The getChildren method iterates correctly over children.
  describe("The getChildren method iterates correctly over children.") {
    val tree = createEmptyTree
    val root = tree.getRoot
    tree.appendChildTo(root,tree.createNode())
    tree.appendChildTo(root,tree.createNode())
    tree.appendChildTo(root,tree.createNode())
    val children = root.getChildren.toSeq
    for(index <- 0 until 3) children(index) should be (root.getChild(index))
  }
  //@-others
}
//@-others
//@-leo
