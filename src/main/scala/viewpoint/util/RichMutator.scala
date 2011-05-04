//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1734: * @file RichMutator.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110420231854.1735: ** << Imports >>
import java.util.UUID
import scala.collection.IndexedSeq
import scala.collection.mutable.{ArrayBuffer,Queue,Set}

import viewpoint.model.{Child,Mutator,Node,Parent}
//@-<< Imports >>

//@+others
//@+node:gcross.20110420231854.1736: ** class RichMutator
class RichMutator(mutator: Mutator) extends RichLibrarian(mutator) {
  //@+<< Imports >>
  //@+node:gcross.20110420231854.1737: *3* << Imports >>
  import RichMutator._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110420231854.1738: *3* << Fields >>
  override val self: Mutator = mutator
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110422204808.1666: *3* appendChildrenTo
  def appendChildrenTo(parent: Parent, nodes: Node*): IndexedSeq[Long] = {
    var index = parent.getChildCount
    val tags = new ArrayBuffer[Long]
    for(node <- nodes) {
      tags += self.insertChildInto(parent,node,index)
      index += 1
    }
    tags
  }
  //@+node:gcross.20110422115402.2035: *3* appendChildTo
  def appendChildTo(parent: Parent, node: Node): Long =
    self.insertChildInto(parent,node,parent.getChildCount)
  //@+node:gcross.20110420231854.1740: *3* createNode
  def createNode(heading: String, body: String): Node =
    self.createNode(UUID.randomUUID.toString,heading,body)
  def createNode(heading: String): Node = createNode(heading,null)
  def createNode(): Node = createNode(null)
  //@-others
}
//@+node:gcross.20110420231854.1742: ** object RichMutator
object RichMutator {
  //@+others
  //@+node:gcross.20110420231854.1743: *3* mutatorWrapper/Unwrapper
  implicit def mutatorWrapper(mutator: Mutator): RichMutator = new RichMutator(mutator)
  implicit def mutatorUnwrapper(rich_mutator: RichMutator): Mutator = rich_mutator.self
  //@-others
}
//@-others
//@-leo
