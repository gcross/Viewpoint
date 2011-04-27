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
class RichMutator(override val self: Mutator) extends RichLibrarian(self) {
  //@+<< Imports >>
  //@+node:gcross.20110420231854.1737: *3* << Imports >>
  import RichMutator._
  //@-<< Imports >>
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
  //@+node:gcross.20110425140158.1766: *3* moveChildDownOne
  def moveChildDownOne(parent: Parent, tag: Long) {
    val index = parent.getIndexOfChild(tag)
    val child = self.removeChildFrom(parent,index)
    self.insertChildInto(parent,child,index+1)
  }
  //@+node:gcross.20110425140158.1768: *3* moveChildUpOne
  def moveChildUpOne(parent: Parent, tag: Long) {
    val index = parent.getIndexOfChild(tag)
    val child = self.removeChildFrom(parent,index)
    self.insertChildInto(parent,child,index-1)
  }
  //@+node:gcross.20110425121514.2231: *3* withinTransaction
  def withinTransaction[V](callback: Mutator => V): Transaction[V] =
    Transaction.wrapInTransaction(self,callback)
  //@+node:gcross.20110503191908.1808: *3* withTemporaryAccess
  def withTemporaryAccess[V](callback: Mutator => V): V =
    TemporaryAccessMutator.withTemporaryAccess(self)(callback)
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
