//@+leo-ver=5-thin
//@+node:gcross.20110503170605.1704: * @file TemporaryAccessMutator.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110503170605.1705: ** << Imports >>
import java.util.concurrent.Callable
import scala.collection.mutable.ArrayBuffer

import viewpoint.event.TreeChangeListener
import viewpoint.model.{Child,Mutator,Node,Parent}
//@-<< Imports >>

//@+others
//@+node:gcross.20110503170605.1706: ** class TemporaryAccessMutator
final class TemporaryAccessMutator(initial_mutator: Mutator) extends Mutator {
  //@+<< Imports >>
  //@+node:gcross.20110503170605.1707: *3* << Imports >>
  import TemporaryAccessMutator._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110503170605.1708: *3* << Fields >>
  private[this] var maybe_mutator: Option[Mutator] = Some(initial_mutator)
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110503170605.1709: *3* createNode
  def createNode(id: String, heading: String, body: String): Node =
    maybe_mutator match {
      case Some(mutator) => mutator.createNode(id,heading,body)
      case None => throw AccessRevoked
    }
  //@+node:gcross.20110503170605.1710: *3* forgetNode
  def forgetNode(node: Node) {
    maybe_mutator match {
      case Some(mutator) => mutator.forgetNode(node)
      case None => throw AccessRevoked
    }
  }
  //@+node:gcross.20110503170605.1712: *3* getRoot
  def getRoot: Parent =
    maybe_mutator match {
      case Some(mutator) => mutator.getRoot
      case None => throw AccessRevoked
    }
  //@+node:gcross.20110503170605.1713: *3* insertChildInto
  def insertChildInto(parent: Parent, node: Node, index: Int): Long =
    maybe_mutator match {
      case Some(mutator) => mutator.insertChildInto(parent,node,index)
      case None => throw AccessRevoked
    }

  def insertChildInto(parent: Parent, child: Child, index: Int) {
    maybe_mutator match {
      case Some(mutator) => mutator.insertChildInto(parent,child,index)
      case None => throw AccessRevoked
    }
  }
  //@+node:gcross.20110503170605.1714: *3* lookupNode
  def lookupNode(id: String) =
    maybe_mutator match {
      case Some(mutator) => mutator.lookupNode(id)
      case None => throw AccessRevoked
    }
  //@+node:gcross.20110503170605.1715: *3* removeChildFrom
  def removeChildFrom(parent: Parent, index: Int): Child =
    maybe_mutator match {
      case Some(mutator) => mutator.removeChildFrom(parent,index)
      case None => throw AccessRevoked
    }
  //@+node:gcross.20110503170605.1722: *3* revokeAccess
  def revokeAccess() {
    maybe_mutator = None
  }
  //@+node:gcross.20110503170605.1716: *3* setBodyOf
  def setBodyOf(node: Node, new_body: String) {
    maybe_mutator match {
      case Some(mutator) => mutator.setBodyOf(node,new_body)
      case None => throw AccessRevoked
    }
  }
  //@+node:gcross.20110503170605.1717: *3* setHeadingOf
  def setHeadingOf(node: Node, new_heading: String) {
    maybe_mutator match {
      case Some(mutator) => mutator.setHeadingOf(node,new_heading)
      case None => throw AccessRevoked
    }
  }
  //@-others
}
//@+node:gcross.20110503170605.1719: ** object TemporaryAccessMutator
object TemporaryAccessMutator {
  //@+others
  //@+node:gcross.20110503170605.1720: *3* exception AccessDenied
  object AccessRevoked extends Exception {
    override val toString = "Access has been revoked."
  }
  //@+node:gcross.20110503170605.1721: *3* withTemporaryAccess
  def withTemporaryAccess[V](mutator: Mutator)(callback: Mutator => V): V = {
    val temporary_mutator = new TemporaryAccessMutator(mutator)
    try {
      callback(temporary_mutator)
    } finally {
      temporary_mutator.revokeAccess()
    }
  }
  //@-others
}
//@-others
//@-leo
