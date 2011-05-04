//@+leo-ver=5-thin
//@+node:gcross.20110503162356.1695: * @file RichLibrarian.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110503162356.1696: ** << Imports >>
import java.util.UUID
import scala.collection.IndexedSeq
import scala.collection.mutable.{ArrayBuffer,Queue,Set}

import viewpoint.model.{Child,Librarian,Node,Parent}
//@-<< Imports >>

//@+others
//@+node:gcross.20110503162356.1697: ** class RichLibrarian
class RichLibrarian(librarian: Librarian) extends Proxy {
  //@+<< Imports >>
  //@+node:gcross.20110503162356.1698: *3* << Imports >>
  import RichLibrarian._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110503162356.1699: *3* << Fields >>
  override val self: Librarian = librarian
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110503162356.1703: *3* lookupChild
  def lookupChild(id: String, tag: Long): Child =
    new Child with ChildEqualityPolicy {
      lazy val node = self.lookupNode(id)
      def getNode = node
      def getTag = tag
    }
  //@+node:gcross.20110503162356.1704: *3* lookupParent
  def lookupParent(maybe_id: Option[String]): Parent =
    maybe_id match {
      case Some(id) => self.lookupNode(id)
      case None => self.getRoot
    }
  //@-others
}
//@+node:gcross.20110503162356.1705: ** object RichLibrarian
object RichLibrarian {
  //@+others
  //@+node:gcross.20110503162356.1706: *3* librarianWrapper/Unwrapper
  implicit def librarianWrapper(librarian: Librarian): RichLibrarian = new RichLibrarian(librarian)
  implicit def librarianUnwrapper(rich_librarian: RichLibrarian): Librarian = rich_librarian.self
  //@-others
}
//@-others
//@-leo
