//@+leo-ver=5-thin
//@+node:gcross.20110507151400.1936: * @file NodeDocument.scala
//@@language Scala
package org.fife.ui.rsyntaxtextarea

//@+<< Imports >>
//@+node:gcross.20110507151400.1937: ** << Imports >>
import scala.collection.mutable
import scala.ref.WeakReference
import org.fife.ui.rtextarea.RTextArea

import viewpoint.view.{Lock,TreeController}
//@-<< Imports >>

//@+others
//@+node:gcross.20110507151400.1938: ** class NodeDocument
class NodeDocument private(controller: TreeController, node: Node, client: BodyEditor) extends RSyntaxDocument {
  //@+<< Imports >>
  //@+node:gcross.20110507151400.1945: *3* << Imports >>
  import NodeDocument.documents
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110507151400.1942: *3* << Fields >>
  protected val clients = mutable.Set[BodyEditor](editor)
  protected var maybe_lock: Option[Lock] = controller.requestBodyEditorLock(node)
  //@-<< Fields >>
  //@+<< Initialization >>
  //@+node:gcross.20110507151400.1944: *3* << Initialization >>
  //@-<< Initialization >>
  //@+others
  //@+node:gcross.20110507151400.1959: *3* attachClient
  def attachClient(client: BodyEditor) {
    clients += client
  }
  //@+node:gcross.20110507151400.1943: *3* detachClient
  def detachClient(editor: BodyEditor) {
    clients -= client
    if(clients.isEmpty) {
      maybe_lock.foreach(_.release())
      documents -= ((controller,node))
    }
  }
  //@+node:gcross.20110507151400.1948: *3* getLock
  def getLock: Option[Lock] = maybe_lock
  //@-others
}
//@+node:gcross.20110507151400.1939: ** object NodeDocument
object NodeDocument {
  //@+<< Fields >>
  //@+node:gcross.20110507151400.1941: *3* << Fields >>
  val documents = mutable.Map[(TreeController,Node),WeakReference[NodeDocument]]
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110507151400.1940: *3* apply
  def apply(controller: TreeController, node: Node, client: BodyEditor) =
    documents.get((controller,node)).map(_.get) match {
      case Some(Some(document)) =>
        document.editors += editor
        document
      case _ =>
        val document = new NodeDocument(controller,node,client)
        documents((controller,node)) = document
        document
    }
  //@-others
}
//@-others
//@-leo
