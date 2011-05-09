//@+leo-ver=5-thin
//@+node:gcross.20110507151400.1927: * @file BodyEditor.scala
//@@language Scala
package viewpoint.view.editor.rsyntax

//@+<< Imports >>
//@+node:gcross.20110507151400.1928: ** << Imports >>
import javax.swing.JPanel
import org.fife.ui.rsyntaxtextarea.{RSyntaxTextArea,SyntaxConstants}
import org.fife.ui.rtextarea.RTextArea

import viewpoint.view.TreeController
//@-<< Imports >>

//@+others
//@+node:gcross.20110507151400.1929: ** class BodyEditor
class BodyEditor extends JPanel {
  //@+<< Imports >>
  //@+node:gcross.20110507151400.1958: *3* << Imports >>
  import BodyEditor._
  //@-<< Imports >>
  //@+<< Constructors >>
  //@+node:gcross.20110507151400.1950: *3* << Constructors >>
  def this(controller: TreeController, node: Node) = BodyEditor(NodeDocument(controller,node))
  //@-<< Constructors >>
  //@+<< Fields >>
  //@+node:gcross.20110507151400.1949: *3* << Fields >>
  protected var maybe_document = None
  protected val text_area = new RSyntaxTextArea(empty_document)
  //@-<< Fields >>
  //@+<< Initialization >>
  //@+node:gcross.20110507151400.1951: *3* << Initialization >>
  text_area.setEditable(false)
  add(text_area)
  //@-<< Initialization >>
  //@+others
  //@+node:gcross.20110507151400.1955: *3* release
  def release() {
    maybe_document.foreach(_.detachClient(this))
    maybe_document = None
    text_area.setDocument(empty_document)
    text_area.setEditable(false)
  }
  //@+node:gcross.20110507151400.1953: *3* setNode
  def setNode(controller: TreeController, node: Node) {
    setNodeDocument(NodeDocument(controller,node,this))
  }
  //@+node:gcross.20110507151400.1952: *3* setNodeDocument
  def setNodeDocument(document) {
    maybe_document.foreach(_.detachClient(this))
    document.attachClient(this)
    maybe_document = Some(document)
    text_area.setEditable(document.getLock.isDefined)
  }
  //@-others
}
//@+node:gcross.20110507151400.1956: ** object BodyEditor
object BodyEditor {
  //@+<< Fields >>
  //@+node:gcross.20110507151400.1957: *3* << Fields >>
  private[BodyEditor] val empty_document = new RSyntaxDocument(SyntaxConstants.SYNTAX_STYLE_NONE)
  //@-<< Fields >>
  //@+others
  //@-others
}
//@-others
//@-leo
