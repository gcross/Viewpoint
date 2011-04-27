//@+leo-ver=5-thin
//@+node:gcross.20110413143734.1426: * @file JViewpoint.scala
//@@language Scala
package viewpoint.view

//@+<< Imports >>
//@+node:gcross.20110413143734.1427: ** << Imports >>
import java.awt.Component
import java.awt.event.ActionEvent
import javax.swing.{AbstractAction,JPanel,JTree,SwingUtilities}
import javax.swing.event.{TreeModelEvent,TreeModelListener,TreeSelectionEvent,TreeSelectionListener}
import javax.swing.tree._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer,Set}

import viewpoint.action._
import viewpoint.model.{Child,Mutator,Node,Parent,Tree}
import viewpoint.util.JavaConversions._
import viewpoint.util.RichInterface._
import viewpoint.view.event._
//@-<< Imports >>

//@+others
//@+node:gcross.20110413143734.1428: ** class JViewpoint
class JViewpoint(val controller: TreeController) extends JPanel {
  //@+<< Imports >>
  //@+node:gcross.20110417144805.2178: *3* << Imports >>
  import JViewpoint._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110413143734.1429: *3* << Fields >>
  protected var current_child_selection: Option[ChildSelection] = None
  private[this] var ignore_selection_events: Boolean = false
  protected val listeners = Set[ChildSelectionListener]()
  protected val tree = controller.tree
  protected val jtree = new JTree(new TreeModelDelegate(tree))
  //@-<< Fields >>
  //@+<< Inner Classes >>
  //@+node:gcross.20110422115402.3301: *3* << Inner Classes >>
  //@+others
  //@+node:gcross.20110422115402.3302: *4* ActionOnSelection
  protected class ActionOnSelection(
    name: String,
    computeAction: ChildSelection => (Mutator => Unit)
  ) extends AbstractAction(name) {
    def actionPerformed(event: ActionEvent) {
      for(selection <- getChildSelection) {
        val action = computeAction(selection)
        controller.enqueue(
          getChildSelection match {
            case Some(current_selection) if current_selection == selection => {
              (mutator: Mutator) =>
                blockChildSelectionEventsDuring({action(mutator)})
                SwingUtilities.invokeLater({() => refreshTreeSelection()})
            }
            case _ => action
          }
        )
      }
    }
  }
  //@+node:gcross.20110422115402.3320: *4* CellRenderer
  protected class CellRenderer extends DefaultTreeCellRenderer {
    override def getTreeCellRendererComponent(
      tree: JTree,
      value: Object,
      selected: Boolean,
      expanded: Boolean,
      leaf: Boolean,
      row: Int,
      hasFocus: Boolean
    ): Component =
      super.getTreeCellRendererComponent(
        tree,
        value.asInstanceOf[Node].getHeading,
        selected,
        expanded,
        leaf,
        row,
        hasFocus
      )
  }
  //@+node:gcross.20110422115402.3319: *4* SelectionListener
  protected class SelectionListener extends TreeSelectionListener {
    def valueChanged(event: TreeSelectionEvent) {
      if(!ignore_selection_events) {
        Option(event.getNewLeadSelectionPath) match {
          case Some(new_selection) => {
            val new_child_selection = new ChildSelection(new_selection)
            val old_child_selection = current_child_selection
            current_child_selection = Some(new_child_selection)
            old_child_selection match {
              case None => {
                fireChildSelectionAdded(new_child_selection)
              }
              case Some(child_selection) if child_selection != new_child_selection => {
                fireChildSelectionChanged(child_selection,new_child_selection)
              }
              case _ =>
            }
          }
          case None => {
            val old_child_selection = current_child_selection
            current_child_selection = None
            for(child_selection <- old_child_selection) {
              fireChildSelectionRemoved(child_selection)
            }
          }
        }
      }
    }
  }
  //@-others
  //@-<< Inner Classes >>
  //@+<< Initialization >>
  //@+node:gcross.20110413143734.1430: *3* << Initialization >>
  jtree.setRootVisible(false)
  jtree.setCellRenderer(new CellRenderer)
  jtree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION)
  jtree.addTreeSelectionListener(new SelectionListener)
  add(jtree)
  //@-<< Initialization >>
  //@+others
  //@+node:gcross.20110422115402.3289: *3* addChildSelectionListener
  def addChildSelectionListener(listener: ChildSelectionListener) {
    listeners += listener
  }
  //@+node:gcross.20110422115402.3300: *3* blockChildSelectionEventsDuring
  def blockChildSelectionEventsDuring[V](thunk: => V) =
    try {
      ignore_selection_events = true
      thunk
    } finally {
      ignore_selection_events = false
    }
  //@+node:gcross.20110422115402.3292: *3* fireChildSelectionAdded
  protected def fireChildSelectionAdded(selection: ChildSelection) {
    val event = new ChildSelectionAddedEvent(this,selection)
    for(listener <- listeners) {
      try {
        listener.childSelectionAdded(event)
      } catch {
        case (e: Exception) =>
      }
    }
  }
  //@+node:gcross.20110422115402.3296: *3* fireChildSelectionChanged
  protected def fireChildSelectionChanged(old_selection: ChildSelection, new_selection: ChildSelection) {
    val event = new ChildSelectionChangedEvent(this,old_selection,new_selection)
    for(listener <- listeners) {
      try {
        listener.childSelectionChanged(event)
      } catch {
        case (e: Exception) =>
      }
    }
  }
  //@+node:gcross.20110422115402.3298: *3* fireChildSelectionRemoved
  protected def fireChildSelectionRemoved(selection: ChildSelection) {
    val event = new ChildSelectionRemovedEvent(this,selection)
    for(listener <- listeners) {
      try {
        listener.childSelectionRemoved(event)
      } catch {
        case (e: Exception) =>
      }
    }
  }
  //@+node:gcross.20110417144805.2201: *3* getMoveDownAction
  lazy val move_down_action =
    new ActionOnSelection(
      "Move Down",
      {selection =>
        val parent: Parent = selection.getAncestor(1)
        val tag: Long = selection.getChild.getTag()
        mutator => mutator.moveChildDownOne(parent,tag)
      }
    )
  def getMoveDownAction: javax.swing.Action = move_down_action
  //@+node:gcross.20110417144805.2203: *3* getMoveUpAction
  lazy val move_up_action =
    new ActionOnSelection(
      "Move Up",
      {selection =>
        val parent: Parent = selection.getAncestor(1)
        val tag: Long = selection.getChild.getTag()
        mutator => mutator.moveChildUpOne(parent,tag)
      }
    )
  def getMoveUpAction: javax.swing.Action = move_up_action
  //@+node:gcross.20110417144805.2180: *3* getChildSelection
  def getChildSelection: Option[ChildSelection] = current_child_selection
  //@+node:gcross.20110422115402.3303: *3* setChildSelection
  def setChildSelection(selection: ChildSelection) {
    jtree.setSelectionPath(selection);
  }
  //@+node:gcross.20110422115402.3323: *3* refreshTreeSelection
  protected def refreshTreeSelection() {
    if(Option(jtree.getSelectionPath).map(new ChildSelection(_)) != current_child_selection) {
      jtree.setSelectionPath(current_child_selection.map(_.asTreePath).orNull)
    }
  }
  //@+node:gcross.20110422115402.3291: *3* removeChildSelectionListener
  def removeChildSelectionListener(listener: ChildSelectionListener) {
    listeners -= listener
  }
  //@-others
}
//@+node:gcross.20110413143734.1431: ** object JViewpoint
object JViewpoint {
  //@+<< TreeModelDelegate >>
  //@+node:gcross.20110413143734.1440: *3* << TreeModelDelegate >>
  class TreeModelDelegate(tree: Tree) extends javax.swing.tree.TreeModel {
    //@+<< Imports >>
    //@+node:gcross.20110413224016.1463: *4* << Imports >>
    import javax.swing.SwingUtilities.invokeLater
    //@-<< Imports >>
    //@+<< Fields >>
    //@+node:gcross.20110413224016.1464: *4* << Fields >>
    val listeners = Set[TreeModelListener]()
    //@-<< Fields >>
    //@+others
    //@+node:gcross.20110413224016.1465: *4* addTreeModelListener
    def addTreeModelListener(listener: TreeModelListener) {
      listeners += listener
    }
    //@+node:gcross.20110413224016.1479: *4* fireNodeChanged
    def fireNodeChanged(parent: Parent, node: Node, index: Int) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesChanged(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110413224016.1477: *4* fireNodeInserted
    def fireNodeInserted(parent: Parent, node: Node, index: Int) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesInserted(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110413224016.1475: *4* fireNodeRemoved
    def fireNodeRemoved(parent: Parent, node: Node, index: Int) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesRemoved(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110413224016.1481: *4* fireStructureChanged
    def fireStructureChanged(parent: Parent) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeStructureChanged(new TreeModelEvent(this,path))
        }
      )
    }
    //@+node:gcross.20110413224016.1471: *4* getChild
    def getChild(parent: AnyRef, index: Int) = {
      parent.asInstanceOf[Parent].getChild(index)
    }
    //@+node:gcross.20110413224016.1472: *4* getChildCount
    def getChildCount(parent: AnyRef): Int =
      parent.asInstanceOf[Parent].getChildCount
    //@+node:gcross.20110413224016.1473: *4* getIndexOfChild
    def getIndexOfChild(parent: AnyRef, child_and_tag: AnyRef) =
      child_and_tag match {
        case (_,tag:Long) =>
          parent.asInstanceOf[Parent].getIndexOfChild(tag)
      }
    //@+node:gcross.20110413224016.1468: *4* getRoot
    def getRoot: AnyRef = tree.getRoot
    //@+node:gcross.20110413143734.1432: *4* invokeLaterForEachPathAndListener
    def invokeLaterForEachPathAndListener(parent: Parent, callback: (TreePath,TreeModelListener) => Unit) {
      callWithAncestorPaths(parent,{path =>
        invokeLater(new Runnable { def run {
          for(listener <- listeners)
            callback(path,listener)
        }})
      })
    }
    //@+node:gcross.20110413224016.1469: *4* isLeaf
    def isLeaf(node: AnyRef): Boolean =
      node match {
        case (parent: Parent) => false
        case (child: Node,_:Int) => child.getChildCount == 0
      }
    //@+node:gcross.20110413224016.1467: *4* removeTreeModelListener
    def removeTreeModelListener(listener: TreeModelListener) {
      listeners -= listener
    }
    //@+node:gcross.20110413224016.1470: *4* valueForPathChanged
    def valueForPathChanged(path: TreePath, newValue: AnyRef) {
      throw new UnsupportedOperationException("This has not been implemented.")
    }
    //@-others
  }
  //@-<< TreeModelDelegate >>
  //@+others
  //@+node:gcross.20110413224016.1462: *3* callWithAncestorPaths
  def callWithAncestorPaths(initial_parent: Parent, callback: TreePath => Unit) {
    def recurse(current_parent: Parent, current_path: List[Parent]) {
      current_parent match {
        case (current_node : Node) =>
          for(parent <- current_node.getParents)
            recurse(parent, parent :: current_path)
        case _ => callback(new TreePath(current_path.toArray[Object]))
      }
    }
    recurse(initial_parent,List(initial_parent))
  }
  //@-others
}
//@-others
//@-leo
