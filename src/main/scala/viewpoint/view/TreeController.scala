//@+leo-ver=5-thin
//@+node:gcross.20110417144805.2197: * @file TreeController.scala
//@@language Scala
package viewpoint.view

//@+<< Imports >>
//@+node:gcross.20110417144805.2198: ** << Imports >>
import javax.swing.event.{TreeModelEvent,TreeModelListener}
import javax.swing.tree.TreePath
import scala.actors.Actor._
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer,Set,Stack}

import viewpoint.model.{Mutator,Node,Parent,Tree}
import viewpoint.util._
import viewpoint.util.ExceptionUtilities._
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110417144805.2199: ** class TreeController
class TreeController(tree: Tree) {
  //@+<< Imports >>
  //@+node:gcross.20110422115402.3328: *3* << Imports >>
  import TreeController._
  //@-<< Imports >>
  //@+<< Actor >>
  //@+node:gcross.20110422115402.3325: *3* << Actor >>
  protected val runner = actor {
    val undos = new Stack[MutationLog]
    val redos = new Stack[MutationLog]
    loop {
      react {
        case Action(callback) =>
          ignoreAndLogException({
            undos.push(tree.withinTransaction({_.withTemporaryAccess(callback)}).log)
            redos.clear()
          })
        case Undo(bracket) => {
          if(
            try {
              bracket.beforeAction(); true
            } catch {
              case (e: Exception) => false
            }
          ) {
            val undo = undos.pop()
            undo.unwind(tree)
            redos.push(undo)
            ignoreAndLogException({bracket.afterAction()})
          }
        }
        case Redo(bracket) => {
          if(
            try {
              bracket.beforeAction(); true
            } catch {
              case (e: Exception) => false
            }
          ) {
            val redo = redos.pop()
            redo.replay(tree)
            undos.push(redo)
            ignoreAndLogException({bracket.afterAction()})
          }
        }
      }
    }
  }
  //@-<< Actor >>
  //@+<< Fields >>
  //@+node:gcross.20110503191908.1841: *3* << Fields >>
  val delegate = new TreeModelDelegate(tree)
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110417223621.1618: *3* enqueue
  def enqueue(callback: Mutator => Unit) { runner ! Action(callback) }
  //@+node:gcross.20110422115402.3331: *3* redo
  def redo(bracket: ActionBracket) { runner ! Redo(bracket) }
  def redo() { runner ! Redo(empty_action_bracket) }
  //@+node:gcross.20110422115402.3329: *3* undo
  def undo(bracket: ActionBracket) { runner ! Undo(bracket) }
  def undo() { runner ! Undo(empty_action_bracket) }
  //@-others
}
//@+node:gcross.20110422115402.3326: ** object TreeController
object TreeController {
  //@+<< Fields >>
  //@+node:gcross.20110427143105.2184: *3* << Fields >>
  val empty_action_bracket = new EmptyActionBracket
  //@-<< Fields >>
  //@+<< Messages >>
  //@+node:gcross.20110422115402.3327: *3* << Messages >>
  protected case class Action(callback: Mutator => Unit)
  protected case class Undo(bracket: ActionBracket)
  protected case class Redo(bracket: ActionBracket)
  //@-<< Messages >>
  //@+<< TreeModelDelegate >>
  //@+node:gcross.20110503191908.1825: *3* << TreeModelDelegate >>
  class TreeModelDelegate(tree: Tree) extends javax.swing.tree.TreeModel {
    //@+<< Imports >>
    //@+node:gcross.20110503191908.1826: *4* << Imports >>
    import javax.swing.SwingUtilities.invokeLater
    //@-<< Imports >>
    //@+<< Fields >>
    //@+node:gcross.20110503191908.1827: *4* << Fields >>
    val listeners = Set[TreeModelListener]()
    //@-<< Fields >>
    //@+others
    //@+node:gcross.20110503191908.1828: *4* addTreeModelListener
    def addTreeModelListener(listener: TreeModelListener) {
      listeners += listener
    }
    //@+node:gcross.20110503191908.1829: *4* fireNodeChanged
    def fireNodeChanged(parent: Parent, node: Node, index: Int) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesChanged(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110503191908.1830: *4* fireNodeInserted
    def fireNodeInserted(parent: Parent, node: Node, index: Int) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesInserted(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110503191908.1831: *4* fireNodeRemoved
    def fireNodeRemoved(parent: Parent, node: Node, index: Int) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesRemoved(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110503191908.1832: *4* fireStructureChanged
    def fireStructureChanged(parent: Parent) {
      invokeLaterForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeStructureChanged(new TreeModelEvent(this,path))
        }
      )
    }
    //@+node:gcross.20110503191908.1833: *4* getChild
    def getChild(parent: AnyRef, index: Int) = {
      parent.asInstanceOf[Parent].getChild(index)
    }
    //@+node:gcross.20110503191908.1834: *4* getChildCount
    def getChildCount(parent: AnyRef): Int =
      parent.asInstanceOf[Parent].getChildCount
    //@+node:gcross.20110503191908.1835: *4* getIndexOfChild
    def getIndexOfChild(parent: AnyRef, child_and_tag: AnyRef) =
      child_and_tag match {
        case (_,tag:Long) =>
          parent.asInstanceOf[Parent].getIndexOfChild(tag)
      }
    //@+node:gcross.20110503191908.1836: *4* getRoot
    def getRoot: AnyRef = tree.getRoot
    //@+node:gcross.20110503191908.1837: *4* invokeLaterForEachPathAndListener
    def invokeLaterForEachPathAndListener(parent: Parent, callback: (TreePath,TreeModelListener) => Unit) {
      callWithAncestorPaths(parent,{path =>
        invokeLater(new Runnable { def run {
          for(listener <- listeners)
            callback(path,listener)
        }})
      })
    }
    //@+node:gcross.20110503191908.1838: *4* isLeaf
    def isLeaf(node: AnyRef): Boolean =
      node match {
        case (parent: Parent) => false
        case (child: Node,_:Int) => child.getChildCount == 0
      }
    //@+node:gcross.20110503191908.1839: *4* removeTreeModelListener
    def removeTreeModelListener(listener: TreeModelListener) {
      listeners -= listener
    }
    //@+node:gcross.20110503191908.1840: *4* valueForPathChanged
    def valueForPathChanged(path: TreePath, newValue: AnyRef) {
      throw new UnsupportedOperationException("This has not been implemented.")
    }
    //@-others
  }
  //@-<< TreeModelDelegate >>
  //@+others
  //@+node:gcross.20110503191908.1843: *3* callWithAncestorPaths
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
