//@+leo-ver=5-thin
//@+node:gcross.20110417144805.2197: * @file TreeController.scala
//@@language Scala
package viewpoint.view

//@+<< Imports >>
//@+node:gcross.20110417144805.2198: ** << Imports >>
import javax.swing.event.{TreeModelEvent,TreeModelListener}
import javax.swing.tree.TreePath
import scala.collection.JavaConversions._
import scala.collection.mutable.{ArrayBuffer,Map,Set,Stack}
import scala.ref.WeakReference

import viewpoint.model.{Librarian,Mutator,Node,Parent,Tree}
import viewpoint.util._
import viewpoint.util.ExceptionUtilities._
import viewpoint.util.JavaConversions._
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110417144805.2199: ** class TreeController
class TreeController(tree: Tree) extends Librarian {
  //@+<< Imports >>
  //@+node:gcross.20110422115402.3328: *3* << Imports >>
  import TreeController._
  //@-<< Imports >>
  //@+<< Inner classes >>
  //@+node:gcross.20110503191908.2404: *3* << Inner classes >>
  //@+others
  //@+node:gcross.20110503191908.2405: *4* Lock
  protected class Lock(initial_node: Node, val holder: BodyEditorLockHolder) extends BodyEditorLock {
    //@+<< Fields >>
    //@+node:gcross.20110503191908.2406: *5* << Fields >>
    protected var maybe_node: Option[Node] = Some(initial_node)
    //@-<< Fields >>
    //@+others
    //@+node:gcross.20110503191908.2422: *5* getNode
    def getNode =
      maybe_node match {
        case None => throw LockReleased
        case Some(node) => node
      }
    //@+node:gcross.20110503191908.2424: *5* getMaybeNode
    def getMaybeNode = maybe_node
    //@+node:gcross.20110503191908.2407: *5* release
    def release() {
      for(node <- maybe_node) lockmap -= node.getId
      maybe_node = None
    }
    //@+node:gcross.20110503191908.2421: *5* valid
    def valid = maybe_node.isDefined
    //@+node:gcross.20110503191908.2408: *5* write
    def write(new_body: String) { write(Array(new_body)) }

    def write(updates: Array[String]) {
      maybe_node match {
        case None => throw LockReleased
        case Some(node) => {
          flushBodyEditors()
          for(update <- updates)
            mutateWithoutFlushing({mutator => mutator.setBodyOf(node,update)})
        }
      }
    }
    //@-others
  }
  //@-others
  //@-<< Inner classes >>
  //@+<< Fields >>
  //@+node:gcross.20110503191908.1841: *3* << Fields >>
  val delegate = new TreeModelDelegate(tree)
  protected val lockmap = Map[String,WeakReference[Lock]]()
  protected val redos = new Stack[MutationLog]
  protected val undos = new Stack[MutationLog]
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110503191908.2413: *3* assignBodyEditorLock
  protected def assignBodyEditorLock(node: Node, holder: BodyEditorLockHolder): BodyEditorLock = {
    val lock = new Lock(node,holder)
    val id = node.getId
    assert(!lockmap.contains(id))
    lockmap(id) = new WeakReference(lock)
    lock
  }
  //@+node:gcross.20110503191908.2415: *3* flushBodyEditors
  def flushBodyEditors() {
    for {
      lock_ref <- lockmap.values
      lock <- lock_ref.get
      node <- lock.getMaybeNode
      edits <- Option(lock.holder.flushEdits())
      edit <- edits
    } mutateWithoutFlushing({mutator => mutator.setBodyOf(node,edit)})
  }
  //@+node:gcross.20110503191908.1844: *3* getRoot
  def getRoot: Parent = tree.getRoot
  //@+node:gcross.20110503191908.2410: *3* lookupBodyEditorLock
  protected def lookupBodyEditorLock(node: Node): Option[Lock] =
    lookupBodyEditorLock(node.getId)

  protected def lookupBodyEditorLock(id: String): Option[Lock] = {
    for {
      lock_ref <- lockmap.get(id)
      lock <- lock_ref.get
      node <- lock.getMaybeNode
    } return Some(lock)

    lockmap -= id
    None
  }
  //@+node:gcross.20110503191908.1845: *3* lookupNode
  def lookupNode(id: String): Node = tree.lookupNode(id)
  //@+node:gcross.20110417223621.1618: *3* mutate
  def mutate[V](mutation: Mutator => V): V = {
    flushBodyEditors()
    mutateWithoutFlushing(mutation)
  }
  //@+node:gcross.20110503191908.2417: *3* mutateWithoutFlushing
  protected def mutateWithoutFlushing[V](mutation: Mutator => V): V = {
    val Transaction(result,log) = tree.withinTransaction({_.withTemporaryAccess(mutation)})
    undos.push(log)
    redos.clear()
    result
  }
  //@+node:gcross.20110422115402.3331: *3* redo
  def redo() {
    val redo = redos.pop()
    redo.replay(tree)
    undos.push(redo)
  }
  //@+node:gcross.20110503191908.2409: *3* requestBodyEditorLock
  def requestBodyEditorLock(id: String, holder: BodyEditorLockHolder): Option[BodyEditorLock] =
    requestBodyEditorLock(lookupNode(id),holder)

  def requestBodyEditorLock(node: Node, holder: BodyEditorLockHolder): Option[BodyEditorLock] =
    lookupBodyEditorLock(node) match {
      case None => Some(assignBodyEditorLock(node,holder))
      case Some(_) => None
    }
  //@+node:gcross.20110503191908.2412: *3* stealBodyEditorLock
  def stealBodyEditorLock(id: String, holder: BodyEditorLockHolder): BodyEditorLock =
    stealBodyEditorLock(lookupNode(id),holder)

  def stealBodyEditorLock(node: Node, holder: BodyEditorLockHolder): BodyEditorLock = {
    for(lock <- lookupBodyEditorLock(node)) {
      lock.release()
      lock.holder.notifyLockStolen()
    }
    assignBodyEditorLock(node,holder)
  }
  //@+node:gcross.20110422115402.3329: *3* undo
  def undo() {
    val undo = undos.pop()
    undo.unwind(tree)
    redos.push(undo)
  }
  //@-others
}
//@+node:gcross.20110422115402.3326: ** object TreeController
object TreeController {
  //@+<< Nested classes >>
  //@+node:gcross.20110503191908.2403: *3* << Nested classes >>
  //@+others
  //@+node:gcross.20110503191908.1825: *4* TreeModelDelegate
  class TreeModelDelegate(tree: Tree) extends javax.swing.tree.TreeModel {
    //@+<< Imports >>
    //@+node:gcross.20110503191908.1826: *5* << Imports >>
    import javax.swing.SwingUtilities.{invokeLater,isEventDispatchThread}
    //@-<< Imports >>
    //@+<< Fields >>
    //@+node:gcross.20110503191908.1827: *5* << Fields >>
    val listeners = Set[TreeModelListener]()
    //@-<< Fields >>
    //@+others
    //@+node:gcross.20110503191908.1828: *5* addTreeModelListener
    def addTreeModelListener(listener: TreeModelListener) {
      listeners += listener
    }
    //@+node:gcross.20110503191908.1829: *5* fireNodeChanged
    def fireNodeChanged(parent: Parent, node: Node, index: Int) {
      invokeInSwingThreadForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesChanged(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110503191908.1830: *5* fireNodeInserted
    def fireNodeInserted(parent: Parent, node: Node, index: Int) {
      invokeInSwingThreadForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesInserted(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110503191908.1831: *5* fireNodeRemoved
    def fireNodeRemoved(parent: Parent, node: Node, index: Int) {
      invokeInSwingThreadForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeNodesRemoved(new TreeModelEvent(this,path,Array(index),Array[Object](node)))
        }
      )
    }
    //@+node:gcross.20110503191908.1832: *5* fireStructureChanged
    def fireStructureChanged(parent: Parent) {
      invokeInSwingThreadForEachPathAndListener(
        parent,
        {(path,listener) =>
          listener.treeStructureChanged(new TreeModelEvent(this,path))
        }
      )
    }
    //@+node:gcross.20110503191908.1833: *5* getChild
    def getChild(parent: AnyRef, index: Int) = {
      parent.asInstanceOf[Parent].getChild(index)
    }
    //@+node:gcross.20110503191908.1834: *5* getChildCount
    def getChildCount(parent: AnyRef): Int =
      parent.asInstanceOf[Parent].getChildCount
    //@+node:gcross.20110503191908.1835: *5* getIndexOfChild
    def getIndexOfChild(parent: AnyRef, child_and_tag: AnyRef) =
      child_and_tag match {
        case (_,tag:Long) =>
          parent.asInstanceOf[Parent].getIndexOfChild(tag)
      }
    //@+node:gcross.20110503191908.1836: *5* getRoot
    def getRoot: AnyRef = tree.getRoot
    //@+node:gcross.20110503191908.1837: *5* invokeInSwingThreadForEachPathAndListener
    def invokeInSwingThreadForEachPathAndListener(parent: Parent, callback: (TreePath,TreeModelListener) => Unit) {
      val go =
        () =>
        callWithAncestorPaths(parent) {
          path =>
          for(listener <- listeners)
            callback(path,listener)
        }

      if(isEventDispatchThread)
        go()
      else(invokeLater(go))
    }
    //@+node:gcross.20110503191908.1838: *5* isLeaf
    def isLeaf(node: AnyRef): Boolean =
      node match {
        case (parent: Parent) => false
        case (child: Node,_:Int) => child.getChildCount == 0
      }
    //@+node:gcross.20110503191908.1839: *5* removeTreeModelListener
    def removeTreeModelListener(listener: TreeModelListener) {
      listeners -= listener
    }
    //@+node:gcross.20110503191908.1840: *5* valueForPathChanged
    def valueForPathChanged(path: TreePath, newValue: AnyRef) {
      throw new UnsupportedOperationException("This has not been implemented.")
    }
    //@-others
  }
  //@-others
  //@-<< Nested classes >>
  //@+<< Exceptions >>
  //@+node:gcross.20110503191908.2420: *3* << Exceptions >>
  object LockReleased extends Exception {
    override val toString = "Attempt to use a lock which has been released."
  }
  //@-<< Exceptions >>
  //@+<< Fields >>
  //@+node:gcross.20110427143105.2184: *3* << Fields >>
  val empty_action_bracket = new EmptyActionBracket
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110503191908.1843: *3* callWithAncestorPaths
  def callWithAncestorPaths(initial_parent: Parent)(callback: TreePath => Unit) {
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
