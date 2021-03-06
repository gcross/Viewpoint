//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1352: * @file Tree.scala
//@@language Scala
package viewpoint.backend.crosswhite.model

//@+<< Imports >>
//@+node:gcross.20110412144451.1411: ** << Imports >>
import java.io.File
import scala.actors.Actor.actor
import scala.collection.{Map,Set}
import scala.collection.mutable.{ArrayBuffer,HashMap,HashSet}

import viewpoint.backend.crosswhite.parser.Parser.ParseResult
import viewpoint.event
import viewpoint.{model => interface}
//@-<< Imports >>

//@+others
//@+node:gcross.20110412144451.1354: ** class Tree
class Tree {
  //@+<< Imports >>
  //@+node:gcross.20110412230649.1471: *3* << Imports >>
  import Tree._
  //@-<< Imports >>
  //@+<< Errors >>
  //@+node:gcross.20110412144451.1412: *3* << Errors >>
  case class NodeIdAlreadyInTree(id: String) extends Exception
  case class AttemptToReplaceNodeThatIsNotStub(old_node: Node) extends Exception
  //@-<< Errors >>
  //@+<< Fields >>
  //@+node:gcross.20110412144451.1413: *3* << Fields >>
  val delegate = new Delegate(this)
  val nodemap = new HashMap[String,Node]
  val root = new Root(this)
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110412144451.1416: *3* addNode
  protected[Tree] def addNode(node: Node): Node = {
    nodemap(node.id) = node
    node
  }
  //@+node:gcross.20110412144451.1415: *3* containsNode
  def containsNode(id: String): Boolean = lookupNode(id).isDefined
  //@+node:gcross.20110412144451.1421: *3* findFileNodes
  def findFileNodes(base: File): (Map[Node,Set[File]],Map[File,Set[Node]]) = {
    import scala.collection.mutable.{HashMap,HashSet,MultiMap,Set,Stack}
    val files_associated_with_node = new HashMap[Node,Set[File]] with MultiMap[Node,File]
    val nodes_associated_with_file = new HashMap[File,Set[Node]] with MultiMap[File,Node]
    root.accept(new NodeVisitorWithMemory {
      var current_directory = base
      var current_directory_root: Node = null
      val directory_stack = new Stack[File]
      val directory_root_stack = new Stack[Node]
      val ignored_nodes = new HashSet[Node]
      def visit(node: Node, seen: Boolean) =
        node.heading match {
          case Node.IgnoreSentinel() => false
          case Node.FileSentinel(filename) => {
            def registerFileNode {
              val filepath = new File(current_directory,filename)
              files_associated_with_node.addBinding(node,filepath)
              nodes_associated_with_file.addBinding(filepath,node)
            }
            if(seen) {
              if(!ignored_nodes.contains(node)) registerFileNode
            } else {
              node.body match {
                case Node.IgnoreSentinel() => ignored_nodes += node
                case _ => registerFileNode
              }
            }
            false
          }
          case Node.PathSentinel(path) => {
            if(seen) {
              false
            } else {
              directory_stack.push(current_directory)
              directory_root_stack.push(current_directory_root)
              current_directory = new File(current_directory,path)
              current_directory_root = node
              true
            }
          }
          case _ => !seen
        }
      override def exit(node: Node) {
        if(node == current_directory_root) {
          current_directory = directory_stack.pop
          current_directory_root = directory_root_stack.pop
        }
      }
    })
    (files_associated_with_node,nodes_associated_with_file)
  }
  //@+node:gcross.20110412144451.1414: *3* lookupNode
  def lookupNode(id: String): Option[Node] = nodemap.get(id)
  //@+node:gcross.20110412144451.1417: *3* lookupOrElseAddNode
  def lookupOrElseAddNode(id: String, default: => (String,String)): Node =
    lookupNode(id).getOrElse({
      val (heading,body) = default
      addNode(new Node(id,heading,body))
    })
  //@+node:gcross.20110412144451.1419: *3* mergeAndReplaceStub
  def mergeAndReplaceStub(old_node: Node, new_unmerged_node: Node) {
    if(!old_node.isStub) throw AttemptToReplaceNodeThatIsNotStub(old_node)
    nodemap -= old_node.id
    old_node.replaceWith(mergeNode(new_unmerged_node))
  }
  //@+node:gcross.20110412144451.1420: *3* mergeNode
  def mergeNode(node: Node): Node = {
    import scala.collection.JavaConversions._
    lookupNode(node.id).map({other_node =>
      if(node == other_node) {
        return node
      } else if(other_node.isPlaceholder) {
        other_node.replaceWith(node)
        nodemap -= other_node.id
      } else if(node === other_node) {
        return other_node
      } else {
        var counter = 0
        var newid = "%s.%s".format(node.id,counter)
        while(containsNode(newid)) {
          counter += 1
          newid = "%s.%s".format(node.id,counter)
        }
        node.id = newid
      }
    })
    for(old_node: Node <- node.childNodes.toSet) {
      val new_node = mergeNode(old_node)
      if(new_node ne old_node) {
        node.replaceChild(old_node,new_node)
      }
    }
    addNode(node)
  }
  //@+node:gcross.20110412144451.1418: *3* mergeParseResultWithStub
  def mergeParseResultWithStub(old_node: Node, result: ParseResult) {
    result match {
      case Left(e) => old_node.body = "@ignore\n@\nError parsing file:\n\n%s\n@c\n%s".format(e,old_node.body)
      case Right(node) => mergeAndReplaceStub(old_node,node)
    }
  }
  //@-others
}
//@+node:gcross.20110412230649.1470: ** object Tree
object Tree {
  //@+<< Delegate >>
  //@+node:gcross.20110412230649.1472: *3* << Delegate >>
  class Delegate(tree: Tree) extends interface.Tree {
    //@+<< Fields >>
    //@+node:gcross.20110413224016.2035: *4* << Fields >>
    val listeners = new HashSet[event.TreeChangeListener]
    //@-<< Fields >>
    //@+others
    //@+node:gcross.20110413224016.2034: *4* addTreeChangeListener
    def addTreeChangeListener(listener: event.TreeChangeListener) {
      listeners += listener
    }
    //@+node:gcross.20110414143741.1454: *4* createNode
    def createNode(id: String, heading: String, body: String): interface.Node =
      if(tree.nodemap.contains(id))
        throw new interface.NodeIdConflictException(id)
      else
        tree.addNode(new Node(id,heading,body))
    //@+node:gcross.20110418122658.2114: *4* fetchChild
    def fetchChild(ichild: interface.Child): Child =
      ichild match {
        case Child.Delegate(child) => child
        case _ => Child(fetchNode(ichild.getNode),ichild.getTag)
      }
    //@+node:gcross.20110414143741.1453: *4* fetchNode
    def fetchNode(inode: interface.Node): Node =
      inode match {
        case Node.Delegate(node) => node
        case _ =>
          tree.lookupNode(inode.getId) match {
            case Some(node) => node
            case None => throw new interface.UnknownNodeException(this,inode)
          }
      }
    //@+node:gcross.20110414143741.1456: *4* fetchParent
    def fetchParent(iparent: interface.Parent): Parent =
      iparent match {
        case Node.Delegate(node) => node
        case Root.Delegate(root) => root
        case (inode : interface.Node) =>
          tree.lookupNode(inode.getId) match {
            case Some(node) => node
            case None => throw new interface.UnknownParentException(this,iparent)
          }
      }
    //@+node:gcross.20110414153139.1460: *4* fireChildInserted
    def fireChildInserted(parent: interface.Parent, index: Int, child: interface.Child) {
      forEachListenerCallIgnoringException(_.treeNodeChildInserted(
        new event.ChildInsertedEvent(this,parent,index,child)
      ))
    }
    //@+node:gcross.20110414153139.1462: *4* fireChildRemoved
    def fireChildRemoved(parent: interface.Parent, index: Int, child: interface.Child) {
      forEachListenerCallIgnoringException(_.treeNodeChildRemoved(
        new event.ChildRemovedEvent(this,parent,index,child)
      ))
    }
    //@+node:gcross.20110414124800.1604: *4* fireNodeBodyChanged
    def fireNodeBodyChanged(node: interface.Node) {
      forEachListenerCallIgnoringException(_.treeNodeBodyChanged(
        new event.NodeBodyChangedEvent(this,node)
      ))
    }
    //@+node:gcross.20110414143741.1444: *4* fireNodeHeadingChanged
    def fireNodeHeadingChanged(node: interface.Node) {
      forEachListenerCallIgnoringException(_.treeNodeHeadingChanged(
        new event.NodeHeadingChangedEvent(this,node)
      ))
    }
    //@+node:gcross.20110505113029.1726: *4* fireNodePropertyChanged
    def fireNodePropertyChanged(node: interface.Node, key: String) {
      forEachListenerCallIgnoringException(_.treeNodePropertyChanged(
        new event.NodePropertyChangedEvent(this,node,key)
      ))
    }
    //@+node:gcross.20110414153139.1465: *4* fireStructureChanged
    def fireStructureChanged(parent: interface.Parent) {
      forEachListenerCallIgnoringException(_.treeNodeStructureChanged(
        new event.StructureChangedEvent(this,parent)
      ))
    }
    //@+node:gcross.20110414153139.2056: *4* forEachListenerCallIgnoringException
    def forEachListenerCallIgnoringException(callback: event.TreeChangeListener => Unit) {
      for(listener <- listeners) {
        try {
          callback(listener)
        } catch {
          case (e: Exception) => {
            print("Ignoring exception in tree change event listener:")
            print(e)
          }
        }
      }
    }
    //@+node:gcross.20110420231854.1621: *4* forgetNode
    def forgetNode(inode: interface.Node) {
      val node = fetchNode(inode)
      if(node.parents.isEmpty && node.children.isEmpty)
        tree.nodemap -= node.id
      else
        throw new interface.NodeStillHasLinksException(inode)
    }
    //@+node:gcross.20110412230649.1473: *4* getRoot
    def getRoot = tree.root

    //@+node:gcross.20110414153139.1454: *4* insertChildInto
    def insertChildInto(iparent: interface.Parent, inode: interface.Node, index: Int): Long = {
      val tag = fetchParent(iparent).insertChild(index,fetchNode(inode))
      fireChildInserted(iparent,index,iparent.getChild(index))
      tag
    }
    def insertChildInto(iparent: interface.Parent, ichild: interface.Child, index: Int) {
      fetchParent(iparent).insertChild(index,fetchChild(ichild))
      fireChildInserted(iparent,index,ichild)
    }
    //@+node:gcross.20110412230649.1474: *4* lookupNode
    def lookupNode(id: String) = tree.lookupNode(id).map(_.delegate).orNull
    //@+node:gcross.20110414153139.1459: *4* removeChildFrom
    def removeChildFrom(iparent: interface.Parent, index: Int): interface.Child = {
      val old_child = fetchParent(iparent).removeChild(index)
      fireChildRemoved(iparent,index,old_child)
      old_child
    }
    //@+node:gcross.20110413224016.2037: *4* removeTreeChangeListener
    def removeTreeChangeListener(listener: event.TreeChangeListener) {
      listeners -= listener
    }
    //@+node:gcross.20110414143741.1450: *4* setBodyOf
    def setBodyOf(inode: interface.Node, body: String) {
      val node = fetchNode(inode)
      val old_properties = node.properties
      node.body = body
      fireNodeBodyChanged(inode)
      for(properties_future <- node.maybe_properties_future)
        actor {
          for(new_properties <- properties_future) {
            for(removed_key <- old_properties.keySet -- new_properties.keySet)
              fireNodePropertyChanged(inode,removed_key)
            for((key,new_value) <- new_properties) 
              old_properties.get(key) match {
                case None => fireNodePropertyChanged(node,key)
                case Some(old_value) if new_value != old_value => fireNodePropertyChanged(node,key)
                case _ =>
              }
          }
        }
    }
    //@+node:gcross.20110414143741.1452: *4* setHeadingOf
    def setHeadingOf(inode: interface.Node, heading: String) {
      fetchNode(inode).heading = heading
      fireNodeHeadingChanged(inode)
    }
    //@-others
  }
  //@-<< Delegate >>
  //@+others
  //@+node:gcross.20110414153139.2335: *3* getTreeDelegate
  implicit def getTreeDelegate(tree: Tree): interface.Tree = tree.delegate
  //@-others
}
//@-others
//@-leo
