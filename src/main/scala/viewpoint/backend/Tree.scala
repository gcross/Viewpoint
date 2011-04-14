//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1352: * @file Tree.scala
//@@language Scala
package viewpoint.backend.crosswhite.model

import viewpoint.{model => interface}

//@+<< Imports >>
//@+node:gcross.20110412144451.1411: ** << Imports >>
import java.io.File
import scala.collection.{Map,Set}
import scala.collection.mutable.HashMap
import scala.ref.WeakReference

import viewpoint.backend.crosswhite.parser.Parser.ParseResult
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
  val nodemap = new HashMap[String,WeakReference[Node]]
  val root = new Root(this)
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110412144451.1416: *3* addNode
  def addNode(node: Node): Node = {
    lookupNode(node.id) match {
      case Some(other_node) =>
        throw NodeIdAlreadyInTree(node.id)
      case None =>
        nodemap(node.id) = new WeakReference(node)
    }
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
  def lookupNode(id: String): Option[Node] = {
    nodemap.get(id).map(_.get) match {
      case None => None
      case Some(None) => {
        nodemap -= id
        None
      }
      case Some(Some(node)) => Some(node)
    }
  }
  //@+node:gcross.20110412144451.1417: *3* lookupOrElseAddNode
  def lookupOrElseAddNode(id: String, default: => Node): Node = {
    lookupNode(id).getOrElse({addNode(default)})
  }
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
    node.children = node.children.map({old_child =>
      val new_child = mergeNode(old_child)
      if(new_child ne old_child) {
        old_child.parents -= node
        new_child.parents += node
      }
      new_child
    })
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
    //@+others
    //@+node:gcross.20110412230649.1473: *4* getRoot
    def getRoot = tree.root.delegate
    //@+node:gcross.20110412230649.1474: *4* lookupNode
    def lookupNode(id: String) = tree.lookupNode(id).map(_.delegate).orNull
    //@-others
  }
  //@-<< Delegate >>
}
//@-others
//@-leo
