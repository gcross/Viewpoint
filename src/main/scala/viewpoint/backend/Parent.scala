//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1348: * @file Parent.scala
//@@language Scala
package viewpoint.backend.crosswhite.model

//@+<< Imports >>
//@+node:gcross.20110412144451.1376: ** << Imports >>
import java.io.PrintWriter
import scala.collection.Set
import scala.collection.mutable.{Buffer,HashMap,ListBuffer}

import viewpoint.{model => interface}

import Node.NamedSection
//@-<< Imports >>

//@+others
//@+node:gcross.20110412144451.1377: ** class Parent
class Parent {
  //@+<< Imports >>
  //@+node:gcross.20110412230649.1458: *3* << Imports >>
  import Parent._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110412144451.1364: *3* << Fields >>
  var children : Buffer[Node] = new ListBuffer[Node]
  val delegate = new Delegate(this)
  var properties = new HashMap[String,String]
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110412144451.1373: *3* accept
  def accept(visitor: NodeVisitor) {
    for(child <- children) child.accept(visitor)
  }
  //@+node:gcross.20110412144451.1365: *3* appendChild
  def appendChild(node: Node) {
    node.parents.add(this)
    children += node
  }
  //@+node:gcross.20110412144451.1371: *3* appendYAML
  def appendYAML(indentation: String, builder: StringBuilder) {
    import scala.collection.JavaConversions._
    builder.append(indentation)
    builder.append("properties:")
    builder.append('\n')
    val keys : Array[String] = properties.keySet.toArray
    scala.util.Sorting.quickSort(keys)
    for(key <- keys) {
      builder.append(indentation)
      builder.append("    ")
      builder.append(key)
      builder.append(": ")
      builder.append(properties(key))
      builder.append('\n')
    }

    builder.append(indentation)
    builder.append("children:")
    builder.append('\n')

    val child_indentation = indentation + " "*4
    for(child <- children) {
      builder.append(indentation)
      builder.append("  - ")
      child.appendYAML(child_indentation,builder)
    }
  }
  //@+node:gcross.20110412144451.1368: *3* findChildWithSectionName
  def findChildWithSectionName(section_name: String): Option[Node] = {
    import scala.collection.JavaConversions._
    for {
      child <- children
    } child.heading match {
      case NamedSection(`section_name`) => return Some(child)
      case _ =>
    }
    return None
  }
  //@+node:gcross.20110412144451.1374: *3* gatherChildren
  def gatherChildren: Set[Node] = {
    val visitor = new NodeVisitorWithMemory {
      def visit(node: Node, seen: Boolean) = !seen
    }
    accept(visitor)
    visitor.visited_nodes
  }
  //@+node:gcross.20110412144451.1366: *3* getProperty
  def getProperty(key: String) : Option[String] = properties.get(key)
  //@+node:gcross.20110412144451.1372: *3* replaceChild
  def replaceChild(old_child: Node, new_child: Node) {
    children = children.map({child =>
      if(child == old_child) {
        old_child.parents -= this
        new_child.parents += this
        new_child
      } else child
    })
  }
  //@+node:gcross.20110412144451.1367: *3* setProperty
  def setProperty(key: String, value: String)  { properties(key) = value }
  //@+node:gcross.20110412144451.1370: *3* toYAML
  def toYAML: String = {
    val builder = new StringBuilder
    appendYAML("",builder)
    builder.toString
  }
  //@+node:gcross.20110412144451.1369: *3* writeUnnamedChildrenTo
  def writeUnnamedChildrenTo(level: Int, indentation: String, comment_marker: String, printer: PrintWriter) {
    import scala.collection.JavaConversions._
    for {
      child <- children
    } child.heading match {
      case NamedSection(_) =>
      case _ => child.writeTo(level,indentation,comment_marker,printer)
    }
  }
  //@-others
}
//@+node:gcross.20110412230649.1448: ** object Parent
object Parent {
  //@+<< Delegate >>
  //@+node:gcross.20110412230649.1454: *3* << Delegate >>
  class Delegate(parent: Parent) extends interface.Parent {
    private[this] val children = parent.children
    //@+others
    //@+node:gcross.20110412230649.1455: *4* getChild
    def getChild(index: Int): interface.Node = children(index).delegate
    //@+node:gcross.20110412230649.1456: *4* getChildCount
    def getChildCount: Int = children.size
    //@+node:gcross.20110412230649.1457: *4* getIndexOfChild
    def getIndexOfChild(node: interface.Node): Int =
      node match {
        case Node.Delegate(n) => children.indexOf(n)
        case _ => children.iterator.map(_.id).indexOf(node.getId)
      }
    //@-others
  }

  //@-<< Delegate >>
}
//@-others
//@-leo
