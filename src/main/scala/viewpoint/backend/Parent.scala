//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1348: * @file Parent.scala
//@@language Scala
package viewpoint.backend.crosswhite.model

//@+<< Imports >>
//@+node:gcross.20110412144451.1376: ** << Imports >>
import java.io.PrintWriter
import scala.collection.Set
import scala.collection.mutable.{ArrayBuffer,Buffer,HashMap,HashSet}

import viewpoint.{model => interface}

import Node.NamedSection
//@-<< Imports >>

//@+others
//@+node:gcross.20110412144451.1377: ** class Parent
abstract class Parent {
  //@+<< Imports >>
  //@+node:gcross.20110412230649.1458: *3* << Imports >>
  import Parent._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110412144451.1364: *3* << Fields >>
  private[model] val active_tags = new HashSet[Long]
  private[model] val children = new ArrayBuffer[Child]
  private[model] var next_tag: Long = 0
  private[model] val properties = new HashMap[String,String]
  private[model] val delegate: Delegate
  //@-<< Fields >>
  //@+others
  //@+node:gcross.20110412144451.1373: *3* accept
  def accept(visitor: NodeVisitor) {
    for(node <- childNodes) node.accept(visitor)
  }
  //@+node:gcross.20110412144451.1365: *3* appendChild
  def appendChild(node: Node): Long = {
    node.parents += this
    val tag = nextTag
    children += Child(node,tag)
    active_tags += tag
    tag
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
    for(node <- childNodes) {
      builder.append(indentation)
      builder.append("  - ")
      node.appendYAML(child_indentation,builder)
    }
  }
  //@+node:gcross.20110417144805.2771: *3* childNodes
  def childNodes = children.iterator.map(_.node)
  //@+node:gcross.20110418093501.1594: *3* childTags
  def childTags = children.iterator.map(_.tag)
  //@+node:gcross.20110412144451.1368: *3* findChildWithSectionName
  def findChildWithSectionName(section_name: String): Option[Node] = {
    import scala.collection.JavaConversions._
    for {
      node <- childNodes
    } node.heading match {
      case NamedSection(`section_name`) => return Some(node)
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
  //@+node:gcross.20110417144805.2792: *3* getChild
  def getChild(index: Int): Child = children(index)
  //@+node:gcross.20110417144805.2794: *3* getChildCount
  def getChildCount: Int = children.size
  //@+node:gcross.20110418093501.1596: *3* getChildNode
  def getChildNode(index: Int): Node = getChild(index).node
  //@+node:gcross.20110417144805.2796: *3* getChildTag
  def getChildTag(index: Int): Long = getChild(index).tag
  //@+node:gcross.20110417144805.2798: *3* getIndexOfChild
  def getIndexOfChild(tag: Long): Int = childTags.indexOf(tag)
  //@+node:gcross.20110412144451.1366: *3* getProperty
  def getProperty(key: String) : Option[String] = properties.get(key)
  //@+node:gcross.20110414153139.1467: *3* insertChild
  def insertChild(index: Int, node: Node): Long = {
    node.parents.add(this)
    val tag = nextTag
    children.insert(index,Child(node,tag))
    active_tags += tag
    tag
  }
  def insertChild(index: Int, child: Child) {
    val Child(node,tag) = child
    if(active_tags(tag) || tag >= next_tag) throw new interface.InvalidChildTagException(this,tag)
    node.parents.add(this)
    children.insert(index,Child(node,tag))
  }
  //@+node:gcross.20110417144805.2769: *3* nextTag
  def nextTag: Long = {
    val current_tag = next_tag
    next_tag += 1
    current_tag
  }
  //@+node:gcross.20110417223621.1927: *3* refreshActiveTags
  def refreshActiveTags {
    active_tags.clear
    active_tags ++= childTags
  }
  //@+node:gcross.20110414153139.1469: *3* removeChild
  def removeChild(index: Int): Child = {
    val child@Child(node,tag) = children.remove(index)
    node.parents -= this
    active_tags -= tag
    child
  }
  //@+node:gcross.20110412144451.1372: *3* replaceChild
  def replaceChild(old_node: Node, new_node: Node) {
    old_node.parents -= this
    for(index <- 0 until children.size) {
      val Child(node,tag) = children(index)
      if(node == old_node) {
        children(index) = Child(new_node,tag)
        new_node.parents += this
      }
    }
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
      node <- childNodes
    } node.heading match {
      case NamedSection(_) =>
      case _ => node.writeTo(level,indentation,comment_marker,printer)
    }
  }
  //@-others
}
//@+node:gcross.20110412230649.1448: ** object Parent
object Parent {
  //@+<< Delegate >>
  //@+node:gcross.20110412230649.1454: *3* << Delegate >>
  abstract class Delegate(parent: Parent) extends interface.Parent {
    //@+others
    //@+node:gcross.20110412230649.1455: *4* getChild
    def getChild(index: Int): interface.Child = parent.getChild(index)
    //@+node:gcross.20110412230649.1456: *4* getChildCount
    def getChildCount: Int = parent.getChildCount
    //@+node:gcross.20110418093501.1598: *4* getChildNode
    def getChildNode(index: Int): interface.Node = parent.getChildNode(index)
    //@+node:gcross.20110417144805.2768: *4* getChildTag
    def getChildTag(index: Int): Long = parent.getChildTag(index)
    //@+node:gcross.20110412230649.1457: *4* getIndexOfChild
    def getIndexOfChild(tag: Long): Int = parent.getIndexOfChild(tag)
    //@-others
  }
  //@-<< Delegate >>
  //@+others
  //@+node:gcross.20110418122658.2110: *3* getParentDelegate
  implicit def getParentDelegate(parent: Parent): interface.Parent = parent.delegate
  //@-others
}
//@-others
//@-leo
