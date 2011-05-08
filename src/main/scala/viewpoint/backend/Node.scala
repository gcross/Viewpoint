//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1347: * @file Node.scala
//@@language Scala
package viewpoint.backend.crosswhite.model

//@+<< Imports >>
//@+node:gcross.20110412144451.1397: ** << Imports >>
import java.io.{PrintWriter,Writer}
import scala.actors.{Future,Futures}
import scala.collection.{immutable,mutable}
import scala.collection.JavaConversions._
import scala.collection.Map
import scala.collection.mutable.{HashSet}

import viewpoint.{model => interface}
import viewpoint.util.NodeEqualityPolicy
//@-<< Imports >>

//@+others
//@+node:gcross.20110408155929.1288: ** class Node
class Node(var id: String, var heading: String, initial_body: String) extends Parent {
  //@+<< Imports >>
  //@+node:gcross.20110412144451.1383: *3* << Imports >>
  import Node._
  //@-<< Imports >>
  //@+<< Fields >>
  //@+node:gcross.20110412144451.1382: *3* << Fields >>
  override val delegate = new Delegate(this)
  private[crosswhite] var maybe_properties_future: Option[Future[Map[String,String]]] = None
  private var my_body = initial_body
  private var my_properties: () => Map[String,String] = null
  val parents = new HashSet[Parent]
  //@-<< Fields >>
  //@+<< Initialization >>
  //@+node:gcross.20110505113029.1724: *3* << Initialization >>
  refreshProperties()
  //@-<< Initialization >>
  //@+others
  //@+node:gcross.20110412144451.1381: *3* ===
  def ===(other: Node): Boolean = {
    compareAgainst(new HashSet[(Node,Node)],other)
  }
  //@+node:gcross.20110412144451.1389: *3* accept
  override def accept(visitor: NodeVisitor) {
    if(visitor.visit(this)) {
      super.accept(visitor)
      visitor.exit(this)
    }
  }
  //@+node:gcross.20110504230408.1718: *3* ancestors
  def ancestors = new Iterator[Parent] {
    val enqueued_ancestors = mutable.Queue[Parent]()
    val observed_ancestors = mutable.Set[Parent]()

    enqueued_ancestors ++= parents
    observed_ancestors ++= parents

    def hasNext: Boolean = !enqueued_ancestors.isEmpty

    def next: Parent = {
      val ancestor = enqueued_ancestors.dequeue()
      ancestor match {
        case (node: Node) =>
          for (parent <- node.parents)
            if(observed_ancestors.add(parent))
              enqueued_ancestors.enqueue(parent)
        case _ =>
      }
      ancestor
    }
  }
  //@+node:gcross.20110412144451.1378: *3* appendYAML
  override def appendYAML(indentation: String, builder: StringBuilder) {
    import org.apache.commons.lang.StringEscapeUtils
    import scala.collection.JavaConversions._

    builder.append("id: ")
    builder.append(id)
    builder.append('\n')

    builder.append(indentation)
    builder.append("heading: ")
    builder.append(heading)
    builder.append('\n')

    builder.append(indentation)
    builder.append("body: ")
    builder.append('"')
    builder.append(StringEscapeUtils.escapeJavaScript(body))
    builder.append('"')
    builder.append('\n')

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
      builder.append('"')
      builder.append(StringEscapeUtils.escapeJavaScript(properties(key)))
      builder.append('"')
      builder.append('\n')
    }


    super.appendYAML(indentation,builder)
  }
  //@+node:gcross.20110505113029.1718: *3* body
  def body: String = my_body

  def body_=(new_body: String) {
    my_body = new_body
    refreshProperties()
  }
  //@+node:gcross.20110412144451.1387: *3* compareAgainst
  def compareAgainst(examined_nodes: HashSet[(Node,Node)],other: Node): Boolean = {
    import scala.collection.JavaConversions._
    examined_nodes.contains((this,other)) ||
    (
      this.id == other.id &&
      this.heading == other.heading &&
      this.body == other.body &&
      this.children.size == other.children.size &&
      {
        examined_nodes += ((this,other))
        !this.childNodes.zip(other.childNodes).exists({case (this_child,other_child) =>
          !this_child.compareAgainst(examined_nodes,other_child)
        })
      }
    )
  }
  //@+node:gcross.20110412144451.1379: *3* getInheritedProperty
  def getInheritedProperty(key: String) : Option[String] = {
    getProperty(key).orElse({
      for {
        ancestor <- ancestors
        value <- ancestor.getProperty(key)
      } return Some(value)
      None
    })
  }
  //@+node:gcross.20110504230408.1712: *3* getProperty
  override def getProperty(key: String) : Option[String] = properties.get(key)
  //@+node:gcross.20110412144451.1384: *3* isPlaceholder
  def isPlaceholder: Boolean = heading eq null
  //@+node:gcross.20110412144451.1385: *3* isStub
  def isStub: Boolean = children.isEmpty && body.isEmpty
  //@+node:gcross.20110505113029.1720: *3* properties
  protected[crosswhite] def properties = my_properties()
  //@+node:gcross.20110505113029.1717: *3* refreshProperties
  def refreshProperties() {
    Option(body) match {
      case None =>
        maybe_properties_future = None
        my_properties = () => Map.empty
      case Some("") =>
        maybe_properties_future = None
        my_properties = () => Map.empty
      case _ =>
        val future_value = Futures.future({parseProperties(body)})
        maybe_properties_future = Some(future_value)
        my_properties = () => future_value()
    }
  }
  //@+node:gcross.20110412144451.1388: *3* replaceWith
  def replaceWith(other: Node) {
    for(parent <- parents) parent.replaceChild(this,other)
    parents.clear
  }
  //@+node:gcross.20110412144451.1380: *3* writeTo
  def writeTo(writer: Writer) {
    import scala.util.control.Breaks
    val breaks = new Breaks
    import breaks.{break,breakable}

    val printer = new PrintWriter(writer)
    val comment_marker = getInheritedProperty("comment").getOrElse("#")
    val lines = body.lines
    var number_of_first_lines = 0
    breakable {
      while(lines.hasNext) {
        val line = lines.next
        if(!line.startsWith("@first")) break;
        number_of_first_lines += 1
        printer.println(line.substring(7))
      }
    }

    printer.print(comment_marker)
    printer.println("@+leo-ver=5-thin")
    writeTo(1,"",comment_marker,printer,Some(body.lines.drop(number_of_first_lines)))
    printer.print(comment_marker)
    printer.println("@-leo")
  }

  def writeTo(level: Int, indentation: String, comment_marker: String, printer: PrintWriter, maybe_lines: Option[Iterator[String]]=None) {
    import scala.util.control.Breaks

    printer.print(indentation)
    printer.print(comment_marker)
    printer.print("@+node:")
    printer.print(id)
    printer.print(": ")
    printer.print(Node.levelToString(level))
    printer.print(' ')
    printer.println(heading)

    var seen_others = false
    val lines = maybe_lines.getOrElse({body.lines})
    lines.foreach {
      //@+<< Named Section >>
      //@+node:gcross.20110507151400.1913: *4* << Named Section >>
      case SectionRegex(additional_indentation,given_section_name,maybe_section_name) =>
        val section_indentation = indentation + additional_indentation
        printer.print(section_indentation)
        printer.print(comment_marker)
        printer.print("@+")
        printer.println(given_section_name)
        maybe_section_name match {
          case Some(section_name) =>
            findChildWithSectionName(section_name).getOrElse({
              throw NoSectionFound(section_name)
            }).writeTo(level+1,section_indentation,comment_marker,printer)
          case None => {
            if(seen_others) throw OthersAppearsTwice
            seen_others = true
            writeUnnamedChildrenTo(level+1,section_indentation,comment_marker,printer)
          }
        }
        printer.print(section_indentation)
        printer.print(comment_marker)
        printer.print("@-")
        printer.println(given_section_name)
      //@-<< Named Section >>
      //@+<< Directive >>
      //@+node:gcross.20110507151400.1914: *4* << Directive >>
      case line @ Directive() =>
        line match {
          //@+<< Begin comment directive >>
          //@+node:gcross.20110507151400.1915: *5* << Begin comment directive >>
          case BeginCommentDirective(text) =>
            printer.print(indentation)
            printer.print(comment_marker)
            printer.print("@+at")
            printer.println(text)
            while(lines.hasNext && {
              printer.print(indentation)
              printer.print(comment_marker)
              lines.next match {
                case EndCommentDirective(text) => {
                  printer.print("@@c")
                  printer.println(text)
                  false
                }
                case line => {
                  printer.print(' ')
                  printer.println(line)
                  true
                }
              }
            }) {}
          //@-<< Begin comment directive >>
          //@+<< Begin raw directive >>
          //@+node:gcross.20110507151400.1918: *5* << Begin raw directive >>
          case BeginRawDirective =>
            printer.print(indentation)
            printer.print(comment_marker)
            printer.println("@@raw")
            while(lines.hasNext && {
              lines.next match {
                case EndRawDirective => {
                  printer.print(indentation)
                  printer.print(comment_marker)
                  printer.println("@@end_raw")
                  false
                }
                case line => {
                  printer.println(line)
                  true
                }
              }
            }) {}
          //@-<< Begin raw directive >>
          //@+<< Verbatim directive >>
          //@+node:gcross.20110507151400.1916: *5* << Verbatim directive >>
          case VerbatimDirective =>
            printer.print(indentation)
            printer.print(comment_marker)
            printer.println("@verbatim")
            printer.print(indentation)
            printer.println(lines.next)
          //@-<< Verbatim directive >>
          //@+<< Property directive >>
          //@+node:gcross.20110507151400.1917: *5* << Property directive >>
          case PropertyDirective(key,value) =>
            printer.print(indentation)
            if(isPropertyKey(key)) {
              printer.print(comment_marker)
              printer.print('@')
              printer.println(line)
            } else {
              printer.println(line)
            }
          //@-<< Property directive >>
        }
      //@-<< Directive >>
      case line => {
        printer.print(indentation)
        printer.println(line)
      }
    }
    if(!seen_others)
      writeUnnamedChildrenTo(level+1,indentation,comment_marker,printer)
  }
  //@+node:gcross.20110412144451.1386: *3* writeToString
  def writeToString: String = {
    val writer = new java.io.StringWriter
    writeTo(writer)
    writer.toString
  }
  //@-others
}
//@+node:gcross.20110408155929.1285: ** object Node
object Node {
  //@+<< Errors >>
  //@+node:gcross.20110412144451.1394: *3* << Errors >>
  class TangleError extends Exception
  case class NoSectionFound(section_name: String) extends TangleError
  object OthersAppearsTwice extends TangleError
  //@-<< Errors >>
  //@+<< Delegate >>
  //@+node:gcross.20110412230649.1464: *3* << Delegate >>
  case class Delegate(node: Node) extends Parent.Delegate(node) with interface.Node with NodeEqualityPolicy {
    //@+others
    //@+node:gcross.20110412230649.1465: *4* getBody
    def getBody: String = node.body
    //@+node:gcross.20110412230649.1466: *4* getHeading
    def getHeading: String = node.heading
    //@+node:gcross.20110412230649.1467: *4* getId
    def getId: String = node.id
    //@+node:gcross.20110412230649.1468: *4* getParents
    def getParents: java.util.Iterator[interface.Parent] = node.parents.iterator.map(_.delegate)
    //@+node:gcross.20110504230408.1713: *4* getProperty
    def getProperty(key: String) = node.getProperty(key).orNull
    //@-others
  }
  //@-<< Delegate >>
  //@+<< Property Keys >>
  //@+node:gcross.20110412144451.1393: *3* << Property Keys >>
  val property_keys = new HashSet[String]
  property_keys.add("c")
  property_keys.add("comment")
  property_keys.add("language")
  property_keys.add("tabwidth")
  //@-<< Property Keys >>
  //@+<< Sentinels >>
  //@+node:gcross.20110412144451.1392: *3* << Sentinels >>
  val IgnoreSentinel = "(?m).*^@ignore(?:\\s|$)".r

  val FileSentinel = "\\s*@(?:file|thin)\\s*(.*?)\\s*".r

  //@@raw
val NamedSection = "\\s*<<\\s*(.*?)\\s*>>\\s*\\z".r
  //@@end_raw

  val PathSentinel = "\\s*@path\\s*(.*?)\\s*".r

  object SectionRegex {
  //@@raw
  val Regex = "(\\s*)((?:<<\\s*(.*?)\\s*>>|@others)\\s*)\\z".r
  //@@end_raw
    def unapply(line: String): Option[(String,String,Option[String])] =
      line match {
        case Regex(whitespace,given_section_name,section_name) =>
          if(section_name eq null)
            Some((whitespace,"others",None))
          else
            Some((whitespace,given_section_name,Some(section_name)))
        case _ => None
      }
  }
  //@-<< Sentinels >>
  //@+<< Directives >>
  //@+node:gcross.20110505183655.1900: *3* << Directives >>
  //@+others
  //@+node:gcross.20110505183655.1901: *4* BeginCommentDirective
  object BeginCommentDirective {
    val Regex = "^@( .*)?$".r
    def unapply(line: String): Option[String] =
      line match {
        case Regex() => Some("")
        case Regex(text_or_null) => Some(Option(text_or_null).getOrElse(""))
        case _ => None
      }
  }
  //@+node:gcross.20110507151400.1910: *4* BeginRawDirective
  val BeginRawDirective = "@raw"
  //@+node:gcross.20110505183655.1902: *4* Directive
  object Directive {
    def unapply(line: String): Boolean = line(0) == '@'
  }
  //@+node:gcross.20110505183655.1903: *4* EndCommentDirective
  object EndCommentDirective {
    val Regex = "^@c( .*)?$".r
    def unapply(line: String): Option[String] =
      line match {
        case Regex() => Some("")
        case Regex(text_or_null) => Some(Option(text_or_null).getOrElse(""))
        case _ => None
      }
  }
  //@+node:gcross.20110507151400.1912: *4* EndRawDirective
  val EndRawDirective = "@end_raw"
  //@+node:gcross.20110505183655.1904: *4* PropertyDirective
  object PropertyDirective {
    val Regex = "@([^\\s]*) (.*)".r
    val forbidden_keys = immutable.Set("","c")
    def unapply(line: String): Option[(String,String)] =
      line match {
        case Regex(key,value) if ! forbidden_keys(key) => Some((key,value))
        case _ => None
      }
  }
  //@+node:gcross.20110505183655.1905: *4* VerbatimDirective
  val VerbatimDirective = "@verbatim"
  //@-others
  //@-<< Directives >>

  //@+others
  //@+node:gcross.20110414153139.2329: *3* getNodeDelegate
  implicit def getNodeDelegate(node: Node): interface.Node = node.delegate
  //@+node:gcross.20110412144451.1395: *3* isPropertyKey
  def isPropertyKey(key: String): Boolean =
    return property_keys.contains(key)
  //@+node:gcross.20110412144451.1396: *3* levelToString
  def levelToString(level: Int): String = {
    assert(level > 0)
    level match {
      case 1 => "*"
      case 2 => "**"
      case _ => "*%s*".format(level)
    }
  }
  //@+node:gcross.20110505113029.1719: *3* parseProperties
  def parseProperties(body: String): Map[String,String] = {
    val properties = mutable.Map[String,mutable.StringBuilder]()
    body.lines.foreach({
      case PropertyDirective(key,value) =>
        properties.get(key) match {
          case None =>
            properties(key) = new mutable.StringBuilder(value)
          case Some(builder) => {
            builder += '\n'
            builder ++= value
          }
        }
      case _ =>
    })
    properties.mapValues(_.toString)
  }
  //@-others

}
//@+node:gcross.20110412144451.1351: ** trait NodeVisitor
trait NodeVisitor {
  def visit(node: Node): Boolean
  def exit(node: Node) {}
}
//@+node:gcross.20110412144451.1375: ** trait NodeVisitorWithMemory
trait NodeVisitorWithMemory extends NodeVisitor {
  val visited_nodes = new HashSet[Node]
  def visit(node: Node, seen: Boolean): Boolean
  def visit(node: Node): Boolean =
    visit(node,!visited_nodes.add(node))
}
//@-others
//@-leo
