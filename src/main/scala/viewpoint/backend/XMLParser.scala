//@+leo-ver=5-thin
//@+node:gcross.20110412144451.1359: * @file XMLParser.scala
//@@language Scala
package viewpoint.backend.crosswhite.parser

//@+<< Imports >>
//@+node:gcross.20110412144451.1422: ** << Imports >>
import scala.collection.{mutable,Seq,SortedMap,Traversable}

import viewpoint.backend.crosswhite.model.{Node,Parent,Tree}
//@-<< Imports >>

//@+others
//@+node:gcross.20110408155929.1292: ** object XMLParser
object XMLParser {
  //@+<< Errors >>
  //@+node:gcross.20110412144451.1423: *3* << Errors >>
  class ParseError extends Exception
  object NotALeoFile extends ParseError
  case class TooManyHeadings(id: String,n: Int) extends ParseError
  case class NodeDefinitionAppearsMultipleTimes(id: String) extends ParseError
  case class UnmatchedTNode(id: String) extends ParseError
  //@-<< Errors >>
  //@+others
  //@+node:gcross.20110412144451.1425: *3* parse
  def parse(xml: scala.xml.Node): Tree = {
    val tree = new Tree
    def parseVNode(parent: Parent)(vnode: scala.xml.Node) {
      val id = (vnode \ "@t").text
      val maybe_heading = {
        val heading_nodes = vnode \ "vh"
        heading_nodes.size match {
          case 0 => None
          case 1 => Some(heading_nodes.text)
          case _ => throw TooManyHeadings(id,heading_nodes.size)
        }
      }
      val node = tree.lookupOrElseAddNode(id,{(null,"")})
      for(heading <- maybe_heading) {
        if(!node.isPlaceholder)
          throw NodeDefinitionAppearsMultipleTimes(id)
        else {
          node.heading = heading
          (vnode \ "v").foreach(parseVNode(node))
        }
      }
      parent.appendChild(node)
    }
    (xml \ "vnodes" \ "v").foreach(parseVNode(tree.root))
    for (tnode <- xml \ "tnodes" \ "t"; id = (tnode \ "@tx").text)
      tree.lookupNode(id).getOrElse({throw UnmatchedTNode(id)}).body = tnode.text
    tree
  }
  //@+node:gcross.20110504230408.2333: *3* serialize
  def serialize(parent: Parent): scala.xml.Elem = serialize(parent.childNodes.toSeq : _*)

  def serialize(nodes: Node*): scala.xml.Elem = {
    val Newline = scala.xml.Text("\n")
    val serialized_nodes = mutable.Set[String]()
    var node_bodies = SortedMap[String,String]()

    def serializeNodes(nodes: Iterator[Node]): Iterator[scala.xml.Node] =
      if(nodes.isEmpty)
        Iterator()
      else
        Iterator(Newline) ++
        (for {
          node <- nodes
          child <- Iterator(serializeNode(node),Newline)
         } yield child
        )

    def serializeNode(node: Node): scala.xml.Node =
      <v t={node.id}>{
        if(!serialized_nodes(node.id)) {
          serialized_nodes += node.id
          node_bodies += ((node.id,node.body))
          Iterator(<vh>{node.heading}</vh>) ++ serializeNodes(node.childNodes)
        }
      }</v>

  //@@raw
<leo_file>
<leo_header file_format="2"/>
<vnodes>{serializeNodes(nodes.iterator)}</vnodes>
<tnodes>
{for {
  (id,body) <- node_bodies
  child <- Traversable(<t tx={id}>{body}</t>,Newline)
} yield child
}</tnodes>
</leo_file>
  //@@end_raw
  }
  //@-others
}
//@-others
//@-leo
