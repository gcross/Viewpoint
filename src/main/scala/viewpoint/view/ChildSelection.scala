//@+leo-ver=5-thin
//@+node:gcross.20110422115402.3305: * @file ChildSelection.scala
//@@language Scala
package viewpoint.view

//@+<< Imports >>
//@+node:gcross.20110422115402.3306: ** << Imports >>
import javax.swing.tree.TreePath
import org.apache.commons.lang.builder.HashCodeBuilder

import viewpoint.model._
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.3307: ** class ChildSelection
class ChildSelection(
  protected val root: Parent,
  protected val descendents: Array[Child]
) {
  //@+<< Fields >>
  //@+node:gcross.20110422115402.3308: *3* << Fields >>
  protected val number_of_generations: Int = descendents.length+1
  //@-<< Fields >>
  //@+<< Constructors >>
  //@+node:gcross.20110422115402.3309: *3* << Constructors >>
  def this(path: TreePath) =
    this(
      path.getPathComponent(0).asInstanceOf[Parent],
      {
        val descendents = new Array[Child](path.getPathCount()-1)
        for(index <- 1 until path.getPathCount()) {
          descendents(index-1) = path.getPathComponent(index).asInstanceOf[Child]
        }
        descendents
      }
    )
  //@-<< Constructors >>
  //@+others
  //@+node:gcross.20110422115402.3315: *3* asTreePath
  def asTreePath: TreePath = {
    val components = new Array[Object](number_of_generations);
    components(0) = root;
    for(index <- 1 until number_of_generations) {
      components(index) = descendents(index-1);
    }
    new TreePath(components);
  }
  //@+node:gcross.20110422115402.3321: *3* equals
  override def equals(other: Any): Boolean =
    other match {
      case (o: ChildSelection)
        if o.root == root
        && o.descendents.deep == descendents.deep
        => true
      case _ => false
    }
  //@+node:gcross.20110422115402.3310: *3* getAncestor
  def getAncestor(generation: Int): Parent =
    if(generation == number_of_generations-1)
      root
    else
      descendents(number_of_generations-2-generation).getNode
  //@+node:gcross.20110422115402.3312: *3* getChild
  def getChild(generation: Int): Child = descendents(number_of_generations-2-generation)
  def getChild: Child = getChild(0)
  //@+node:gcross.20110422115402.3311: *3* getDescendent
  def getDescendent(generation: Int): Child = descendents(generation)
  //@+node:gcross.20110422115402.3313: *3* getNumberOfGenerations
  def getNumberOfGenerations: Int = number_of_generations
  //@+node:gcross.20110422115402.3314: *3* getRoot
  def getRoot: Parent = root
  //@+node:gcross.20110422115402.3322: *3* hashCode
  override def hashCode: Int = {
    val hash_code = new HashCodeBuilder
    hash_code.append(root)
    hash_code.append(descendents)
    hash_code.toHashCode
  }
  //@-others
}
//@+node:gcross.20110422115402.3316: ** object ChildSelection
object ChildSelection {
  //@+others
  //@+node:gcross.20110422115402.3317: *3* asTreePath
  implicit def asTreePath(selection: ChildSelection): TreePath = selection.asTreePath
  //@-others
}
//@-others
//@-leo
