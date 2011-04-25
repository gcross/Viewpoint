//@+leo-ver=5-thin
//@+node:gcross.20110422115402.5171: * @file TreeTransaction.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110422115402.5172: ** << Imports >>
import viewpoint.model.Tree
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.5173: ** class TreeTransaction
case class TreeTransaction[V](val result: V, val log: TreeLog)
//@+node:gcross.20110422115402.5174: ** object TreeTransaction
object TreeTransaction {
  //@+others
  //@+node:gcross.20110422115402.5175: *3* wrapInTransaction
  def wrapInTransaction[V](tree: Tree, callback: Tree => V): TreeTransaction[V] = {
    val logger = new TreeLogger(tree)
    try {
      val result = callback(logger)
      val log = logger.getLog
      TreeTransaction(result,log)
    } catch {
      case (e: Exception) => {
        logger.unwind()
        throw e
      }
    }
  }
  //@-others
}
//@-others
//@-leo
