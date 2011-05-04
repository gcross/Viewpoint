//@+leo-ver=5-thin
//@+node:gcross.20110422115402.5171: * @file MutatorTransaction.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110422115402.5172: ** << Imports >>
import viewpoint.model.Mutator
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.5173: ** class MutatorTransaction
case class MutatorTransaction[V](val result: V, val log: MutatorLog)
//@+node:gcross.20110422115402.5174: ** object MutatorTransaction
object MutatorTransaction {
  //@+others
  //@+node:gcross.20110422115402.5175: *3* wrapInTransaction
  def wrapInTransaction[V](mutator: Mutator, callback: Mutator => V): MutatorTransaction[V] = {
    val logger = new MutatorLogger(mutator)
    try {
      val result = callback(logger)
      val log = logger.getLog
      MutatorTransaction(result,log)
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
