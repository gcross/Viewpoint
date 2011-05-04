//@+leo-ver=5-thin
//@+node:gcross.20110422115402.5171: * @file Transaction.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110422115402.5172: ** << Imports >>
import viewpoint.model.Mutator
//@-<< Imports >>

//@+others
//@+node:gcross.20110422115402.5173: ** class Transaction
case class Transaction[V](val result: V, val log: MutationLog)
//@+node:gcross.20110422115402.5174: ** object Transaction
object Transaction {
  //@+others
  //@+node:gcross.20110422115402.5175: *3* wrapInTransaction
  def wrapInTransaction[V](mutator: Mutator, callback: Mutator => V): Transaction[V] = {
    val logger = new LoggedMutator(mutator)
    try {
      val result = callback(logger)
      val log = logger.getLog
      Transaction(result,log)
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
