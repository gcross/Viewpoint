//@+leo-ver=5-thin
//@+node:gcross.20110417144805.2197: * @file TreeController.scala
//@@language Scala
package viewpoint.view

//@+<< Imports >>
//@+node:gcross.20110417144805.2198: ** << Imports >>
import scala.actors.Actor._
import scala.collection.mutable.Stack

import viewpoint.model.{Mutator,Tree}
import viewpoint.util._
import viewpoint.util.ExceptionUtilities._
import viewpoint.util.RichInterface._
//@-<< Imports >>

//@+others
//@+node:gcross.20110417144805.2199: ** class TreeController
class TreeController(val tree: Tree) {
  //@+<< Imports >>
  //@+node:gcross.20110422115402.3328: *3* << Imports >>
  import TreeController._
  //@-<< Imports >>
  //@+<< Actor >>
  //@+node:gcross.20110422115402.3325: *3* << Actor >>
  protected val runner = actor {
    val undos = new Stack[MutationLog]
    val redos = new Stack[MutationLog]
    loop {
      react {
        case Action(callback) =>
          ignoreAndLogException({
            undos.push(tree.withinTransaction({_.withTemporaryAccess(callback)}).log)
            redos.clear()
          })
        case Undo(bracket) => {
          if(
            try {
              bracket.beforeAction(); true
            } catch {
              case (e: Exception) => false
            }
          ) {
            val undo = undos.pop()
            undo.unwind(tree)
            redos.push(undo)
            ignoreAndLogException({bracket.afterAction()})
          }
        }
        case Redo(bracket) => {
          if(
            try {
              bracket.beforeAction(); true
            } catch {
              case (e: Exception) => false
            }
          ) {
            val redo = redos.pop()
            redo.replay(tree)
            undos.push(redo)
            ignoreAndLogException({bracket.afterAction()})
          }
        }
      }
    }
  }
  //@-<< Actor >>
  //@+others
  //@+node:gcross.20110417223621.1618: *3* enqueue
  def enqueue(callback: Mutator => Unit) { runner ! Action(callback) }
  //@+node:gcross.20110422115402.3331: *3* redo
  def redo(bracket: ActionBracket) { runner ! Redo(bracket) }
  def redo() { runner ! Redo(empty_action_bracket) }
  //@+node:gcross.20110422115402.3329: *3* undo
  def undo(bracket: ActionBracket) { runner ! Undo(bracket) }
  def undo() { runner ! Undo(empty_action_bracket) }
  //@-others
}
//@+node:gcross.20110422115402.3326: ** object TreeController
object TreeController {
  //@+<< Fields >>
  //@+node:gcross.20110427143105.2184: *3* << Fields >>
  val empty_action_bracket = new EmptyActionBracket
  //@-<< Fields >>
  //@+<< Messages >>
  //@+node:gcross.20110422115402.3327: *3* << Messages >>
  protected case class Action(callback: Mutator => Unit)
  protected case class Undo(bracket: ActionBracket)
  protected case class Redo(bracket: ActionBracket)
  //@-<< Messages >>
  //@+others
  //@-others
}
//@-others
//@-leo
