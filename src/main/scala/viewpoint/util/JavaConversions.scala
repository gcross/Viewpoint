//@+leo-ver=5-thin
//@+node:gcross.20110414153139.2320: * @file JavaConversions.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110414153139.2321: ** << Imports >>
import java.util.concurrent.Callable
//@-<< Imports >>

//@+others
//@+node:gcross.20110414153139.2322: ** object JavaConversions
object JavaConversions {
  //@+others
  //@+node:gcross.20110414153139.2336: *3* asCallable
  implicit def asCallable[T](thunk: () => T): Callable[T] =
    new Callable[T] { def call = thunk ()}
  //@-others
}
//@-others
//@-leo
