//@+leo-ver=5-thin
//@+node:gcross.20110425140158.1769: * @file JavaConversions.scala
//@@language Scala
package viewpoint.util

object JavaConversions {
  implicit def asJavaRunnable(thunk: () => Unit): Runnable =
    new Runnable { def run() { thunk () } }
}
//@-leo
