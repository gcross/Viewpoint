//@+leo-ver=5-thin
//@+node:gcross.20110427143105.2185: * @file ExceptionUtilities.scala
//@@language Scala
package viewpoint.util

//@+<< Imports >>
//@+node:gcross.20110427143105.2186: ** << Imports >>
//@-<< Imports >>

//@+others
//@+node:gcross.20110427143105.2187: ** object ExceptionUtilities
object ExceptionUtilities {
  //@+others
  //@+node:gcross.20110427143105.2188: *3* ignoreAndLogException
  def ignoreAndLogException(thunk: => Unit) {
    try { thunk } catch { case (e: Exception) => }
  }
  //@+node:gcross.20110427143105.2190: *3* wrapPartialFunctionWithIgnoreAndLogException
  def wrapPartialFunctionWithIgnoreAndLogException[A](f: PartialFunction[A,Unit]) =
    new PartialFunction[A,Unit] {
      def isDefinedAt(x: A): Boolean = f.isDefinedAt(x)
      def apply(x: A) { ignoreAndLogException(f(x)) }
    }
  //@-others
}
//@-others
//@-leo
