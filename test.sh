scalac Viewpoint.scala && (
    scala org.scalatest.tools.Runner -p . -o -s Viewpoint.Testing.ParserSpecification;
    scala -cp . Viewpoint.Testing.ParserSpecification
)