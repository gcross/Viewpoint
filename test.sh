fsc -cp .:commons-lang-2.6.jar Viewpoint.scala && (
    scala -cp .:commons-lang-2.6.jar org.scalatest.tools.Runner -o -s Viewpoint.Testing.ParserSpecification;
    scala -cp .:commons-lang-2.6.jar Viewpoint.Testing.ParserSpecification
)