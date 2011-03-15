fsc -cp .:commons-lang-2.6.jar Viewpoint.scala && (
    scala -cp .:commons-lang-2.6.jar org.scalatest.tools.Runner -o -s Viewpoint.Testing.ParserSpecification -s Viewpoint.Testing.XMLParserSpecification -s Viewpoint.Testing.NodeSpecification -s Viewpoint.Testing.TreeSpecification;
    scala -cp .:commons-lang-2.6.jar Viewpoint.Testing.ParserSpecification
)