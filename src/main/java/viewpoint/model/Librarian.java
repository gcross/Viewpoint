//@+leo-ver=5-thin
//@+node:gcross.20110503162356.1680: * @file Librarian.java
//@@language Java
package viewpoint.model;

public interface Librarian {
    public Parent getRoot();

    public Node lookupNode(String id);
}
//@-leo
