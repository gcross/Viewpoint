//@+leo-ver=5-thin
//@+node:gcross.20110422115402.1648: * @file NodeStillHasAncestorsException.java
//@@language Java
package viewpoint.model;

public class NodeStillHasAncestorsException extends RuntimeException {
    protected Node node;

    public NodeStillHasAncestorsException(Node node) {
        this.node = node;
    }

    public Node getNode() { return node; }
}
//@-leo
