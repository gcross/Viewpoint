//@+leo-ver=5-thin
//@+node:gcross.20110422115402.1648: * @file NodeStillHasLinksException.java
//@@language Java
package viewpoint.model;

public class NodeStillHasLinksException extends RuntimeException {
    protected Node node;

    public NodeStillHasLinksException(Node node) {
        this.node = node;
    }

    public Node getNode() { return node; }
}
//@-leo
