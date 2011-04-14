//@+leo-ver=5-thin
//@+node:gcross.20110414143741.1457: * @file UnknownNodeException.java
//@@language Java
package viewpoint.model;

public class UnknownNodeException extends RuntimeException {
    protected Node node;
    protected Tree tree;

    public UnknownNodeException(Tree tree, Node node) {
        super(String.format("Attempted to use a node (id='%s', heading='%s') that was unknown to a tree.",node.getId(),node.getHeading()));
        this.tree = tree;
        this.node = node;
    }

    public Node getNode() { return node; }
    public Tree getTree() { return tree; }
}
//@-leo
