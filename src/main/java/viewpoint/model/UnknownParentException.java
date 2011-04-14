//@+leo-ver=5-thin
//@+node:gcross.20110414153139.1452: * @file UnknownParentException.java
//@@language Java
package viewpoint.model;

public class UnknownParentException extends RuntimeException {
    protected Parent parent;
    protected Tree tree;

    public UnknownParentException(Tree tree, Parent parent) {
        super("Attempted to use a parent that was unknown to a tree.");
        this.tree = tree;
        this.parent = parent;
    }

    public Parent getParent() { return parent; }
    public Tree getTree() { return tree; }
}
//@-leo
