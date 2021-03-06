//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1291: * @file NodeChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeChangeEvent extends TreeChangeEvent {

    protected Node node;

    public NodeChangeEvent(Tree tree, Node node) {
        super(tree);
        this.node = node;
    }

    public String getId() { return node.getId(); }
    public Node getNode() { return node; }

}
//@-leo
