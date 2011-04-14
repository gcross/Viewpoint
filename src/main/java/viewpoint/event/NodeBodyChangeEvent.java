//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1299: * @file NodeBodyChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeBodyChangeEvent extends NodeChangeEvent {

    public NodeBodyChangeEvent(Tree tree, Node node) {
        super(tree,node);
    }

    public String getBody() { return node.getBody(); }

}
//@-leo
