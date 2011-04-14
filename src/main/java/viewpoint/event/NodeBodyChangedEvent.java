//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1299: * @file NodeBodyChangedEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeBodyChangedEvent extends NodeChangeEvent {

    public NodeBodyChangedEvent(Tree tree, Node node) {
        super(tree,node);
    }

    public String getBody() { return node.getBody(); }

}
//@-leo
