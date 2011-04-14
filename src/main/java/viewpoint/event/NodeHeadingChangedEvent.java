//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1297: * @file NodeHeadingChangedEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeHeadingChangedEvent extends NodeChangeEvent {

    public NodeHeadingChangedEvent(Tree tree, Node node) {
        super(tree,node);
    }

    public String getHeading() { return node.getHeading(); }

}
//@-leo
