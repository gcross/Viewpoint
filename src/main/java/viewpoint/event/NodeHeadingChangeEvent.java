//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1297: * @file NodeHeadingChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeHeadingChangeEvent extends NodeChangeEvent {

    public NodeHeadingChangeEvent(Object source, Tree tree, Node node) {
        super(source,tree,node);
    }

    public String getHeading() { return node.getHeading(); }

}
//@-leo