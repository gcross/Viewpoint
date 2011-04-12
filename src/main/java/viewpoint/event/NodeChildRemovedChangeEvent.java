//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1303: * @file NodeChildRemovedChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeChildRemovedChangeEvent extends NodeChildChangeEvent {

    public NodeChildRemovedChangeEvent(Object source, Tree tree, Node node, int index) {
        super(source,tree,node,index);
    }

}
//@-leo
