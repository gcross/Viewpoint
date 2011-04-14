//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1303: * @file NodeChildRemovedChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeChildRemovedChangeEvent extends NodeChildChangeEvent {

    public NodeChildRemovedChangeEvent(Tree tree, Node node, int index) {
        super(tree,node,index);
    }

}
//@-leo
