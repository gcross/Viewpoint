//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1305: * @file NodeChildChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeChildChangeEvent extends NodeChangeEvent {

    protected int index;

    public NodeChildChangeEvent(Tree tree, Node node, int index) {
        super(tree,node);
    }

    public int getChildIndex() { return index; }

}
//@-leo
