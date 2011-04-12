//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1301: * @file NodeChildInsertedChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeChildInsertedChangeEvent extends NodeChildChangeEvent {

    public NodeChildInsertedChangeEvent(Object source, Tree tree, Node node, int index) {
        super(source,tree,node,index);
    }

}
//@-leo
