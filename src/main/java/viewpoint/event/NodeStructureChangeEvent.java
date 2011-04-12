//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1309: * @file NodeStructureChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodeStructureChangeEvent extends NodeChangeEvent {

    public NodeStructureChangeEvent(Object source, Tree tree, Node node) {
        super(source,tree,node);
    }

}
//@-leo
