//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1303: * @file ChildRemovedEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class ChildRemovedEvent extends ChildChangeEvent {

    public ChildRemovedEvent(Tree tree, Parent parent, int index, Node child) {
        super(tree,parent,index,child);
    }

}
//@-leo
