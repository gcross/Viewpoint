//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1301: * @file ChildInsertedEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class ChildInsertedEvent extends ChildChangeEvent {

    public ChildInsertedEvent(Tree tree, Parent parent, int index, Child child) {
        super(tree,parent,index,child);
    }

}
//@-leo
