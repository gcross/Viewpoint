//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1309: * @file StructureChangedEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class StructureChangedEvent extends ParentChangeEvent {

    public StructureChangedEvent(Tree tree, Parent parent) {
        super(tree,parent);
    }

}
//@-leo
