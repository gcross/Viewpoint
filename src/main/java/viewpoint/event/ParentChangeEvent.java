//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1305: * @file ParentChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class ParentChangeEvent extends TreeChangeEvent {

    protected Parent parent;

    public ParentChangeEvent(Tree tree, Parent parent) {
        super(tree);
        this.parent = parent;
    }

    public Parent getParent() { return parent; }

}
//@-leo
