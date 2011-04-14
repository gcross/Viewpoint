//@+leo-ver=5-thin
//@+node:gcross.20110414153139.1464: * @file ChildChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public abstract class ChildChangeEvent extends ParentChangeEvent {

    protected Node child;
    protected int index;

    public ChildChangeEvent(Tree tree, Parent parent, int index, Node child) {
        super(tree,parent);
        this.index = index;
        this.child = child;
    }

    public Node getChild() { return child; }
    public int getChildIndex() { return index; }

}
//@-leo
