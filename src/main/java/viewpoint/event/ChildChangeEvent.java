//@+leo-ver=5-thin
//@+node:gcross.20110414153139.1464: * @file ChildChangeEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public abstract class ChildChangeEvent extends ParentChangeEvent {

    protected Child child;
    protected int index;

    public ChildChangeEvent(Tree tree, Parent parent, int index, Child child) {
        super(tree,parent);
        this.index = index;
        this.child = child;
    }

    public Child getChild() { return child; }
    public Node getChildNode() { return child.getNode(); }
    public int getChildIndex() { return index; }
    public long getChildTag() { return child.getTag(); }

}
//@-leo
