//@+leo-ver=5-thin
//@+node:gcross.20110414153139.1545: * @file TreeChangeAdapter.java
//@@language Java
package viewpoint.event;

public class TreeChangeAdapter implements TreeChangeListener {

    public void treeNodeBodyChanged(NodeBodyChangedEvent event) {}
    public void treeNodeChildInserted(ChildInsertedEvent event) {}
    public void treeNodeChildRemoved(ChildRemovedEvent event) {}
    public void treeNodeHeadingChanged(NodeHeadingChangedEvent event) {}
    public void treeNodePropertyChanged(NodePropertyChangedEvent event) {}
    public void treeNodeStructureChanged(StructureChangedEvent event) {}

}
//@-leo
