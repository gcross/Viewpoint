//@+leo-ver=5-thin
//@+node:gcross.20110414143741.1449: * @file TreeChangeListener.java
//@@language Java
package viewpoint.event;

public interface TreeChangeListener extends java.util.EventListener {

    public void treeNodeBodyChanged(NodeBodyChangedEvent event);
    public void treeNodeChildInserted(ChildInsertedEvent event);
    public void treeNodeChildRemoved(ChildRemovedEvent event);
    public void treeNodeHeadingChanged(NodeHeadingChangedEvent event);
    public void treeNodePropertyChanged(NodePropertyChangedEvent event);
    public void treeNodeStructureChanged(StructureChangedEvent event);

}
//@-leo
