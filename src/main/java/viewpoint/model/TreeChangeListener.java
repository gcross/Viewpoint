//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1307: * @file TreeChangeListener.java
//@@language Java
package viewpoint.model;

import viewpoint.event.*;

public interface TreeChangeListener extends java.util.EventListener {

    public void treeNodeBodyChanged(NodeBodyChangeEvent event);
    public void treeNodeChildInserted(NodeChildInsertedChangeEvent event);
    public void treeNodeChildRemoved(NodeChildRemovedChangeEvent event);
    public void treeNodeHeadingChanged(NodeHeadingChangeEvent event);
    public void treeNodeStructureChanged(NodeStructureChangeEvent event);

}
//@-leo
