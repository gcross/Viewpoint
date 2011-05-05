//@+leo-ver=5-thin
//@+node:gcross.20110505113029.1723: * @file NodePropertyChangedEvent.java
//@@language Java
package viewpoint.event;

import viewpoint.model.*;

public class NodePropertyChangedEvent extends NodeChangeEvent {

    protected String key;

    public NodePropertyChangedEvent(Tree tree, Node node, String key) {
        super(tree,node);
        this.key = key;
    }

    public String getKey() { return key; }
    public String getValue() { return node.getProperty(key); }

}
//@-leo
