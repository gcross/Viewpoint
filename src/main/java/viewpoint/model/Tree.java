//@+leo-ver=5-thin
//@+node:gcross.20110410073838.1275: * @file Tree.java
//@@language Java
package viewpoint.model;

import viewpoint.event.TreeChangeListener;

public interface Tree {
    public Parent getRoot();
    public Node lookupNode(String id);
}
//@-leo
