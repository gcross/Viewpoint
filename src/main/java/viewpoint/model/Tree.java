//@+leo-ver=5-thin
//@+node:gcross.20110410073838.1275: * @file Tree.java
//@@language Java
package viewpoint.model;

import viewpoint.event.TreeChangeListener;

public interface Tree extends Mutator {
    public void addTreeChangeListener(TreeChangeListener listener);

    public void removeTreeChangeListener(TreeChangeListener listener);
}
//@-leo
