//@+leo-ver=5-thin
//@+node:gcross.20110410073838.1275: * @file Tree.java
//@@language Java
package viewpoint.model;

import java.util.concurrent.Callable;
import viewpoint.event.TreeChangeListener;

public interface Tree {
    public void addTreeChangeListener(TreeChangeListener listener);

    public Node createNode(String heading, String body);

    public Parent getRoot();

    public void insertChildInto(Parent parent, Node child, int index);

    public Node lookupNode(String id);

    public Node removeChildFrom(Parent parent, int index);

    public void setBodyOf(Node node, String body);
    public void setHeadingOf(Node node, String heading);

    public void removeTreeChangeListener(TreeChangeListener listener);

    public <V> V withinTransaction(Callable<V> transaction);
}
//@-leo
