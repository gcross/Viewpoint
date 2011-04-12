//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1287: * @file TreeChangeEvent.java
//@@language Java
package viewpoint.event;

import java.util.EventObject;
import viewpoint.model.*;

public class TreeChangeEvent extends EventObject {

    protected transient Tree tree;

    public TreeChangeEvent(Object source, Tree tree) {
        super(tree);
        this.tree = tree;
    }

    public Tree getTree() { return tree; }

}
//@-leo
