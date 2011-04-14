//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1287: * @file TreeChangeEvent.java
//@@language Java
package viewpoint.event;

import java.util.EventObject;
import viewpoint.model.*;

public class TreeChangeEvent extends EventObject {

    public TreeChangeEvent(Tree tree) {
        super(tree);
    }

    public Tree getTree() { return (Tree)source; }

}
//@-leo
