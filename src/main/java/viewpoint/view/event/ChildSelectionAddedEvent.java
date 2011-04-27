//@+leo-ver=5-thin
//@+node:gcross.20110422115402.3282: * @file ChildSelectionAddedEvent.java
//@@language Java
package viewpoint.view.event;

import java.util.EventObject;

import viewpoint.model.*;
import viewpoint.view.*;

public class ChildSelectionAddedEvent extends ChildSelectionEvent {

    public ChildSelectionAddedEvent(
        JViewpoint viewpoint,
        ChildSelection new_selection
    ) {
        super(viewpoint,null,new_selection);
    }

}
//@-leo
