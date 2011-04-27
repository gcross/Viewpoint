//@+leo-ver=5-thin
//@+node:gcross.20110422115402.3286: * @file ChildSelectionChangedEvent.java
//@@language Java
package viewpoint.view.event;

import java.util.EventObject;

import viewpoint.model.*;
import viewpoint.view.*;

public class ChildSelectionChangedEvent extends ChildSelectionEvent {

    public ChildSelectionChangedEvent(
        JViewpoint viewpoint,
        ChildSelection old_selection,
        ChildSelection new_selection
    ) {
        super(viewpoint,old_selection,new_selection);
    }

}
//@-leo
