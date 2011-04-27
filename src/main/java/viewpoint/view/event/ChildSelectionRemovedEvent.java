//@+leo-ver=5-thin
//@+node:gcross.20110422115402.3284: * @file ChildSelectionRemovedEvent.java
//@@language Java
package viewpoint.view.event;

import java.util.EventObject;

import viewpoint.model.*;
import viewpoint.view.*;

public class ChildSelectionRemovedEvent extends ChildSelectionEvent {

    public ChildSelectionRemovedEvent(
        JViewpoint viewpoint,
        ChildSelection old_selection
    ) {
        super(viewpoint,old_selection,null);
    }

}
//@-leo
