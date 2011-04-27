//@+leo-ver=5-thin
//@+node:gcross.20110422115402.3272: * @file ChildSelectionEvent.java
//@@language Java
package viewpoint.view.event;

import java.util.EventObject;

import viewpoint.model.*;
import viewpoint.view.*;

public class ChildSelectionEvent extends EventObject {
    ChildSelection old_selection, new_selection;

    public ChildSelectionEvent(
        JViewpoint viewpoint,
        ChildSelection old_selection,
        ChildSelection new_selection
    ) {
        super(viewpoint);
        this.old_selection = old_selection;
        this.new_selection = new_selection;
    }

    public ChildSelection getNewSelection() { return new_selection; }
    public ChildSelection getOldSelection() { return old_selection; }
    public JViewpoint getSource() { return getSource(); }
}
//@-leo
