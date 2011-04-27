//@+leo-ver=5-thin
//@+node:gcross.20110422115402.3288: * @file ChildSelectionListener.java
//@@language Java
package viewpoint.view.event;

public interface ChildSelectionListener extends java.util.EventListener {

    public void childSelectionAdded(ChildSelectionAddedEvent event);
    public void childSelectionChanged(ChildSelectionChangedEvent event);
    public void childSelectionRemoved(ChildSelectionRemovedEvent event);

}
//@-leo
