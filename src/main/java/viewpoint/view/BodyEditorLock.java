//@+leo-ver=5-thin
//@+node:gcross.20110503191908.2398: * @file BodyEditorLock.java
//@@language Java
package viewpoint.view;

import viewpoint.model.Node;

public interface BodyEditorLock {
    Node getNode();
    void release();
    void valid();
    void write(String update);
    void write(String[] updates);
}
//@-leo
