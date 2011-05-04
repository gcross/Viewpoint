//@+leo-ver=5-thin
//@+node:gcross.20110503191908.2400: * @file BodyEditorLockHolder.java
//@@language Java
package viewpoint.view;

public interface BodyEditorLockHolder {
    String[] flushEdits();
    void notifyLockStolen();
}
//@-leo
