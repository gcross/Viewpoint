//@+leo-ver=5-thin
//@+node:gcross.20110420231854.1722: * @file NodeIdConflictException.java
//@@language Java
package viewpoint.model;

public class NodeIdConflictException extends RuntimeException {
    protected String id;

    public NodeIdConflictException(String id) {
        this.id = id;
    }

    public String getId() { return id; }
}
//@-leo
