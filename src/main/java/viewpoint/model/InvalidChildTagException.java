//@+leo-ver=5-thin
//@+node:gcross.20110418122658.2100: * @file InvalidChildTagException.java
//@@language Java
package viewpoint.model;

public class InvalidChildTagException extends RuntimeException {
    protected Parent parent;
    protected long tag;

    public InvalidChildTagException(Parent parent, long tag) {
        this.parent = parent;
        this.tag = tag;
    }

    public Parent getParent() { return parent; }
    public long getTag() { return tag; }
}
//@-leo
