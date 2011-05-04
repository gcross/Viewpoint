//@+leo-ver=5-thin
//@+node:gcross.20110503162356.1682: * @file Mutator.java
//@@language Java
package viewpoint.model;

public interface Mutator extends Librarian {
    public Node createNode(String id, String heading, String body);

    public void forgetNode(Node node);

    public long insertChildInto(Parent parent, Node node, int index);
    public void insertChildInto(Parent parent, Child child, int index);

    public Child removeChildFrom(Parent parent, int index);

    public void setBodyOf(Node node, String body);
    public void setHeadingOf(Node node, String heading);

}
//@-leo
