//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1281: * @file Node.java
//@@language Java
package viewpoint.model;

public interface Node {
    public String getBody();
    public Node getChild(int index);
    public int getChildCount();
    public int getIndexOfChild(Node node);
    public String getHeading();
    public Node[] getParents();
    public String getId();

    public void insertChild(int index, Node node);
    public void removeChild(int index, Node node);

    public void setBody();
    public void setHeading();
}
//@-leo
