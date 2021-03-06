//@+leo-ver=5-thin
//@+node:gcross.20110412230649.1416: * @file Parent.java
//@@language Java
package viewpoint.model;

public interface Parent {
    public Child getChild(int index);
    public int getChildCount();
    public int getIndexOfChild(long tag);
}
//@-leo
