//@+leo-ver=5-thin
//@+node:gcross.20110410082044.1281: * @file Node.java
//@@language Java
package viewpoint.model;

import java.util.Iterator;

public interface Node extends Parent {
    public String getBody();
    public String getHeading();
    public String getId();
    public Iterator<Parent> getParents();
    public String getProperty(String key);
}
//@-leo
