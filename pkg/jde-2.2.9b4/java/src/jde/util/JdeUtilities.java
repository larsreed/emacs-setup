/*
 *    JdeUtilities.java
 *    $Revision: 1.8 $
 *
 *    Copyright (C) 1999-2001 Len Trigg (trigg@cs.waikato.ac.nz)
 *    Copyright (C) 1999-2001 Paul Kinnucan (paulk@mathworks.com)
 *
 *    This program is free software; you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation; either version 2 of the License, or
 *    (at your option) any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program; if not, write to the Free Software
 *    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

package jde.util;

import java.io.*;
import java.util.*;

/**
 * This class provides various utility methods.
 *
 * @author Len Trigg (trigg@cs.waikato.ac.nz) 
 * @author Paul Kinnucan (paulk@mathworks.com)
 * @author Matt Conway (Matt_Conway@i2.com )
 * @author Eric D. Friedman (eric@hfriedman.rdsl.lmi.net)
 * @version $Revision: 1.8 $
 */
public class JdeUtilities {

    /** A cache of the items that are important across projects,
     * indexed by the project name */
    private static Map projectCache = new HashMap();

    /** The current project so that callers need not pass in the
        project name every time.  This is convenient, but not
        threadsafe.  If we need thread saftey, we can go change all
        the emacs lisp callers to always pass in the project name */
    private static String currentProjectName = "default";
	
    // Have a default one just in case
    static {
        try {
            ProjectClasses defaultProject =
                new ProjectClasses(System.getProperty("java.class.path"));
            projectCache.put(currentProjectName,defaultProject);
        } catch (IOException e) {
            e.printStackTrace(System.err);
        } // end of try-catch
    }

    /*************************************************************************
     * Constants
     *************************************************************************/
    public static final String NIL = "nil";
    public static final String T = "t";
    public static final String LIST = "list";
    public static final String START_PAREN = "(";
    public static final String END_PAREN = ")";
    public static final String DOUBLE_QUOTE = "\"";
    public static final String SPACE = " ";
    public static final String START_LIST;
    static {
        StringBuffer sb = new StringBuffer (10);
        sb.append(START_PAREN);
        sb.append(LIST);
        sb.append(SPACE);
        START_LIST = sb.toString();
    }

    /**
     * Jde should call this everytime the project changes, or if the
     * classpath needs to be updated.
     *
     * @param projectName a <code>String</code> value
     * @param projectClassPath a <code>String</code> value
     */
    public static void setProjectValues(String projectName,
                                        String projectClassPath) {
        try {
            currentProjectName = projectName;
            ProjectClasses pc = new ProjectClasses(projectClassPath);
            projectCache.put(projectName, pc);
        } catch (IOException e) {
            e.printStackTrace(System.err);
        } // end of try-catch
    }

    /* Convenience to get current project's name */
    public static String getCurrentProjectName() {
        return currentProjectName;
    }

    public static void classExists( String fqn ) {
        try {
            Class.forName( fqn );
            System.out.println( T );
        } catch (NoClassDefFoundError ex1) {
            System.out.println( NIL );
        } catch(Exception ex2) {
            System.out.println( NIL );
        }//catch
    }//met

    /**
     * Forces a reload of the non-system classes used by the current
     * project.
     *
     * @param force ignored, but preserved as part of legacy API.
     */
    public static void buildClassList(boolean force) {
        ProjectClasses pc = null;
        pc = (ProjectClasses)projectCache.get(getCurrentProjectName());
        
        try {
            pc.reloadClasses();
        } catch (IOException e) {
            e.printStackTrace(System.err);
        } // end of try-catch
    }
  
    /**
     * Looks up an unqualified class name in the class path to find possible
     * fully qualified matches.  Given `List,' this will find
     * `java.util.List' and `java.awt.List'
     *
     * @param className a value of type 'String'
     */
    public static void getQualifiedName(String className) {
        ProjectClasses pc = null;
        StringBuffer result = null;

        try {
            pc = (ProjectClasses)projectCache.get(currentProjectName);
            result = new StringBuffer(START_PAREN);
            result.append(LIST);

            for (Iterator i = pc.getClassNames(className).iterator();
                 i.hasNext();) {
                result.append(SPACE);
                result.append(DOUBLE_QUOTE);
                result.append(i.next().toString());
                result.append(DOUBLE_QUOTE);
            }
            result.append(END_PAREN);
            System.out.println(result.toString());
            System.out.flush();
        } catch (IOException e) {
            e.printStackTrace(System.err);
        } // end of try-catch
    }

    public static void getJavaVersion() {
        StringBuffer sb = new StringBuffer(30);
        sb.append(DOUBLE_QUOTE);
        sb.append(System.getProperty("java.version"));
        sb.append(DOUBLE_QUOTE);
        System.out.println(sb);
        System.out.flush();
    }
} // JdeUtilities

/*
 * $Log: JdeUtilities.java,v $
 * Revision 1.8  2001/09/13 02:35:40  eric
 * (comment copied from branch)
 * java 2-only implementation of JdeUtilities functions.  This uses a lazy
 * map based lookup scheme instead of a linear search of class lists to find
 * fully qualified class names.  It also implements a flyweight/singleton
 * approach to sharing classpath entries that appear in multiple projects.
 * Finally, classpath entries that are part of the `system' classes are
 * wrapped in an immutable decorator which balks are reloading them.
 * This eliminates the overhead of rescanning classes which cannot change
 * over the lifetime of a single bsh process.
 *
 * Revision 1.6.2.2  2001/09/13 02:32:06  eric
 * merge HEAD => branch, in anticipation of merging branch => HEAD
 *
 */

// End of JdeUtilities.java
