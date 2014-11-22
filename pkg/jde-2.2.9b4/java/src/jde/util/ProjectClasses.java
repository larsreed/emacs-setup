/*
 * Copyright (C) 2001 Eric D. Friedman (eric@hfriedman.rdsl.lmi.net)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Created: Tue Aug 14 19:24:02 2001
 */

package jde.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;

/**
 * A class that represents a set of classpath entries for a project.
 * All projects include the entries found in boot.class.path and in
 * the various extension directories.  Classpath entries are shared
 * across projects and so are not loaded/copied more than once.
 *
 * @author Eric D. Friedman
 * @version $Id: ProjectClasses.java,v 1.4 2001/10/17 04:03:37 paulk Exp $
 */

class ProjectClasses {
    private String classPath;
    // the boot classpath is loaded at startup.
    private static ArrayList bootClassPathEntries = new ArrayList();
    static {
        try {
            loadBootClassPathEntries();
        } catch (IOException e) {
            e.printStackTrace(System.err);
        } // end of try-catch
    }

    // take a shallow clone of the boot classpath as the starting
    // point for the project classpath.
    private List classPathEntries = (List)bootClassPathEntries.clone();

    /**
     * Creates a new <code>ProjectClasses</code> instance.
     *
     * @param classPath the project classpath
     * @exception IOException if an error occurs
     */
    ProjectClasses(String classPath) throws IOException {
        this.classPath = classPath;

        String classPathEntry;
        File classPathFile;
        StringTokenizer st = new StringTokenizer(classPath,
                                                 File.pathSeparator);

        while (st.hasMoreTokens()) {
            classPathEntry = st.nextToken();
            classPathFile = new File(classPathEntry);
            if (classPathFile.exists()) {
                classPathEntries.add(ClassPathEntry.instanceForEntry(classPathFile));
            }
        }
    }

    void reloadClasses() throws IOException {
        ClassPathEntry cpe;

        for (Iterator i = classPathEntries.iterator(); i.hasNext();) {
            cpe = (ClassPathEntry)i.next();
            cpe.reload();
        }
    }

    /**
     * returns a list of fully qualified classnames matching an
     * unqualified name in all classpath entries for the project.
     *
     * @param unqualifiedName a <code>String</code> value
     * @return a <code>List</code> value
     * @exception IOException if an error occurs
     */
    List getClassNames(String unqualifiedName) throws IOException {
        List rv = new ArrayList();
        ClassPathEntry cpe;
        for (Iterator i = classPathEntries.iterator(); i.hasNext();) {
            cpe = (ClassPathEntry)i.next();
            rv.addAll(cpe.getClassNames(unqualifiedName));
        }
        return rv;
    }

    public String toString() {
        return classPathEntries.toString();
    }

    public static void main (String[] args) throws Exception {
        System.out.println(new ProjectClasses(System.getProperty("java.class.path")).getClassNames(args[0]));
    }
    
    /**
     * loads the boot classpath entries.
     *
     * @exception IOException if an error occurs
     */
    static void loadBootClassPathEntries() throws IOException {
        StringTokenizer st;
        File file;
        File[] extFiles;
        
        String bootClassPath = System.getProperty("sun.boot.class.path");
        String extDirs = System.getProperty("java.ext.dirs");

        st = new StringTokenizer(bootClassPath,File.pathSeparator);
        while (st.hasMoreTokens()) {
            file = new File(st.nextToken());
            addToBootClassPath(file);
        }
        
        if (extDirs != null) {
            st = new StringTokenizer(extDirs,File.pathSeparator);
			 
            // Iterate through extension directories
            while (st.hasMoreTokens()) {
                extFiles = new File(st.nextToken()).listFiles();
				 
                if (extFiles != null) {
                    // Iterate through files added them to classPath
                    for (int i = 0; i < extFiles.length; i++) {
                        addToBootClassPath(extFiles[i]);
                    }
                }
            }
        }
    }

    static void addToBootClassPath(File file) throws IOException {
        if (file.exists()) {
            ClassPathEntry entry = ClassPathEntry.instanceForEntry(file);
            if (null != entry) {
                bootClassPathEntries.add(new ImmutableClassPathEntry(entry));
            }
        } // end of if (file.exists())
    }
}// ProjectClasses

/*
 * $Log: ProjectClasses.java,v $
 * Revision 1.4  2001/10/17 04:03:37  paulk
 * Cosmetic changes to fit JDE coding style.
 *
 *
 */

// End of ProjectClasses.java
