/*
 *    CompileServer.java
 *    Copyright (C) 2001 Javier Lopez (jslopez@alum.mit.edu)
 *
 *    $Revision: 1.3 $
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

import com.sun.tools.javac.Main;
import java.util.StringTokenizer;
import java.util.Vector;

/**
 * CompileServer.java provides an interface between 
 * the command line(or from Emacs) and the javac compiler.
 * @see com.sun.tools.javac.main
 * Calling the compile server instead of doing javac will 
 * avoid the start up time that occur with every incovation
 * of javac.exe
 *
 * Created: Sun Aug 12 21:56:50 2001
 *
 * @author <a href="mailto:jslopez@alum.mit.edu"></a>
 * @version 1.0
 * @since jde-2.2.8beta5
 */
public class CompileServer {
    private static Main compiler;

    /**
     *
     * @param args a <code>String[]</code> with
     * the arguments to passed to compiler.
     * @see com.sun.tools.javac.Main#compiler(String[] args)
     */
    public static int compile(String[] args) {
        if (compiler == null) {
            compiler = new Main();
        } // end of if (compileserver = null)
        return compiler.compile(args);
    }

    /**
     *
     * @param commands a <code>String[]</code> with
     * the arguments to passed to compiler.
     * @see com.sun.tools.javac.Main#compiler(String[] args)
     */
    public static void compile(String commands) {
        //Parsing commands
        StringTokenizer st = new StringTokenizer(commands);
        String[] args = new String[st.countTokens()];
        
        for (int i = 0; st.hasMoreTokens(); i++) { //Fetching the array 
            args[i] = st.nextToken();
        }
        System.out.println(compile(args)); 
    }
}// CompileServer

/*
 * $Log: CompileServer.java,v $
 * Revision 1.3  2001/10/17 04:15:30  paulk
 * Added change log at the end of the file.
 *
 *
 */

// End of CompileServer.java
