/*
 * Copyright (c) Paul Kinnucan 1998, 1999, 2000, 2001. All Rights Reserved.
 *
 * $Revision: 1.8 $ 
 * $Date: 2002/06/03 18:12:27 $ 
 *
 * MethodOverrideFactory is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2, or (at
 * your option) any later version.
 *
 * InterfaceFactory is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * To obtain a copy of the GNU General Public License write to the
 * Free Software Foundation, Inc.,  59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.  
 */

package jde.wizards;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.io.PrintWriter;
import java.util.Hashtable;
import java.util.Vector;
import java.util.Enumeration;

import jde.util.DynamicClassLoader;

/**
 * Defines a factory for creating an override of a method defined in a 
 * superclass.
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.8 $
 */

public class MethodOverrideFactory extends MethodFactory
{

  /** The factory. */
  static MethodOverrideFactory overrideFactory;

  Vector candidates = new Vector();

  String baseClassName;

  String methodName;
  
  public MethodOverrideFactory() {}

  /** 
   * Creates a MethodOverrideFactory that uses the specified NameFactory
   * for generating parameter names 
   *
   * @param factory Factory for generating parameter names
   */
  public MethodOverrideFactory(NameFactory factory)
  {
    super(factory);
  }

  public Vector getMethods(Class cl, String methodName) {

    Method [] methods = cl.getDeclaredMethods();
   
    Vector m = new Vector();

    for (int i = 0; i < methods.length; ++i) 
      if (methods[i].getName().equals(methodName)) 
	m.addElement(methods[i]);  
    
    return m;

  }


  public static void getCandidateSignatures(String baseClassName, String methodName) {

    if (overrideFactory == null) 
      overrideFactory = new MethodOverrideFactory();
    else
      overrideFactory.flush();

    overrideFactory.baseClassName = baseClassName;
    overrideFactory.methodName = methodName;

    try {

      DynamicClassLoader dcl = new DynamicClassLoader();
      Class baseClass = dcl.loadClass(baseClassName);

      while (baseClass != null) {

	Vector methods = overrideFactory.getMethods(baseClass, methodName);

	int n = methods.size();

	if (n > 0) {
	  for (int i = 0; i < n; ++i) {
	    Method m = (Method) methods.elementAt(i);
	    Signature s = new Signature(m, overrideFactory);
	    boolean containsSignature = false;
	    for (int j = 0; j < overrideFactory.candidates.size(); ++j) 
	      if (s.equals(overrideFactory.candidates.elementAt(j))) {
		containsSignature = true;
		break;
	      }
	    if (! containsSignature) 
	      overrideFactory.candidates.addElement(s);
	  }
	}
	baseClass = baseClass.getSuperclass();
      }
      
      int n = overrideFactory.candidates.size();

      if (n > 0) {
	String res = "(list ";
	for (int i = 0; i < n; ++i) {
	  Signature s = (Signature) overrideFactory.candidates.elementAt(i);
	  String p = s.getParameters(s.getMethod().getParameterTypes());
	  res = res + "\"" + methodName + "(" + p +  ")\" ";
	}
	res = res + ")";
	println(res);
      }
      else
	println("(error \"Could not find any method named " +
		methodName + " in " + baseClassName + 
		" or any of its superclasses.\")");

    }
    catch (ClassNotFoundException ee) {
      println("(error \"Could not find class " + 
	      baseClassName + "\")");
    }

  }

  public static void getMethodSkeletonExpression(int variant) {
    Signature s = (Signature) overrideFactory.candidates.elementAt(variant);
    String skel = overrideFactory.getMethodSkeletonExpression(s);
    println(skel);

    // Register imported classes.
    overrideFactory.imports.clear();
    Method m = s.getMethod();

    Class[] types = m.getParameterTypes();
    for (int i = 0; i < types.length; ++i)
      overrideFactory.registerImport(types[i]);

    types = m.getExceptionTypes();
    for (int i = 0; i < types.length; ++i)
      overrideFactory.registerImport(types[i]);

    overrideFactory.registerImport(m.getReturnType());

  }

  public static void getImportedClasses() {
    println(overrideFactory.getImportsAsList());
  }

  /** 
   * Clears the import and candidate hashtables for this factory so they
   * can be re-used to process a new set of interfaces.
   */
  public void flush()
  {
    super.flush();
    candidates.removeAllElements();
  }

  public static void main (String[] args) {

    getCandidateSignatures("jde.ui.design.command.Command","getName");
     
  } // end of main ()
  


} // MethodOverrideFactory


/*
 * $Log: MethodOverrideFactory.java,v $
 * Revision 1.8  2002/06/03 18:12:27  mnl
 * Use DynamicClassLoader instead of simple Class.forName to avoid class
 * resolving (linking).
 *
 * Revision 1.7  2002/05/14 06:38:44  paulk
 * Enhances code generation wizards for implementing interfaces, abstract
 * classes, etc., to use customizable templates to generate skeleton methods
 * instead of hard-wired skeletons. Thanks to "Dr. Michael Lipp" <lipp@danet.de>
 * for proposing and implementing this improvement.
 *
 * Revision 1.6  2001/08/14 05:15:03  paulk
 * Miscellaneous updates.
 *
 * Revision 1.5  2001/08/11 06:52:31  paulk
 * Adds javadoc parameter to getMethodSkeleton. Thanks to Javier Lopez.
 *
 *
 */

// End of MethodOverrideFactory.java
