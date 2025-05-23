package com.icl.saxon.om;
import org.xml.sax.SAXException;

/**
  * An object representing a Namespace
  * @author <A HREF="mailto:Michael.Kay@icl.com>Michael H. Kay, ICL</A> 
  */

public class Namespace {

    /**
    * Fixed namespace name for XML: "http://www.w3.org/XML/1998/namespace". 
    */
    public static final String XML = "http://www.w3.org/XML/1998/namespace";

    /**
    * Fixed namespace name for XMLNS: "http://www.w3.org/xml". Actually, no namespace URL
    * is defined for the "xmlns" prefix, it is implicit. We use this for convenience
    */
    public static final String XMLNS = "http://www.w3.org/xml/xmlns";

    /**
    * Fixed namespace name for XSLT: "http://www.w3.org/1999/XSL/Transform"
    */
    public static final String XSLT = "http://www.w3.org/1999/XSL/Transform";

    /**
    * Fixed namespace name for SAXON: "http://icl.com/saxon"
    */
    public static final String SAXON = "http://icl.com/saxon";

    /**
    * Fixed namespace name for SAXON DTD extension: "http://icl.com/saxon/dtd"
    */
    public static final String DTD = "http://icl.com/saxon/dtd";


}

//
// The contents of this file are subject to the Mozilla Public License Version 1.0 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/ 
//
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either express or implied.
// See the License for the specific language governing rights and limitations under the License. 
//
// The Original Code is: all this file. 
//
// The Initial Developer of the Original Code is
// Michael Kay of International Computers Limited (Michael.Kay@icl.com).
//
// Portions created by (your name) are Copyright (C) (your legal entity). All Rights Reserved. 
//
// Contributor(s): none. 
//
