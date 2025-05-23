package com.icl.saxon.style;
import com.icl.saxon.*;
import org.xml.sax.SAXException;

/**
* A procedure represents a top-level element that can contain local variable declarations.
* Specifically, a top-level xsl:template, xsl:variable, xsl:param, or saxon:function element
*/
    
public class Procedure {

    protected int numberOfVariables = 0;

    /**
    * Get number of variables (size of stack frame)
    */

    public int getNumberOfVariables() {
        return numberOfVariables;
    }

    /**
    * Allocate a slot number for a variable
    */

    public int allocateSlotNumber() {
        return numberOfVariables++;
    }

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
