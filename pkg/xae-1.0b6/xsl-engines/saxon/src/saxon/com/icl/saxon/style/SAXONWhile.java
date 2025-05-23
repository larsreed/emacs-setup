package com.icl.saxon.style;
import com.icl.saxon.*;
import com.icl.saxon.expr.*;
import org.xml.sax.SAXException;
import org.xml.sax.AttributeList;

/**
* Handler for saxon:while elements in stylesheet.<BR>
* The saxon:while element has a mandatory attribute test, a boolean expression.
* The content is output repeatedly so long as the test condition is true.
*/

public class SAXONWhile extends StyleElement {

    private Expression test;

    /**
    * Determine whether this node is an instruction.
    * @return true - it is an instruction
    */

    public boolean isInstruction() {
        return true;
    }


    public void prepareAttributes() throws SAXException {

        String[] allowed = {"test"};
        allowAttributes(allowed);
        
        String testAtt = getAttributeValue("test");        
        if (testAtt==null)
            reportAbsence("test");
        test = Expression.make(testAtt, this);
    }

    public void validate() throws SAXException {
        checkWithinTemplate();
        //checkElementExtensions("saxon");
    }

    public void process(Context context) throws SAXException
    {        
        while (test.evaluateAsBoolean(context)) {
            processChildren(context);
        }
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
