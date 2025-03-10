package com.icl.saxon.style;
import com.icl.saxon.*;
import com.icl.saxon.om.Name;
import com.icl.saxon.expr.*;
import com.icl.saxon.output.*;
import org.xml.sax.SAXException;
import org.xml.sax.AttributeList;
import java.util.*;

/**
* An xsl:processing-instruction element in the stylesheet.
*/

public class XSLProcessingInstruction extends StyleElement {

    Expression name;

    /**
    * Determine whether this node is an instruction.
    * @return true - it is an instruction
    */

    public boolean isInstruction() {
        return true;
    }


    public void prepareAttributes() throws SAXException {

        String[] allowed = {"name"};
        allowAttributes(allowed);

        String nameAtt = getAttributeValue("name");
        if (nameAtt==null) {
            reportAbsence("name");
        } else {
            name = AttributeValueTemplate.make(nameAtt, this);
        }
    }

    public void validate() throws SAXException {
        checkWithinTemplate();
    }


    public void process( Context context ) throws SAXException
    {
        String expandedName = name.evaluateAsString(context);

        if (!(Name.isNCName(expandedName)) || expandedName.equalsIgnoreCase("xml")) {
            context.getController().reportRecoverableError(
                "Processing instruction name is invalid: " + expandedName, this);
            return;
        }

        String data = expandChildren(context);

        int hh = data.indexOf("?>");
        if (hh >= 0) {
            context.getController().reportRecoverableError(
                "Invalid characters (?>) in processing instruction", this);
            data = data.substring(0, hh+1) + " " + data.substring(hh+1);
        }

        context.getOutputter().writePI(expandedName, data);
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
