package com.icl.saxon.axis;
import com.icl.saxon.*;
import com.icl.saxon.om.NodeInfo;
import com.icl.saxon.expr.NodeEnumeration;
import org.xml.sax.SAXException;
import java.util.*;

class PrecedingEnumeration extends AxisEnumeration {

    NodeInfo start;
    
    public PrecedingEnumeration(NodeInfo node, int nodeType, NameTest nodeName)
    throws SAXException {
        super(node, nodeType, nodeName);
        start = node;
        advance();   
    }


    /**
    * Special code to skip the ancestors of the start node
    */

    protected boolean conforms(NodeInfo node) throws SAXException {
        if (node!=null) {
            if (node.isAncestor(start)) {
                return false;
            }
        }
        return super.conforms(node);
    }

    protected void step() throws SAXException {
        do {
            next = next.getPreviousInDocument();
        } while (next!=null && next.isAncestor(start));
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
