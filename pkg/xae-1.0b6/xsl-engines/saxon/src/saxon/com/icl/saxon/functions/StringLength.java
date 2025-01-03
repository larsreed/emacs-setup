package com.icl.saxon.functions;
import com.icl.saxon.*;
import com.icl.saxon.expr.*;
import org.xml.sax.SAXException;
import java.util.*;

public class StringLength extends Function {

    public String getName()      { return "string-length"; };

    /**
    * Determine the data type of the exprssion, if possible
    * @return Value.NUMBER
    */

    public int getDataType() {
        return Value.NUMBER;
    }

    /**
    * Determine which aspects of the context the expression depends on. The result is
    * a bitwise-or'ed value composed from constants such as Context.VARIABLES and
    * Context.CURRENT_NODE
    */

    public int getIntrinsicDependencies() {
        int numArgs = arguments.size();
        return (numArgs==0 ? Context.CONTEXT_NODE : 0);
    }

    public Value eval(Vector args, Context context) throws SAXException {
        int numArgs = checkArgumentCount(0, 1);
        String s;
        if (args.size()==0) {
            s = context.getContextNode().getValue();
        } else {
            Value arg0 = (Value)args.elementAt(0);
            s = arg0.asString();
        }
        return new NumericValue(StringValue.getLength(s));
    }
    
    /**
    * Simplify
    * If there is an argument and it is a Value, this is a pure function so it can be simplified
    */

    public Expression simplify() throws SAXException {
        int numArgs = checkArgumentCount(0, 1);
        if (numArgs==1) {
            return simplifyPureFunction();
        } else {
            return super.simplify();
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
