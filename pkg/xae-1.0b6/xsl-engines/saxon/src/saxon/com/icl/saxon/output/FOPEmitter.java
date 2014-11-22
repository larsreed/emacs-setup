package com.icl.saxon.output;
import com.icl.saxon.*;
import org.xml.sax.*;
import java.io.*;
import org.apache.fop.apps.Driver;
import org.apache.fop.apps.FOPException;

/**
  * FOPEmitter: This class acts as a SAXON output emitter that feeds data into James Tauber's
  * FOP (see http://www.jtauber.com/fop).
  * @version 14 Dec 1999: rewritten to use FOP v0.12 which doesn't require building the tree
  * in memory first.
  * @version 19 Sep 2000: the latest version of FOP (0.14.0) provides a SAX2 ContentHandler instead
  * of a SAX1 DocumentHandler
  */
  
public class FOPEmitter extends ContentHandlerProxy 
{
    private PrintWriter printWriter;
    private Driver fop;

    /**
    * Start of document processing
    */

    public void startDocument() throws SAXException {
        fop = new Driver();
	    String version = org.apache.fop.apps.Version.getVersion();
        ContentHandler ch = fop.getContentHandler();
	    fop.setRenderer("org.apache.fop.render.pdf.PDFRenderer", version);
	    fop.addElementMapping("org.apache.fop.fo.StandardElementMapping");
	    fop.addElementMapping("org.apache.fop.svg.SVGElementMapping");
        setUnderlyingContentHandler(ch);
        super.startDocument();
    }

    /**
    * End of document processing
    */

    public void endDocument() throws SAXException {
        super.endDocument();
        try {
            fop.setWriter(printWriter);
            fop.format();
            fop.render();
        } catch (FOPException err) {
            throw new SAXException("Error from FOP: ", err);
        } catch (java.io.IOException err) {
            throw new SAXException("I/O Error from FOP: ", err);
        } catch (Throwable err) {
            throw new SAXException("FOP processing failed: " + err.getMessage());
        }
    }

    /**
    * Set the Writer to be used. The writer will already be set up to perform any encoding
    * requested. A writer will always be supplied before startDocument() is called.
    */

    public void setWriter (Writer writer) {

        if (writer instanceof PrintWriter) {
            printWriter = (PrintWriter)writer;
        } else {
            printWriter = new PrintWriter(writer);
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
