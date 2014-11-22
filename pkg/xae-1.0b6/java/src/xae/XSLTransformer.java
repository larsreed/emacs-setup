/* XSLTransformer.java -- XML Authoring Environment for Emacs.
 * $Revision: 1.2 $ $Date: 2001/02/03 16:43:35 $ 
 *
 * Author: Paul Kinnucan <pkinnucan@mediaone.net>
 * Maintainer: Paul Kinnucan
 * Copyright (C) 2000, 2001 Paul Kinnucan.
 *
 * GNU Emacs is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * GNU Emacs is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GNU Emacs; see the file COPYING.  If not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * Commentary:
 *
 * This is one of a set of Lisp and Java programs that make up the 
 * XML Authoring Environment (XAE) for Emacs. See the
 * XAE User's Guide for more information.
 *
 * The latest version of the XAE is available at
 * <URL:http://xae.sunsite.dk>.
 *
 * Please send any comments, bugs, or upgrade requests to
 * Paul Kinnucan at pkinnucan@mediaone.net
 *
 * $Log: XSLTransformer.java,v $
 * Revision 1.2  2001/02/03 16:43:35  paulk
 * The stylesheet URI is now computed using the context concept in Java's URL
 * class. Thanks to "Sven Kuenzler" <svenk@gmx.net>.
 *
 */
package xae;

import java.util.Hashtable;
import com.icl.saxon.StyleSheet;
import com.icl.saxon.trax.Templates;
import org.xml.sax.InputSource;
import com.icl.saxon.trax.ProcessorException;
import java.io.File;
import com.icl.saxon.PreparedStyleSheet;
import com.icl.saxon.PIGrabber;
import org.xml.sax.XMLReader;
import org.xml.sax.SAXException;
import com.icl.saxon.ParserManager;
import java.net.URL;
import java.net.MalformedURLException;


/**
 * XSLTransformer.java
 *
 *
 * Created: Sat Dec 30 00:07:56 2000
 *
 * @author <a href="mailto: "</a>
 * @version
 */
public class XSLTransformer {

  public XSLTransformer (){
  }

  public void applyStylesheet(String inputDocURL, String stylesheetURL, String outputDocPath) {

    PreparedStyleSheet stylesheet = (PreparedStyleSheet) styles.get(stylesheetURL);
	
    if (stylesheet == null) {
      try {
	System.out.println("Compiling stylesheet...\n");
	stylesheet = (PreparedStyleSheet) xslt.process(new InputSource(stylesheetURL));
      }
      catch (ProcessorException ex) {
	System.out.println(ex.toString());
	System.out.println("Command aborted.\n");
	return;
      }
      styles.put(stylesheetURL, stylesheet);
    }
    
    try {
      System.out.println("Transforming document...\n");
      xslt.processFile(new InputSource(inputDocURL), stylesheet, new File(outputDocPath),
		       null);
      System.out.println("Done.\n");
    }
    catch (Exception ex) {
      System.out.println(ex.toString());
      System.out.println("Command aborted.\n");
    }
    
  }


  public void applyAssociatedStylesheet(String inputDocURI, String outputDocPath) {

    InputSource inputDoc = new InputSource(inputDocURI);
    try {
      URL docURL;
      try{
	  docURL = new URL(inputDocURI);
      } catch(MalformedURLException ex3){
	  System.out.println("Assuming 'file:' protocol");
	  docURL=new URL("file:"+inputDocURI);
      }
      String[] ssURIStrs = getAssociatedStylesheetURIs(inputDoc, null, null, null);
      URL ssURL= new URL(docURL,ssURIStrs[0]);
      
      applyStylesheet(inputDocURI, ssURL.toExternalForm(), outputDocPath);
    }
    catch (ProcessorException ex) {
      ex.printStackTrace();
      System.out.println("Command aborted.\n");
    }
    catch (MalformedURLException ex1) {
      ex1.printStackTrace();
      System.out.println("Command aborted.\n");      
    }
  }

  public String[] getAssociatedStylesheetURIs(InputSource source,
                                                      String media, 
                                                      String title,
                                                      String charset)
    throws ProcessorException
    {
        PIGrabber grabber = new PIGrabber();
        grabber.setCriteria(media, title, charset);
        grabber.setBaseURI(source.getSystemId());

	if (parser==null) {
            try {
                parser = ParserManager.makeParser();
            } catch (SAXException err) {
                throw new ProcessorException(err);
            }
        }

        parser.setContentHandler(grabber);
        try {
            parser.parse(source);   // this parse will be aborted when the first start tag is found
        } catch (Exception err) {
            if (!(err instanceof SAXException && err.getMessage().equals("#start#"))) { 
                throw new ProcessorException("Failed while looking for xml-stylesheet PI", err);
            }
        }
        try {
            return grabber.getStylesheetURIs();
        } catch (SAXException err) {
            throw new ProcessorException(err.getMessage());
        }            
    }

  StyleSheet xslt = new StyleSheet();
  Hashtable styles = new Hashtable(5);
  XMLReader parser;

}// XSLTransformer

// End of XSLTransformer.java
