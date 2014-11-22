// Transformations for XML (TRaX)
// Copyright ©2000 Lotus Development Corporation, Exoffice Technologies,
// Oracle Corporation, Michael Kay of International Computers Limited, Apache
// Software Foundation.  All rights reserved.

// Needed java classes
import java.io.*;
import java.net.URL;
import java.net.MalformedURLException;

// Needed SAX classes
import org.xml.sax.*;

// Needed DOM classes
import org.w3c.dom.*;

// Needed Trax classes
import com.icl.saxon.trax.*;
import com.icl.saxon.trax.serialize.*;

// For the DOM examples we use Xerces
import org.apache.xerces.parsers.DOMParser;
import org.apache.xerces.dom.DocumentImpl;

/**
 * Some examples to show how the Simple API for Transformations 
 * could be used.
 *
 * @version Alpha
 * @author <a href="mailto:scott_boag@lotus.com">Scott Boag</a>, modified by
 * <a href="mailto:Michael.Kay@icl.com">Michael Kay</a>
 */
 
public class TestTrax
{
  private static String indir;
  private static String outdir;

  
  public static void main( String argv[] )
    throws ProcessorException, ProcessorFactoryException, 
           TransformException, SAXException, IOException
  {
    if (argv.length < 2) {
        System.err.println("Usage: java TestTrax input-directory output-directory");
    }
    
    indir = argv[0] + File.separator;
    outdir = argv[1] + File.separator;
    
    TestTrax t = new TestTrax();
    
    announce("Simple test");
    t.exampleSimple();

    announce("SAX test");
    t.exampleSAX();

    announce("Filter chain - two step");
    t.exampleXMLFilterChain2();

    announce("Filter chain - three step");
    t.exampleXMLFilterChain3();

    announce("DOM to DOM");
    t.exampleDOM2DOM();

    announce("Params");
    t.exampleParam();
    
    announce("Output Format");
    t.exampleOutputFormat();

    announce("Use Associated");
    t.exampleUseAssociated();

  }

  private static void announce(String test) {
    System.err.println("----------------------------------------");
    System.err.println(test);
    System.err.println("----------------------------------------");
  }
  
  /**
   * Show the simplest possible transformation from system id to output stream.
   */
   
    public void exampleSimple()
    throws ProcessorException, ProcessorFactoryException, 
           TransformException, SAXException, IOException
    {  
        Processor processor = Processor.newInstance("xslt");

        Templates templates = processor.process(source(indir+"books.xsl"));
        Transformer transformer = templates.newTransformer();
        transformer.transform(source(indir+"books.xml"),
                              new Result(new FileWriter(outdir+"simple.html")));
    }
  
    /**
    * Show the Transformer using SAX events in and SAX events out.
    */

    public void exampleSAX()
    throws ProcessorException, ProcessorFactoryException, 
           TransformException, SAXException, IOException {
   
        Processor processor = Processor.newInstance("xslt");
        XMLReader parser = new com.icl.saxon.aelfred.SAXDriver();

        // Have a Templates builder handle the parse events from the SAXParser's 
        // parse of an xslt file.
        
        TemplatesBuilder templatesBuilder = processor.getTemplatesBuilder();
        parser.setContentHandler(templatesBuilder);
        parser.parse(source(indir+"books.xsl"));
        Templates templates = templatesBuilder.getTemplates();
    
        // Get a transformer instance for the templates.
        Transformer transformer = templates.newTransformer();
        ((com.icl.saxon.Controller)transformer).addTraceListener(new com.icl.saxon.trace.SimpleTraceListener());
    
        // Set the result handling to be a content handler that shows element names only.
        ContentHandler elementDisplay = new ElementDisplay(outdir + "saxoutput.txt");
        transformer.setContentHandler(elementDisplay);
        transformer.parse(source(indir+"books.xml"));

    }

    private class ElementDisplay extends org.xml.sax.helpers.DefaultHandler {

        private Writer writer;
        int indent = 0;
        public ElementDisplay(String out) {
            try {
                writer = new FileWriter(out);
            } catch (java.io.IOException err) {
                System.err.println(out = ": " + err);
            }
        }
        
        public void startElement (String uri, String localName,
    			      String qName, Attributes attributes)
    	throws SAXException
        {
            try {
                for (int i=0; i<indent; i++) writer.write(' ');
    	        writer.write(qName);
    	        writer.write('\n');
    	        indent+=2;
            } catch (java.io.IOException err) {
                System.err.println(err);
            }
        }

        public void endElement (String uri, String localName,
    			      String qName)
    	throws SAXException
        {
  	        indent-=2;
        }

        public void endDocument() {
            try {
                writer.close();
            } catch (java.io.IOException err) {
                System.err.println(err);
            }
        }
    }

    /**
    * This example shows how to chain events from one Transformer 
    * to another transformer, using the Transformer as a 
    * SAX2 XMLFilter/XMLReader.
    */


    public void exampleXMLFilterChain2()
    throws SAXException, IOException
    {  
        Processor processor = Processor.newInstance("xslt");

        Templates stylesheet1 = processor.process(source(indir+"books.xsl"));
        Transformer transformer1 = stylesheet1.newTransformer();
        ((com.icl.saxon.Controller)transformer1).setDiagnosticName("transformer1");

        Templates stylesheet2= processor.process(source(indir+"filter1.xsl"));
        Transformer transformer2 = stylesheet2.newTransformer();
        ((com.icl.saxon.Controller)transformer2).setDiagnosticName("transformer2");

        // transformer1 will use a SAX parser as it's reader.    
        transformer1.setParent(new com.icl.saxon.aelfred.SAXDriver());
    
        // transformer2 will use transformer1 as it's reader.
        transformer2.setParent(transformer1);
    
        // transformer2 will output the events to the serializer.
        transformer2.setContentHandler(new ElementDisplay(outdir+"filter2output.txt"));

        // Now, when you call transformer2 to parse, it will set  
        // itself as the ContentHandler for transform1, and call transform1.parse, 
        // which will set itself as the content listener for the 
        // SAX parser, and call parser.parse(new InputSource("foo.xml")).
        
        transformer2.parse(source(indir+"books.xml"));
    }



    /**
    * This example shows how to chain events from one Transformer 
    * to another transformer, using the Transformer as a 
    * SAX2 XMLFilter/XMLReader.
    */


    public void exampleXMLFilterChain3()
    throws SAXException, IOException
    {  
        Processor processor = Processor.newInstance("xslt");

        Templates stylesheet1 = processor.process(source(indir+"books.xsl"));
        Transformer transformer1 = stylesheet1.newTransformer();

        Templates stylesheet2= processor.process(source(indir+"filter1.xsl"));
        Transformer transformer2 = stylesheet2.newTransformer();

        Templates  stylesheet3 = processor.process(source(indir+"filter2.xsl"));
        Transformer transformer3= stylesheet3.newTransformer();
    
        // transformer1 will use a SAX parser as it's reader.    
        transformer1.setParent(new com.icl.saxon.aelfred.SAXDriver());
    
        // transformer2 will use transformer1 as it's reader.
        transformer2.setParent(transformer1);
    
        // transform3 will use transform2 as it's reader.
        transformer3.setParent(transformer2);
    
        // transform3 will output the events to the serializer.
        //transformer3.setOutputWriter(new ElementDisplay());

        // Now, when you call transformer3 to parse, it will set  
        // itself as the ContentHandler for transform2, and 
        // call transform2.parse, which will set itself as the 
        // content handler for transform1, and call transform1.parse, 
        // which will set itself as the content listener for the 
        // SAX parser, and call parser.parse(new InputSource("foo.xml")).
        
        transformer3.transform(source(indir+"books.xml"),
                               new Result(new FileWriter(outdir+"filter3output.xml")));
    }

    /**
    * Show how to transform a DOM tree into another DOM tree.  
    * This uses the javax.xml.parsers to parse an XML file into a 
    * DOM, and create an output DOM.
    */

    public void exampleDOM2DOM()
        throws SAXException, IOException
    {  
        Processor processor = Processor.newInstance("xslt");

        if (processor.getFeature("http://xml.org/trax/features/dom/input")) {
            Templates templates = processor.process(source(indir+"books.xsl"));
            Transformer transformer = templates.newTransformer();


            // Use Xerces DOM
            DOMParser parser = new DOMParser();
            parser.parse(source(indir+"books.xml"));
            Document doc = parser.getDocument();
            
            Node outNode = new DocumentImpl();
            transformer.transformNode(doc, new Result(outNode));

            // demonstrate that there's something in the DOM node

            Templates stylesheet2= processor.process(source(indir+"filter1.xsl"));
            Transformer transformer2 = stylesheet2.newTransformer();
            transformer2.transformNode(outNode,
                               new Result(new FileWriter(outdir+"domoutput.xml")));            
            
        } else {
            throw new org.xml.sax.SAXNotSupportedException("DOM node processing not supported!");
        }
    }

    /**
    * This shows how to set a parameter for use by the templates.
    */
   
    public void exampleParam()
        throws ProcessorException, ProcessorFactoryException, 
            TransformException, SAXException, IOException
    {  
        Processor processor = Processor.newInstance("xslt");

        Templates templates = processor.process(source(indir+"books.xsl"));
        Transformer transformer = templates.newTransformer();
        transformer.setParameter("title", "mine.own.uri" /* namespace */,
                                 "More Boring Old Books");
    
        transformer.transform(source(indir+"books.xml"),
                new Result(new FileWriter(outdir+"boringbooks.html")));
    }
    
  /**
   * Show how to override output properties.
   */
   
  public void exampleOutputFormat()
    throws ProcessorException, ProcessorFactoryException, 
           TransformException, SAXException, IOException
  {  
    Processor processor = Processor.newInstance("xslt");

    Templates templates = processor.process(source(indir+"books.xsl"));
    OutputFormat oprops = templates.getOutputFormat();
    oprops.setIndenting( false );
    Transformer transformer = templates.newTransformer();
    transformer.setOutputFormat(oprops);
    
    transformer.transform(source(indir+"books.xml"),
                          new Result(new FileWriter(outdir+"non-indented.html")));
  }
    
  /**
   * Show how to get stylesheets that are associated with a given 
   * xml document via the xml-stylesheet PI (see http://www.w3.org/TR/xml-stylesheet/).
   */
   
  public void exampleUseAssociated()
    throws ProcessorException, ProcessorFactoryException, 
           TransformException, SAXException, IOException
  {  
    Processor processor = Processor.newInstance("xslt");

    InputSource docSource = source(indir+"books.xml");
    InputSource[] sources 
      = processor.getAssociatedStylesheets(docSource, null, "priced-books", null);

    if (sources==null) {
        System.err.println("No stylesheet found");
    } else {        
        Templates templates = processor.processMultiple(sources);
        Transformer transformer = templates.newTransformer();    
        transformer.transform(docSource,
                         new Result(new FileWriter(outdir+"priced-books.html")));
    }
  }

    /**
    * Create an InputSource that refers to a given File
    */

  private static InputSource source(String filename)
  {
    File file = new File(filename);
    String path = file.getAbsolutePath();

    URL url = null;
    try 
        {
            url = new URL(path);
        } 
    catch (MalformedURLException ex) 
        {
            try 
            {
                // This is a bunch of weird code that is required to
                // make a valid URL on the Windows platform, due
                // to inconsistencies in what getAbsolutePath returns.
                String fs = System.getProperty("file.separator");
                if (fs.length() == 1) 
                {
                    char sep = fs.charAt(0);
                    if (sep != '/')
                        path = path.replace(sep, '/');
                    if (path.charAt(0) != '/')
                        path = '/' + path;
                }
                path = "file://" + path;
                url = new URL(path);
            } 
            catch (MalformedURLException e) 
            {
                return null;
            }
        }   
     return( new InputSource(url.toString()) );
  }


    
}
