import java.util.*;
import java.io.*;
import java.net.*;
import com.icl.saxon.*;
import com.icl.saxon.handlers.*;
import com.icl.saxon.output.*;
import com.icl.saxon.expr.*;
import com.icl.saxon.om.*;
import com.icl.saxon.tree.Builder;
import org.xml.sax.SAXException;


/******************************************************************
* Class LoadPlay<P>
* This program takes a Shakespeare play in XML form and splits it
* into a number of output XML files: play.xml and sceneN.xml (one per
* scene) in an output directory.<P>
* Usage: java LoadPlay <I>play-name</I><BR>
* where <I>play-name</I> identifies the play. The input XML file
* is in <I>play-name.xml</I>; the output files will be created in
* a directory called <I>play-name</I>, which is created if necessary.
* @author M.H.Kay (Michael.Kay@icl.com)
* @version 15 December 1999
*/

class LoadPlay extends Controller
{

    private int sceneNr;                // scene number within play, starts at 1
    private String outputDir;           // output directory
    private String playName;            // name of play, as supplied on input
    
    /**
    * command-line interface.<BR>
    * Takes a single argument, the name of a play<BR>
    * Expects to find the XML in [play].xml<BR>
    * Creates output files in the directory called [play]
    */

    public static void main (String args[])
    throws java.lang.Exception
    {
        // Check the command-line arguments

        if (args.length != 1) {
            System.err.println("Usage: java LoadPlay name-of-play");
            System.exit(1);
        }
        
        // Instantiate the loader and run it
        
        LoadPlay d = new LoadPlay();
        d.load( args[0] );
    }

    /**
    * Process a play, generating multiple output files
    * @param play The name of the play (the input must be in "<I>play</I>.xml")
    */

    public void load(String play) throws java.lang.Exception {
    
        sceneNr = 0;

        // set up the element handlers

        RuleManager rm = new RuleManager();
        setRuleManager(rm);

        rm.setHandler("*", new ElementCopier());
        rm.setHandler("PLAY", new PLAYHandler()); 

        rm.setHandler("SCENE", new SCENEHandler());
        rm.setHandler("PROLOGUE", new SCENEHandler());
        rm.setHandler("EPILOGUE", new SCENEHandler());
 
        rm.setHandler("FM", new ElementSuppressor()); // removes the element from output
       
        // Remember the name of the play
        
        playName = play;

        // Check the input file is OK
        
        String inputFile = play + ".xml";
        File f = new File(inputFile);
        if (!f.exists() || f.isDirectory() || !f.canRead()) {
            System.err.println("Cannot read from " + inputFile + "!");
            System.err.println("Check that the current directory is the directory containing " + inputFile);
            System.exit(2);
        }
        
        // Determine the output directory, create it if necessary
        
        outputDir = play;
        File dir = new File(outputDir);
        if (dir.exists()) {
            if (!dir.isDirectory()) {
                System.err.println(outputDir + " exists but is not a directory!");
                System.err.println("LoadPlay needs to create its output in a directory called " + outputDir);
                System.err.println("It will create the directory if necessary");
                System.exit(2);
            }
        } else {
            try {
                dir.mkdir();
            }
            catch (Exception e) {
                System.err.println("Cannot create output directory " + outputDir);
                return;
            }
        }

        // Build the source document tree

        Builder b = new Builder();
        b.setStripAll();
        DocumentInfo doc = b.build( new ExtendedInputSource( new File(inputFile) ) );


     
	    // Process the XML through the registered handlers

        run(doc);

        // close the output file

        resetOutputDetails();


    }

// Node handlers

    class ElementCopier extends NodeHandler {
    
        public void start(NodeInfo e, Context context) throws SAXException {
            getOutputter().writeStartTag(e.getExpandedName());
            applyTemplates(context, null, null, null);
            getOutputter().writeEndTag(e.getExpandedName());
        }

    }

    class ElementSuppressor extends NodeHandler {
    
        public void start(NodeInfo e, Context context) throws SAXException {
            // do nothing            
        }

    }

   class PLAYHandler extends NodeHandler {
    
        public void start(NodeInfo e, Context context) throws SAXException {

            // Count the number of scenes

            Expression exp = Expression.make("count(/PLAY/ACT/SCENE | /PLAY/ACT/EPILOGUE | /PLAY/ACT/PROLOGUE)");
            int sceneCount = (int)exp.evaluateAsNumber(context);

            // Define output Writers for different parts of the XML input document
            // (Note: we will define an output writer for each SCENE instance later)
        
            String playFile = outputDir + File.separator + "play.xml";

            try {
                PrintWriter playWriter = new PrintWriter(new FileWriter(playFile));
                OutputDetails playDetails = new OutputDetails();
                playDetails.setWriter(playWriter);
                playDetails.setMethod("xml");
                playDetails.setEncoding("iso-8859-1");
                playDetails.setIndent("yes");
                setNewOutputDetails(playDetails);
            } catch (java.io.IOException err) {
                throw new SAXException(err);
            }

            Outputter out = getOutputter();
            out.writeStartTag(new Name("PLAY"));
            out.writeAttribute(new Name("NAME"), playName);
            out.writeAttribute(new Name("SCENES"), ""+sceneCount);

            applyTemplates(context, null, null, null);

            out.writeEndTag(new Name("PLAY"));
            resetOutputDetails();
        }
   }


class SCENEHandler extends NodeHandler {

    /**
    * Handle a SCENE element (or PROLOGUE or EPILOGUE).
    * At start, increment the scene number and start a new output file.
    * Output a SCENE tag with context information about the scene.
    * At the end of the scene, output summary details
    * to the PLAY file in the form of a cross-reference.
    */

    public void start(NodeInfo e, Context context) throws SAXException {
        
        sceneNr++;

        // start a new output file for this scene
        
        String fileName = outputDir + File.separator +"scene" + sceneNr + ".xml";
        try {
            FileWriter sceneFile = new FileWriter(fileName);
            OutputDetails sceneDetails = new OutputDetails();
            sceneDetails.setWriter(sceneFile);
            sceneDetails.setMethod("xml");
            sceneDetails.setEncoding("iso-8859-1");
            sceneDetails.setIndent("yes");
            setNewOutputDetails(sceneDetails);
        } catch (java.io.IOException err) {
            System.err.println("Cannot write to " + fileName);
        }
        Outputter out = getOutputter();
        out.writeStartTag(e.getExpandedName());
        out.writeAttribute(new Name("SCENE"), ""+sceneNr);
        out.writeStartTag(new Name("CONTEXT"));

        out.writeStartTag(new Name("PLAY"));
        out.writeAttribute(new Name("NAME"), playName);

        Expression playTitle = Expression.make("ancestor::PLAY/child::TITLE");
        out.writeAttribute(new Name("TITLE"), playTitle.evaluateAsString(context));
        out.writeEndTag(new Name("PLAY"));

        out.writeStartTag(new Name("ACT"));
        Expression actTitle = Expression.make("ancestor::ACT/child::TITLE");
        out.writeAttribute(new Name("ACTTITLE"), actTitle.evaluateAsString(context));
            
        out.writeEndTag(new Name("ACT"));        
        out.writeEndTag(new Name("CONTEXT"));

        applyTemplates(context, null, null, null);

        out.writeEndTag(e.getExpandedName());
        resetOutputDetails();

        // now write the scene title to the play file
        out = getOutputter();
        out.writeStartTag(new Name("SCTITLE"));
        out.writeAttribute(new Name("SCENE"), ""+sceneNr);
        Expression sceneTitle = Expression.make("child::TITLE");
        out.writeContent(sceneTitle.evaluateAsString(context));
        out.writeEndTag(new Name("SCTITLE"));
        
    }
}


} // end of outer class

