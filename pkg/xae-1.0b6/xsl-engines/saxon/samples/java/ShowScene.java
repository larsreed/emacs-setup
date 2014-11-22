import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;

import com.icl.saxon.*;
import com.icl.saxon.handlers.*;
import com.icl.saxon.output.*;
import com.icl.saxon.expr.*;
import com.icl.saxon.om.*;
import com.icl.saxon.tree.Builder;
import org.xml.sax.SAXException;

/**
 * ShowScene Servlet<BR>
 * Displays a scene of a Shakespeare play encoded in XML 
 * @author Michael Kay
 * @version 17 December 1999
 */
 
public class ShowScene extends ShowPlay {

    /**
    * service() - accept request and produce response<BR>
    * URL parameters: <UL>
    * <LI>dir - the directory in which all the XML is held
    * <LI>play - the directory name of this Shakespeare play
    * <LI>scene - the sequential number of this scene
    * <LI>of - the number of the last scene in the play
    * </UL>
    * @param req The HTTP request
    * @param res The HTTP response
    */
    
    public void service(HttpServletRequest req, HttpServletResponse res)
	throws ServletException, IOException
    {
        Controller app = new Controller();
        RuleManager rm = new RuleManager();
        app.setRuleManager(rm);
        
        String errorText = "";
    
        res.setContentType("text/html");
        ServletOutputStream out = res.getOutputStream();
        
        String play = req.getParameter("play");
        String scene = req.getParameter("scene");
        String dir = req.getParameter("dir");
        String lastScene = req.getParameter("of");
        
        String title = "";
        String playtitle = "";
        String acttitle = "";

        StringWriter mainSW = new StringWriter();
        
        try {
            OutputDetails details = new OutputDetails();
            details.setWriter(mainSW);
            details.setMethod("html");

            app.setOutputDetails(details);
            
            rm.setHandler("CONTEXT", new ElementSuppressor());        
            rm.setHandler("SCENE/TITLE", new ElementSuppressor());
            
            setItemRendition(rm,  "PLAY",            "<A HREF='ShowPlay?dir=" + dir + "&play=" + play + "'>{@TITLE}</A>", "<BR>\n" );
            setItemRendition(rm,  "ACT",             "{@TITLE}", "<BR>\n" );
       
            setItemRendition(rm,  "SCENE",           "", ""       );             
            setItemRendition(rm,  "SPEECH",          "<TABLE><TR>", "</TR></TABLE>\n" );                          
            setGroupRendition(rm, "SPEAKER",         "<TD WIDTH=160 VALIGN=TOP><B>", "<BR>", "</B></TD><TD VALIGN=TOP>" );         
            setItemRendition(rm,  "SCENE/STAGEDIR",  "<CENTER><H3>", "</H3></CENTER>\n" );
            setItemRendition(rm,  "SPEECH/STAGEDIR", "<P><I>", "</I></P>" );                 
            setItemRendition(rm,  "LINE/STAGEDIR",   " [ <I>", "</I> ] " );
            setItemRendition(rm,  "SCENE/SUBHEAD",   "<CENTER><H3>", "</H3></CENTER>\n" );                           
            setItemRendition(rm,  "SPEECH/SUBHEAD",  "<P><B>", "</B></P>" );
            setItemRendition(rm,  "LINE",            "", "<BR>\n" );

            String inputFile = dir + File.separator + play + File.separator + "scene" + scene + ".xml";
            Builder b = new Builder();
            DocumentInfo doc = b.build ( new ExtendedInputSource( new File(inputFile) ) );
            app.run(doc);
            Context c = app.makeContext(doc);
            title = Expression.make("/*/TITLE").evaluateAsString(c);
            playtitle = Expression.make("/*/CONTEXT/PLAY/@TITLE").evaluateAsString(c);
            acttitle = Expression.make("/*/CONTEXT/ACT/@ACTTITLE").evaluateAsString(c);
            
        } catch (SAXException err) {
            out.println("Error in servlet " + err.getClass().getName() +
                         ": " + err.getMessage());
        }
        out.println("<HTML><HEAD><TITLE>" + title + "</TITLE></HEAD>");
        out.println("<BODY BGCOLOR='#FFFFCC'>"); 
        out.println("<A HREF='ShowPlay?dir=" + dir + "&play=" + play + "'>" + playtitle + "</A>");
        out.println("<BR>");
        out.println(acttitle + "<HR>");
        out.println("<CENTER><H1>" + title + "</H1></CENTER>");
        out.println(mainSW.toString() + "<HR>");
        if (!scene.equals(lastScene)) {
            int nextScene = Integer.parseInt(scene) + 1;
            String nextSceneLink = "ShowScene?dir=" + dir + "&play=" + play + "&scene=" + nextScene + "&of=" + lastScene;
            out.println("<A HREF='" + nextSceneLink + "'>Next scene</A>");
        }
        out.println("</BODY></HTML>");        
    }

    /**
    * getServletInfo<BR>
    * Required by Servlet interface
    */   

    public String getServletInfo() {
        return "Returns an HTML representation of a Shakespeare scene";
    }
    
    // inner classes

    private class ElementSuppressor extends NodeHandler {

        public void start(NodeInfo e, Context c) throws SAXException {

        }

    }

}
