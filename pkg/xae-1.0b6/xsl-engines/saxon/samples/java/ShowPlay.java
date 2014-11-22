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
 * ShowPlay Servlet<BR>
 * Outputs an XML Shakespeare play file in HTML. The play file must first be generated
 * using the LoadPlay application.
 * @author Michael Kay
 * @version 21 Sept 2000
 */
 
public class ShowPlay extends HttpServlet {

    /**
    * service() - accept request and produce response<BR>
    * URL parameters: <UL>
    * <LI>dir - the directory in which all the XML is held
    * <LI>play - the directory name of this Shakespeare play
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

        String dir = req.getParameter("dir");
        String play = req.getParameter("play");

        StringWriter mainSW = new StringWriter();
        String title="";
        String subtitle="";
        String scenedesc="";

        try {
            OutputDetails details = new OutputDetails();
            details.setWriter (mainSW);
            details.setMethod ("html");
            details.setOmitDeclaration("yes");
            app.setOutputDetails(details);
            
            
            rm.setHandler ("PLAY", new PlayHandler() );
            
            setItemRendition (rm, "PERSONAE", "<TABLE><TR><TD WIDTH=350 VALIGN=TOP BGCOLOR='#88FF88'>",
                                        "</TD><TD WIDTH=30></TD><TD VALIGN=TOP>");  
            setItemRendition (rm, "PERSONAE/TITLE | ACT/TITLE", "<CENTER><H3>", "</H3></CENTER>" );
            setItemRendition (rm, "PERSONAE/PERSONA", "<TABLE><TR><TD VALIGN=TOP>", "</TD></TR></TABLE>");
            setItemRendition (rm, "PGROUP", "", "" );
            setGroupRendition (rm, "PGROUP/PERSONA", "<TABLE><TR><TD WIDTH=160 VALIGN=TOP>", "<BR>\n", "</TD><TD WIDTH=20></TD>" );
            setGroupRendition (rm, "PGROUP/GRPDESCR", "<TD VALIGN=BOTTOM>", "<BR>\n", "</TD></TR></TABLE>\n" );                                        

            setItemRendition (rm, "ACT", "", "" );
            setItemRendition (rm, "SCTITLE",
                 "<A HREF='ShowScene?dir=" + dir +
                 "&play={ancestor::PLAY/@NAME}&scene={@SCENE}&of={ancestor::PLAY/@SCENES}'>",
                 "</A><BR>\n" );

            String inputFile = dir + File.separator + play + File.separator + "play.xml";
            Builder b = new Builder();
            DocumentInfo doc = b.build( new ExtendedInputSource( new File(inputFile) ) );
            app.run (doc);

            Context c = app.makeContext(doc);
            title = Expression.make("/PLAY/TITLE").evaluateAsString(c);
            subtitle = Expression.make("/PLAY/PLAYSUBT").evaluateAsString(c);
            scenedesc = Expression.make("/PLAY/SCNDESCR").evaluateAsString(c);


        } catch (SAXException err) {
            out.println("Error in servlet: " + err.getClass().getName() +
                         ": " + err.getMessage());         
        }

        out.println("<HTML><HEAD><TITLE>"  + title + "</TITLE></HEAD>");
        out.println("<BODY BGCOLOR='#FFFFCC'><CENTER><H1>" + title + "</H1>");
        out.println("<H3>" + subtitle + "</H3>");
        out.println("<I>" + scenedesc + "</I></CENTER><BR><HR>");
        out.println(mainSW.toString());
        out.println("</TD></TR></TABLE><HR></BODY></HTML>");
    }

    /**
    * getServletInfo<BR>
    * Required by Servlet interface
    */

    public String getServletInfo() {
        return "Returns an HTML representation of a Shakespeare play";
    }

    // Convenience methods previously part of Controller class

    protected void setItemRendition(RuleManager rm, String pattern, String before, String after)
            throws SAXException
    {
        rm.setHandler(pattern, new ItemRenderer(before, after));
    }

    protected void setGroupRendition(RuleManager rm, String pattern, String before, String between, String after)
            throws SAXException
    {
        rm.setHandler(pattern, new GroupRenderer(before, between, after));
    }

    // inner classes

    private class PlayHandler extends NodeHandler {

        public void start(NodeInfo e, Context c) throws SAXException {
            c.getController().applyTemplates(c, Expression.make("PERSONAE"), null, null);
            c.getController().applyTemplates(c, Expression.make("ACT"), null, null);
        }

    }

    protected class ItemRenderer extends NodeHandler {
        Expression before;
        Expression after;

        public ItemRenderer(String bef, String aft) throws SAXException {
            before = AttributeValueTemplate.make(bef);
            after = AttributeValueTemplate.make(aft);
        }

        public void start(NodeInfo e, Context c) throws SAXException {
            c.getOutputter().write(before.evaluateAsString(c));
            c.getController().applyTemplates(c, null, null, null);
            c.getOutputter().write(after.evaluateAsString(c));
        }
    }

    protected class GroupRenderer extends NodeHandler {
        Expression before;
        Expression between;
        Expression after;

        public GroupRenderer(String bef, String bet, String aft) throws SAXException {
            before = AttributeValueTemplate.make(bef);
            between = AttributeValueTemplate.make(bet);
            after = AttributeValueTemplate.make(aft);
        }

        public void start(NodeInfo e, Context c) throws SAXException {
            NodeInfo prev = e;
            while (true) {
                prev = (NodeInfo)prev.getPreviousSibling();
                if (prev==null) break;
                if (prev instanceof ElementInfo) break;
            }
            
            if (prev==null || !prev.getExpandedName().equals(e.getExpandedName())) {
                c.getOutputter().write(before.evaluateAsString(c));
            } else {
                c.getOutputter().write(between.evaluateAsString(c));
            }
            
            c.getController().applyTemplates(c, null, null, null);

            NodeInfo next = e;
            while (true) {
                next = (NodeInfo)next.getNextSibling();
                if (next==null) break;
                if (next instanceof ElementInfo) break;
            }
            if (next==null || !next.getExpandedName().equals(e.getExpandedName())) {
                c.getOutputter().write(after.evaluateAsString(c));
            }

        }
    }
    

}
