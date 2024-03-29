import java.io.*;
import javax.servlet.*;
import javax.servlet.http.*;
import java.util.Hashtable;
import java.util.Enumeration;

import com.icl.saxon.trax.Processor;
import com.icl.saxon.trax.Templates;
import com.icl.saxon.trax.Transformer;
import com.icl.saxon.trax.Result;
import com.icl.saxon.trax.serialize.OutputFormat;
import com.icl.saxon.ExtendedInputSource;
//import com.icl.saxon.output.*;
import com.icl.saxon.expr.StringValue;
import org.xml.sax.*;

/**
 * SaxonServlet. Transforms a supplied input document using a supplied stylesheet
 */
 
public class SaxonServlet extends HttpServlet {

    /**
    * service() - accept request and produce response<BR>
    * URL parameters: <UL>
    * <li>source - URL of source document</li>
    * <li>style - URL of stylesheet</li>
    * <li>clear-stylesheet-cache - if set to yes, empties the cache before running.
    * </UL>
    * @param req The HTTP request
    * @param res The HTTP response
    */ 
    
    public void service(HttpServletRequest req, HttpServletResponse res)
	throws ServletException, IOException
    {
        String source = req.getParameter("source");
        String style = req.getParameter("style");
        String clear = req.getParameter("clear-stylesheet-cache");

        if (clear!=null && clear.equals("yes")) {
            clearCache();
        }

        try {
            apply(style, source, req, res);
        } catch (SAXException err) {
            res.getOutputStream().println("Error applying stylesheet: " + err.getMessage());         
        }

    }

    /**
    * getServletInfo<BR>
    * Required by Servlet interface
    */

    public String getServletInfo() {
        return "Calls SAXON to apply a stylesheet to a source document";
    }

    /**
    * Apply stylesheet to source document
    */

    private void apply(String style, String source,
                           HttpServletRequest req, HttpServletResponse res)
                           throws SAXException, java.io.IOException {
                            
        ServletOutputStream out = res.getOutputStream();

        if (style==null) {
            out.println("No style parameter supplied");
            return;
        }
        if (source==null) {
            out.println("No source parameter supplied");
            return;
        }
        try {
            Templates pss = tryCache(style);
            Transformer ssi = pss.newTransformer();
            OutputFormat details = pss.getOutputFormat();

            // Use the following line only if there is an explicit xsl:output method
            // res.setContentType(details.getMediaType());
            res.setContentType("text/html");

            Enumeration p = req.getParameterNames();
            while (p.hasMoreElements()) {
                String name = (String)p.nextElement();
                if (!(name.equals("style") || name.equals("source"))) {
                    String value = req.getParameter(name);
                    ssi.setParameter(name, "", new StringValue(value));
                }
            }
            
            String path = getServletContext().getRealPath(source);
            if (path==null) {
                throw new SAXException("Source file " + source + " not found");
            }
            File sourceFile = new File(path);
            ssi.transform(new ExtendedInputSource(sourceFile), new Result(out));
        } catch (Exception err) {
            out.println(err.getMessage());
            err.printStackTrace();
        }

    }

    /**
    * Maintain prepared stylesheets in memory for reuse
    */

    private synchronized Templates tryCache(String url) throws SAXException, java.io.IOException {
        String path = getServletContext().getRealPath(url);
        if (path==null) {
            throw new SAXException("Stylesheet " + url + " not found");
        }
        
        Templates x = (Templates)cache.get(path);
        if (x==null) {
            Processor processor = Processor.newInstance("xslt");
            x = processor.process(new ExtendedInputSource(new File(path)));
            cache.put(path, x);
        }
        return x;
    }

    /**
    * Clear the cache. Useful if stylesheets have been modified, or simply if space is
    * running low. We let the garbage collector do the work.
    */

    private synchronized void clearCache() {
        cache = new Hashtable();
    }

    private Hashtable cache = new Hashtable();
}
