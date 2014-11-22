import com.icl.saxon.*;
import com.icl.saxon.handlers.*;
import com.icl.saxon.expr.*;
import com.icl.saxon.output.*;
import com.icl.saxon.sort.*;
import com.icl.saxon.om.*;
import com.icl.saxon.tree.Builder;
import com.icl.saxon.tree.Stripper;
import org.xml.sax.SAXException;
import java.util.*;
import java.io.*;

/**
  * Class ShowBooks:
  * This class produces an HTML rendition of a List of Books supplied in XML.
  * It is intended to be run with the input file books.xml (which is
  * adapted from the MSXML distribution).
  *
  * @author Michael H. Kay (Michael.Kay@icl.com)
  * @version 20 September 2000
  */
  
public class ShowBooks extends Controller
{
            // table of category codes and descriptions

    private Hashtable categories = new Hashtable();
    
    /**
      * main()<BR>
      * Expects one argument, the input filename<BR>
      * It produces an HTML rendition on the standard output<BR>
      */

    public static void main (String args[])
    throws java.lang.Exception
    {
        // Check the command-line arguments

        if (args.length != 1) {
            System.err.println("Usage: java ShowBooks input-file >output-file");
            System.exit(1);
        }
        ShowBooks app = new ShowBooks();

        // Set up element handlers
        RuleManager rm = new RuleManager();
        app.setRuleManager(rm);
        app.prepare(rm);

        // Set up output destination details
        OutputDetails details = new OutputDetails();
        details.setWriter(new BufferedWriter(new PrintWriter(System.out)));
        details.setMethod("html");
        details.setIndent("yes");
        details.setEncoding("iso-8859-1");
        app.setOutputDetails(details);
        
        // Build the source document tree, stripping whitespace nodes
        Builder builder = new Builder();
        Stripper stripper = new Stripper();
        stripper.setPreserveSpace(new AnyNameTest(), false);
        builder.setStripper(stripper);
        DocumentInfo doc = builder.build(new ExtendedInputSource( new File(args[0]) ));

        // run the parse, calling registered handlers as appropriate
        app.run(doc); 

        // close the output file
        app.resetOutputDetails();
        
        System.exit(0);
    }

    /**
    * Register the element handlers
    */

    public void prepare(RuleManager rm) throws SAXException {

        // define how each XML element type should be handled

        rm.setHandler( "BOOKLIST", new BookListHandler());

        rm.setHandler( "BOOKS", new BooksHandler() );
        
        rm.setHandler( "ITEM", new ItemHandler() );
                         
        rm.setHandler( "CATEGORY", new CategoryHandler() );
                         
    }

    /////////////////////////////////////////////////////////////////////////////
    // INNER CLASSES
    /////////////////////////////////////////////////////////////////////////////

    /**
    * Handler for the BOOKLIST element (the outermost element)
    */

    private class BookListHandler extends ElementHandlerBase {

        public void startElement(ElementInfo e, Context c) throws SAXException {
            Expression categories = Expression.make("//CATEGORY");
            Expression books = Expression.make("BOOKS");

            // process the categories
            c.getController().applyTemplates(c, categories, null, null);

            // process the books
            c.getController().applyTemplates(c, books, null, null);
        }
    }

    /**
    * Handler for the BOOKS element.
    * This extends ItemRenderer, which has the capability to display an HTML string before and
    * after the element content.
    */

    private class BooksHandler extends ElementHandlerBase {

        String before = "<HTML><HEAD><TITLE>Book List</TITLE></HEAD>\n" +
                        "<BODY><H1>Book List</H1><TABLE BORDER='1'>\n" +
                        "<TR><TH>Category</TH><TH>Author</TH><TH>Title</TH>" +
                        "<TH>Publisher</TH><TH>Quantity</TH><TH>Price</TH></TR>";
        String after = "</TABLE></BODY></HTML>";
        
        public void startElement(ElementInfo e, Context c) throws SAXException {

            Controller ctrl = c.getController();
            
            // write the "before" string
            ctrl.getOutputter().write(before);

            // create an expression to select the child elements of this node and sort them
            NodeSetExtent children = new NodeSetExtent(e.getAllChildNodes());
            SortedSelection sortedChildren = new SortedSelection(children);
            
            SortKeyDefinition sk1 = new SortKeyDefinition();
            sk1.setSortKey(Expression.make("AUTHOR"));
            sortedChildren.addSortKey(sk1);
            
            SortKeyDefinition sk2 = new SortKeyDefinition();
            sk2.setSortKey(Expression.make("TITLE"));
            sortedChildren.addSortKey(sk2);

            // process the nodes selected by this expression
            ctrl.applyTemplates(c, sortedChildren, null, null);

            // write the "after" string
            ctrl.getOutputter().write(after);

        }

    }

    /**
    * CategoryHandler keeps track of category codes and descriptions
    * in a local hash table for use while processing the book details
    */

    private class CategoryHandler extends ElementHandlerBase {
        public void startElement( ElementInfo e, Context context )
        throws SAXException {            
            String code = e.getAttributeValue("CODE");
            String desc = e.getAttributeValue("DESC");
            categories.put(code, desc);
        }
    }

    /**
    * Handler for ITEM elements (representing individual books)
    */

    private class ItemHandler extends ElementHandlerBase {

        public void startElement( ElementInfo e, Context c ) throws SAXException {
            Outputter out = c.getOutputter();
            Name tr = new Name("TR");
            out.writeStartTag(tr);
            writeEntry(out, categories.get(e.getAttributeValue("CAT")).toString());
            writeEntry(out, Expression.make("AUTHOR").evaluateAsString(c));
            writeEntry(out, Expression.make("TITLE").evaluateAsString(c));
            writeEntry(out, Expression.make("PUBLISHER").evaluateAsString(c));
            writeEntry(out, Expression.make("QUANTITY").evaluateAsString(c));
            writeEntry(out, Expression.make("PRICE").evaluateAsString(c));
            out.writeEndTag(tr);
        }

        private void writeEntry(Outputter out, String val) throws SAXException {
            Name td = new Name("TD");
            out.writeStartTag(td);
            out.writeContent(val);
            out.writeEndTag(td);
        }
        
    }          			
} 
