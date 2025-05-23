package com.icl.saxon.trace;
import org.xml.sax.SAXException;
import com.icl.saxon.om.NodeInfo;
import com.icl.saxon.handlers.NodeHandler;
import com.icl.saxon.Context;

import java.util.EventListener;

/**
 * A class which implements efficient and thread-safe multi-cast event 
 * dispatching for the TraceListener evants.
 *
 * Grabbed from java.awt.AWTEventMulticaster
 */
public class SaxonEventMulticaster implements TraceListener {

  protected final EventListener a, b;

  /**
   * Creates an event multicaster instance which chains listener-a
   * with listener-b.
   * @param a listener-a
   * @param b listener-b
   */ 
  protected SaxonEventMulticaster(EventListener a, EventListener b) {
    this.a = a; this.b = b;
  }

  /**
   * Removes a listener from this multicaster and returns the
   * resulting multicast listener.
   * @param oldl the listener to be removed
   */
  protected EventListener remove(EventListener oldl) {
    if (oldl == a)  return b;
    if (oldl == b)  return a;
    EventListener a2 = removeInternal(a, oldl);
    EventListener b2 = removeInternal(b, oldl);
    if (a2 == a && b2 == b) {
	return this;	// it's not here
    }
    return addInternal(a2, b2);
  }

  /**
  * Called at start
  */

  public void open() {
    ((TraceListener)a).open();
    ((TraceListener)b).open();
  }

  /**
  * Called at end
  */

  public void close() {
    ((TraceListener)a).close();
    ((TraceListener)b).close();
  }
  

  /**
   * Called for all top level elements
   */
  public void toplevel(NodeInfo element)
    throws SAXException
  {
    ((TraceListener)a).toplevel(element);
    ((TraceListener)b).toplevel(element);
  }

  /**
   * Called when a node of the source tree gets processed
   */
  public void enterSource(NodeHandler handler, Context context)
    throws SAXException
  {
    ((TraceListener)a).enterSource(handler, context);
    ((TraceListener)b).enterSource(handler, context);
  }

  /**
   * Called after a node of the source tree got processed
   */
  public void leaveSource(NodeHandler handler, Context context)
    throws SAXException
  {
    ((TraceListener)a).leaveSource(handler, context);
    ((TraceListener)b).leaveSource(handler, context);
  }

  /**
   * Called when an element of the stylesheet gets processed
   */
  public void enter(NodeInfo element, Context context)
    throws SAXException
  {
    ((TraceListener)a).enter(element, context);
    ((TraceListener)b).enter(element, context);
  }

  /**
   * Called after an element of the stylesheet got processed
   */
  public void leave(NodeInfo element, Context context)
    throws SAXException
  {
    ((TraceListener)a).leave(element, context);
    ((TraceListener)b).leave(element, context);
  }

  /**
   * Adds trace-listener-a with trace-listener-b and
   * returns the resulting multicast listener.
   * @param a trace-listener-a
   * @param b trace-listener-b
   */
  public static TraceListener add(TraceListener a, TraceListener b) {
    return (TraceListener)addInternal(a, b);
  }

  /**
   * Removes the old trace-listener from trace-listener-l and
   * returns the resulting multicast listener.
   * @param l trace-listener-l
   * @param oldl the trace-listener being removed
   */
  public static TraceListener remove(TraceListener l, TraceListener oldl) {
    return (TraceListener) removeInternal(l, oldl);
  }

  /** 
   * Returns the resulting multicast listener from adding listener-a
   * and listener-b together.  
   * If listener-a is null, it returns listener-b;  
   * If listener-b is null, it returns listener-a
   * If neither are null, then it creates and returns
   * a new EventMulticaster instance which chains a with b.
   * @param a event listener-a
   * @param b event listener-b
   */
  protected static EventListener addInternal(EventListener a, EventListener b) {
    if (a == null)  return b;
    if (b == null)  return a;
    return new SaxonEventMulticaster(a, b);
  }
  
  /** 
   * Returns the resulting multicast listener after removing the
   * old listener from listener-l.
   * If listener-l equals the old listener OR listener-l is null, 
   * returns null.
   * Else if listener-l is an instance of SaxonEventMulticaster, 
   * then it removes the old listener from it.
   * Else, returns listener l.
   * @param l the listener being removed from
   * @param oldl the listener being removed
   */
  protected static EventListener removeInternal(EventListener l, EventListener oldl) {
    if (l == oldl || l == null) {
	return null;
    } else if (l instanceof SaxonEventMulticaster) {
	return ((SaxonEventMulticaster)l).remove(oldl);
    } else {
	return l;		// it's not here
    }
  }

}

// Contributor(s): 
// This module is from Edwin Glaser (edwin@pannenleiter.de)
//
