/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;


import jde.debugger.JDEException;
import jde.debugger.Etc;
import com.sun.jdi.ObjectReference;
import jde.debugger.Rep;
import com.sun.jdi.ThreadReference;
import com.sun.jdi.StackFrame;
import com.sun.jdi.IncompatibleThreadStateException;
import com.sun.jdi.ObjectCollectedException;
import jde.debugger.LispForm;
import com.sun.jdi.AbsentInformationException;
import com.sun.jdi.NativeMethodException;
import jde.debugger.command.DebugProcessCommand;



/**
 * Stops the VM and debug the specified thread.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * debug_thread threadID
 * </pre>
 *
 * @author Raffael Herzog
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
 */
public class DebugThread extends DebugProcessCommand {
  
    /**
     *
     * @exception jde.debugger.JDEException <description>
     */
    public void doCommand() throws JDEException {

        if (args.size() < 1)
            throw new JDEException("Insufficient arguments");

        // find the thread to debug
        Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");
      
        ThreadReference tRef = (ThreadReference) proc.getStore().get(uniqueID);
        
        // it should exist
        if (tRef == null) {
            throw new JDEException("Invalid thread ID or the thread is dead");
        }
       
        // suspend the whole vm
        proc.getVM().suspend();

        // simulate a step event
        try {
            final LispForm locationRep = Rep.getLocationRep(tRef.frame(0).location());
            final LispForm lispForm = new LispForm("(list '"
                                                   +EVENT_STEP_COMPLETED
                                                   +" "+locationRep
                                                 +")");

            jde.signal(procID, EVENTSET, 
			new LispForm("\"thread\" " + 
				     Rep.getThreadRep(tRef, 
						      proc.getStore()) + BR + lispForm));
            jde.signalCommandResult(procID, cmdID);
        }
        catch ( IncompatibleThreadStateException exc ) {
            // this should never happen...
            throw new JDEException(exc.toString());
        }
    }

    public Object clone() {return new DebugThread();}
  
} // DebugThread

/*
 * $Log: DebugThread.java,v $
 * Revision 1.2  2001/07/07 04:51:35  paulk
 * Removed DOS line endings.
 *
 * Revision 1.1  2001/07/06 02:04:50  paulk
 * Initial revision.
 *
 *
 *
 */

// End of DebugThread.java
