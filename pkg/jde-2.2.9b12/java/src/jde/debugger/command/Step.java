/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.2 $
 */

package jde.debugger.command;
import com.sun.jdi.request.StepRequest;
import jde.debugger.JDEException;
import com.sun.jdi.ThreadReference;
import jde.debugger.Etc;
import com.sun.jdi.request.EventRequestManager;
import java.util.List;
import java.util.Iterator;



/**
 * 'step' command. This is only possible if the current thread is
 * suspended.
 * <p>
 *
 * <b>Syntax:</b>
 * <pre>
 * step <u>type</u> threadID
 *      [{@link Etc#getSuspendPolicyFromArgs(List) suspend-policy}]
 * </pre>
 *
 * <b>Comments:</b>
 * <ul>
 * <li> <u>type</u> is one of "over", "into", "into-all" and "out"
 * There are three kinds of steps that are being supported currently:
 * <ul>
 * <li> step over: steps over method calls</li>
 * <li> step into: goes into called methods except java and sun methods</li>
 * <li> step into-all: goes into all methods except excluded methods</li>
 * <li> step out: executes till it returns to the calling method</li>
 * </ul>
 *
 * <li>
 *	threadID is required. Also, this is only possible on a
 *	suspended thread. jde should check this before calling
 *	step.
 * </li>
 * </ul>
 *
 * <p>
 * @see jde.debugger.EventHandler#stepEvent(StepEvent)
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.2 $
*/
public class Step extends DebugProcessCommand {
  
  /**
   *
   * @exception jde.debugger.JDEException <description>
   */
  public void doCommand() throws JDEException {

    if (args.size() < 2)
      throw new JDEException("Insufficient arguments");
	
    // ascertain the kind of step
    String arg = args.remove(0).toString().toLowerCase();
    int depth;
    boolean into_all = false;
    if (arg.equals("over")) {
      depth = StepRequest.STEP_OVER;
    } else if (arg.equals("out")) {
      depth = StepRequest.STEP_OUT;
    } else if (arg.equals("into")) {
      depth = StepRequest.STEP_INTO;
    } else if (arg.equals("into-all")) {
      depth = StepRequest.STEP_INTO;
      into_all = true;
    }else {
      throw new JDEException("Syntax error: use step over/out/into");
    }

    // find the thread on which to step
    Long uniqueID = Etc.safeGetLong(args.remove(0), "thread ID");
	
    ThreadReference tRef = (ThreadReference) proc.getStore().get(uniqueID);
	    
    // it should exist
    if (tRef == null) {
      throw new JDEException("Invalid thread ID or the thread is dead");
    }

    // we need to be suspended.
    // also see ThreadCommands.getThreadStringRep for some info
    if (tRef.suspendCount() == 0) {
      throw new  JDEException("The specified thread is not suspended");
    }

    // clear any previous steps on this thread
    clearPreviousStep(tRef);

    // set a new request!
    EventRequestManager erm = proc.getVM().eventRequestManager();
    StepRequest request =
      erm.createStepRequest(tRef, StepRequest.STEP_LINE, depth);
    request.setSuspendPolicy(Etc.getSuspendPolicyFromArgs(args));

    if (depth == StepRequest.STEP_INTO) 
      if (into_all) {
	
      } else {
	request.addClassExclusionFilter("java.*");
	request.addClassExclusionFilter("javax.*");     
	request.addClassExclusionFilter("sun.*"); 
      }

    // a single step event... will set it again if need be.
    request.addCountFilter(1);
    request.enable();

    // and now resume the vm. the thread suspended is resumed now.
    proc.getVM().resume();

    jde.signalCommandResult(procID, cmdID);
  }


  /**
   * Clear a previous step request on this thread: only one is allowed
   * per thread
   */
  private void clearPreviousStep(ThreadReference thread) {
    synchronized (proc) {
      EventRequestManager mgr = proc.getVM().eventRequestManager();
      List requests = mgr.stepRequests();
      Iterator iter = requests.iterator();
      while (iter.hasNext()) {
	StepRequest request = (StepRequest)iter.next();
	ThreadReference requestThread =  request.thread();
	if (request.thread().equals(thread)) {
	  mgr.deleteEventRequest(request);
	  break;
	}
      }
    }
  }

  public Object clone() {return new Step();}
  
} // Step

/*
 * $Log: Step.java,v $
 * Revision 1.2  2001/03/24 05:42:37  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.1  2000/08/14 02:40:40  paulk
 * Initial revision.
 *
 *
 *
 */

// End of Step.java
