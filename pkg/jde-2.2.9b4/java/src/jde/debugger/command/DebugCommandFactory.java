/*
 * Copyright (c) 2000, 2001    Paul Kinnucan
 *
 * $Revision: 1.8 $
 */

package jde.debugger.command;

import java.util.HashMap;
import java.util.List;
import java.lang.InstantiationException;
import java.lang.ClassNotFoundException;
import java.lang.IllegalAccessException;
import jde.debugger.JDEException;



/**
 * DebugCommandFactory.java
 *
 *
 * Created: Fri Jan 28 22:04:57 2000
 *
 * @author Paul Kinnucan
 * @version $Revision: 1.8 $
 */

public class DebugCommandFactory  {
 
  protected DebugCommandFactory() {

    prototypes.put("launch", new LaunchApplication());
    prototypes.put("attach_shmem", new AttachShmem());
    prototypes.put("attach_socket", new AttachSocket());
    prototypes.put("listen_shmem", new ListenShmem());
    prototypes.put("listen_socket", new ListenSocket());
    prototypes.put("quit", new Quit());
    prototypes.put("run", new Run());
    prototypes.put("finish", new Finish());
    prototypes.put("get_string", new GetString());
    prototypes.put("get_array", new GetArray());
    prototypes.put("get_locals", new GetLocals());
    prototypes.put("get_this", new GetThis());
    prototypes.put("get_object", new GetObject());
    prototypes.put("get_loaded_classes", new GetLoadedClasses());
    prototypes.put("get_path_information", new GetPathInfo());
    prototypes.put("trace_classes", new TraceClasses());
    prototypes.put("cancel_trace_classes", new CancelTraceClasses());
    prototypes.put("trace_exceptions", new TraceExceptions());
    prototypes.put("trace_methods", new TraceMethods());
    prototypes.put("cancel_trace_methods", new CancelTraceMethods());
    prototypes.put("evaluate", new EvaluateExpression());
    prototypes.put("watch", new Watch());
    prototypes.put("break", new Break());
    prototypes.put("clear", new Clear());
    prototypes.put("step", new Step());
    prototypes.put("suspend", new Suspend());
    prototypes.put("resume", new Resume());
    prototypes.put("interrupt", new Interrupt());
    prototypes.put("kill_thread", new KillThread());
    prototypes.put("get_threads", new GetThreads());
    prototypes.put("get_thread", new GetThread());
    prototypes.put("get_object_monitors", new GetObjectMonitors());
    prototypes.put("trace_threads", new TraceThreads());
    prototypes.put("cancel_trace_threads", new CancelTraceThreads());
    prototypes.put("debug_thread", new DebugThread());
  }

  public final DebugCommand createCommand(Integer procID, Integer cmdID, 
					  String cmdName, List args)   
                            throws JDEException {

    DebugCommand prototype = (DebugCommand) prototypes.get(cmdName);

    if (prototype == null) return null;

    DebugCommand cmd = (DebugCommand) prototype.clone();
    cmd.init(procID, cmdID, cmdName, args);
    
    return cmd;

  }

  private HashMap prototypes = new HashMap();
 
  public static DebugCommandFactory theFactory = new DebugCommandFactory();
  
} // DebugCommandFactory


/*
 * $Log: DebugCommandFactory.java,v $
 * Revision 1.8  2001/07/06 02:05:51  paulk
 * Makefile
 *
 * Revision 1.7  2001/03/24 05:42:36  paulk
 * Updated to reflect reorganization of debugger code.
 *
 * Revision 1.6  2000/10/20 04:19:00  paulk
 * *** empty log message ***
 *
 * Revision 1.5  2000/07/28 06:27:02  paulk
 * Committing all modified files.
 *
 * Revision 1.4  2000/04/10 05:36:50  paulk
 * Added get_locals and get_this commands.
 *
 * Revision 1.3  2000/03/03 07:40:32  paulk
 * Converted get_string and get_array commands from functions to objects.
 *
 * Revision 1.2  2000/01/31 12:41:45  paulk
 * * Continue converting commands from functional to OO implementation.
 *
 * Revision 1.1  2000/01/30 12:35:20  paulk
 * Creates debugger commands.
 *
 */

// End of DebugCommandFactory.java
