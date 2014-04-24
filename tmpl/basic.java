§rem -*-java-*-
§rem ; Hi-lock: (("^§[a-z]*" (0 (quote hi-yellow) t)))
§rem /* JAVA source file template - LRE, 980410-030913*/
§default notttused (eval (progn (make-variable-buffer-local 'tplsub-enable-eval) (setq tplsub-enable-eval t) ""))
§rem ' to satisfy emacs
§default className (eval (if buffer-file-name (file-name-nondirectory (file-name-sans-extension buffer-file-name)) "MyClass"))
§ask className Class Name
§rem type --------------------------------------------------------------
§define strAbs
§if isInterface Is this an interface
§define clAcc public
§defbool isApplet nil
§defbool isApplic nil
§defbool isClass nil
§defbool notInterf nil
§define strClass interface
§else
§define strClass class
§defbool notInterf t
§if isClass Is this a normal class (not applet/app)
§defbool isApplet nil
§defbool isApplic nil
§select clAcc u public o protected i private 0
§if isAbstract Abstract class
§define strAbs abstract
§fi
§else
§define strAbs
§if isApplet Is this an applet (not an application)
§defbool isApplic nil
§define clAcc public
§else
§defbool isApplic t
§define clAcc public
§fi isApplet
§fi isClass
§fi isInterface
§rem type end ----------------------------------------------------------
§if isInPkg Is in package
§ask pkgName Name of package
package §pkgName§;
§fi isInPkg
§rem include suggestions -----------------------------------------------
§if useNet Use java.net
§for include URL URLConnection MalformedURLException *
§if inclNET§include§ Include §include§
import java.net.§include§;
§fi inclNET§include§
§endfor include
§fi useNet
§if isInterface
§defbool useAwt nil
§else
§if useAwt Use AWT
§for include Event Font *
§if inclAWT§include§ Include §include§
import java.awt.§include§;
§fi inclAWT§include§
§endfor include
§fi useAwt
§fi isInterface
§if useIO Use java.io
§for include URL DataInputStream BufferedInputStream IOException *
§if inclIO§include§ Include §include§
import java.io.§include§;
§fi inclIO§include§
§endfor include
§fi useIO
§if inclPaint Include paint method
import java.awt.Graphics;
§fi inclPaint
§if useCollections Use collections
§for include List Vector ArrayList HashMap *
§if inclUtil§include§ Include §include§
import java.util.§include§;
§fi inclUtil
§endfor include
§fi useCollections
§rem -------------------------------------------------------------------
§if bRunnable Use threads
§defbool inclStart nil
§defbool inclStop nil
§fi bRunnable

/* ------------------------------------------------------------------------- */
§if isClass
§else
§if isInterface
§else
§if isApplet
// Applet: §className§
§else
// Application: §className§
§fi isApplet
/* ------------------------------------------------------------------------- */

§fi isInterface
§fi isClass
/**
§ask Description
 *
 * §Description§
 *
§ask author
 * @author §author§
§ask Version
 * @version §Version§
§ask Irfdate Creation date
 * @created §Irfdate§
 **/

§§§clAcc§ §strAbs§ §strClass§ §className§
§if isApplet
        extends java.applet.Applet
§else
§if useAwt
        extends Frame
§defbool hasBaseClass t
§define baseClass Frame
§else
§if hasBaseClass Has a base class
§ask baseClass Base class name
        extends §baseClass§
§fi hasBaseClass
§fi useAwt
§fi isApplet
§if notInterf
        implements
§if bRunnable
            Runnable,
§fi
§if bComparable Implement comparable
            Comparable,
§fi bComparable
§if bSerialize Implement serializable
            Serializable,
§fi bSerialize
§if bCloneable Implement cloneable
            Cloneable,
§fi bSerialize
§fi notInterf
          {
§include stdjava.inc

§if useAwt
§if inclBuildConst Include buildConstraints
    /**
     * Set constraints for one item
     **/
    void buildConstraints(GridBagConstraints gbc, int gx, int gy, int gw,
                          int gh, int wx, int wy) {
       gbc.gridx=      gx;
       gbc.gridy=      gy;
       gbc.gridwidth=  gw;
       gbc.gridheight= gh;
       gbc.weightx=    wx;
       gbc.weighty=    wy;
    } // buildConstraints()

    /**
     * Set constraints for one item
     **/
    void buildConstraints(GridBagConstraints gbc, int gx, int gy, int gw,
                          int gh, int wx, int wy, int fi, int an) {
       buildConstraints(gbc, gx, gy, gw, gh, wx, wy);
       gbc.fill=       fi;
       gbc.anchor=     an;
    } // buildConstraints()

§fi buildConstraints
§fi useAwt
§if notInterf
§do numConstr Number of constructors
    /**
     * Constructor #§R§ - ...
     **/
    §className§() {
    } // §className§()

§done numConstr
§do numStatInit Number of static initializers (factories)
§ask statInit§R§ Name of initializer §R§ (e.g. getInstance/valueOf)
    /**
     * §className§ factory method ...
     * @return instance of §className§ ...
     **/
    public static §className§ §statInit§R§§() {
    } // §statInit§R§§

§done numStatInit
§fi notInterf
§do numAccessors Number of additional set/get-pairs
§ask dField§R§ Field name for pair #§R§
§ask dType§R§ Datatype for pair #§R§
    /**
     * Get the value of §dField§R§§.
     * @return Copy of §dField§R§§
     * @see #set§dField§R§§
     **/
§if isInterface
    public §dType§R§§ get§dField§R§§();
§else
    public §dType§R§§ get§dField§R§§() {
       return §dField§R§§;
    } // get§dField§R§§
§fi isInterface

    /**
     * Set the value of §dField§R§§.
     * @param new§dField§R§§ New value of §dField§R§§
     * @see #get§dField§R§§
     **/
§if isInterface
    public void set§dField§R§§(§dType§R§§ new§dField§R§§);
§else
    public void set§dField§R§§(§dType§R§§ new§dField§R§§) {
       §dField§R§§= new§dField§R§§;
    } // set§dField§R§§
§fi isInterface

§done
§if inclPaint
    /**
     * Default Paint method...
     *
     * @param g Graphic context
     **/
§if isInterface
    public void paint(Graphics g);
§else
    public void paint(Graphics g) {
       g.drawString("");
    } // paint()
§fi isInterface

§fi inclPaint
§if useAwt
§if inclAWTevHandler Include AWT event handler (action())
    /**
     * Handle events
     *
     * @param evt Event that triggered the action
     * @param arg Additional event information
     * @return true if the event is handled, false to pass it on
     **/
    public boolean action(Event evt, Object arg) {
       if (evt.target instanceof Button) {
          String lab=(String) arg;
          if ( lab.equals("xxx") )
             ...
       }
       return false;
    } // action()

§fi inclAWTevHandler
§fi useAwt
§if inclEvHandler Include general event handler
    /**
     * General event handler
     *
     * @param evt event data
     * @param x x-coordinate
     * @param y y-coordinate
     * @return true if event is handled, otherwise false
     **/
§if isInterface
    public boolean handleEvent(Event evt);
§else
    public boolean handleEvent(Event evt) {
  : if (evt.shift/meta/controlDown()) - meta=left, control=middle :
       switch (evt.id) {
§for ev DOWN UP MOVE DRAG ENTER EXIT
          case Event.MOUSE_§ev§:
             break;
§endfor ev
§for ev PRESS RELEASE ACTION ACTION_RELEASE
          case Event.KEY_§ev§:
             break;
§endfor ev
§for ev ABSOLUTE LINE_DOWN LINE_UP PAGE_DOWN PAGE_UP
          case Event.SCROLL§ev§:
             break;
§endfor ev
§for ev DESTROY EXPOSE ICONIFY DEICONIFY MOVED
          case Event.WINDOW_§ev§:
             break;
§endfor ev
§for ev ACTION_EVENT GOT_FOCUS LOST_FOCUS LIST_SELECT LIST_DESELECT
          case Event.§ev§:
             break;
§endfor ev
       }
       return super.handleEvent(evt);
    } // handleEvent()
§fi isInterface

§fi inclEvHandler
§if inclMouse Include mouse events
  // Mouse event handlers
  : if (evt.shift/meta/controlDown()) - meta=left, control=middle :
§for dir Down Up Move Drag Enter Exit
    /**
     * <desc>
     *
     * @param evt event data
     * @param x x-coordinate
     * @param y y-coordinate
     * @return true if event is handled, otherwise false
     **/
§if isInterface
    public boolean mouse§dir§(Event evt, int x, int y);
§else
    public boolean mouse§dir§(Event evt, int x, int y) {
       return false;  // Allow other processing
    } // mouse§dir§()
§fi isInterface

§endfor dir
§fi inclMouse
§if inclKeys Include keyboard events
// Keyboard event handlers
  : Event.HOME/END/PGUP/PGDN/UP/DOWN/LEFT/RIGHT/F1..F12 :
  : evt.shift/meta/controlDown() :
§for dir Down Up
    /**
     * <desc>
     *
     * @param evt event data
     * @param key keycode
     * @return true if event is handled, otherwise false
     */
§if isInterface
    public boolean key§dir§(Event evt, int key);
§else
    public boolean key§dir§(Event evt, int key) {
       return false;  // Allow other processing
    } // key§dir§()
§fi isInterface

§endfor dir
§fi inclKeys
§if inclInsets Override insets()
    /**
     * Override insets ...
     *
     * @return Inset=new insets
     **/
§if isInterface
    public Insets insets();
§else
    public Insets insets() {
       return new Insets(top,left,bottom,right);
    } // insets()
§fi isInterface

§fi inclInsets
§if isApplet
§if inclInit Include init method
    /**
     * Applet load method.
     **/
    public void init() {
       setFont();
    } // init()

§fi inclInit
§if inclDestroy Include destroy method
    /**
     * Applet unload method.
     **/
    public void destroy() {
    } // destroy()

§fi inclDestroy
§fi isApplet
§if notInterf
§if bRunnable
    /**
     * Called to run the applet.
     **/
    public void run() {
       while (true) {
          repaint();
          try { Thread.sleep(1000); }
          catch (InterruptedException e)  {}
       }
    }

    /**
     * Runs each time the applet (re)starts.
     **/
    public void start() {
       while (proc1==null) {
          proc1= new Thread(this);
          proc1.start();
       }
    }

    /**
     * Runs each time the applet stops.
     **/
    public void stop() {
       while (proc1!=null) {
          proc1.stop();
          proc1= null;
       }
    }

§else bRunnable
§if isApplet
§if inclStart Include start method
    /**
     * Runs each time the thread (re)starts.
     **/
    public void start() {
    } // start()

§fi inclStart
§if inclStop Include stop method
    /**
     * Runs each time the thread stops.
     **/
    public void stop() {
    } // stop()

§fi inclStop
§fi isApplet
§fi bRunnable
§if isApplet
§if inclInfo Override getAppletInfo()
    /**
     * Return applet infomation...
     *
     * @return Applet info string
     **/
    public String getAppletInfo() {
       return "";
    } // getAppletInfo()

§fi inclInfo
§fi isApplet
§fi notInterf

§doR noXmeth No. of other methods
§ask methName§R§ Method name #§R§
§select methAcc§R§ u public o protected i private 0 ""
    /**
§ask methDesc§R§ Description #§R§
     * §methDesc§R§§
     *
§define mpStr§R§
§define mxStr§R§
§define mxtStr§R§
§doP noMethPar§R§ No. of parameters
§ask mparName§R§_§P§ Parameter name §P§
     * @param §mparName§R§_§P§§ ...
§append mpStr§R§ , §mparName§R§_§P§§
§doneP noMethPar
§if mhasRet§R§ Has return type
§ask mretType§R§ Return type
     * @return ...
§else
§define mretType§R§ void
§fi mhasRet
§if mhasThrow§R§ Throws exceptions
§define mxtStr§R§ §_§throws
§doX noMethExc§R§ No. of exceptions
§ask mexc§R§ Exception #§R§
§append mxStr§R§ , §mexc§R§§
     * @throws §mexc§R§§ ...
§doneX
§fi mhasThrow
     **/
§if mIsAbstr§R§ Abstract method
§define mAbsStr§R§ abstract§_§

§else
§define mAbsStr§R§
§fi mIsAbstr
§if isInterface
    §methAcc§R§§ §mAbsStr§R§§§mretType§R§§ §methName§R§§(§mpStr§R§§)§mxtStr§R§§ §mxStr§R§§;
§else
    §methAcc§R§§ §mAbsStr§R§§§mretType§R§§ §methName§R§§(§mpStr§R§§)§mxtStr§R§§ §mxStr§R§§ {
§if mhasRet§R§
        return ...;
§fi mhasRet
    } // §methName§R§§()
§fi isInterface

§doneR noXmeth
§if inclMain Include main method
    // ---------------------------------------------------------------------
    // /// MAIN §className§ ///
    // ---------------------------------------------------------------------
    /**
     * Main application method.
     *
     * @param args Command line arguments
     * @return ...
     **/
    public static void main (String[] args) {
§if useAwt Use AWT
        §className§ app= new §className§();
        app.resize(,);
        app.show();
§fi useAwt
        System.out.println("");
    } // main()
§fi inclMain
} // §strClass§ §className§
