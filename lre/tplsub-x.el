;;; tplsub-x.el  --- mode-specific abbreviations
;;  Variables for the modes we know about

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author:              (c) Lars Reed <Lars@kalars.net>
;; Version:             1.12
;; Keywords:            see tplsub.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
(defconst tplsub-x--mode-version "1.12")

(defvar tplsub-sysdul-tmpl-list
        '(
          ("++"    (tplsub--inc "+"))
          ("--"    (tplsub--inc "-"))
          ("0"     "False")
          ("1"     "True")
          ("2"     "@" ("Macro") "@")
          ("arr"   "declare " ("Array") " as " ("Record name")
                   " numbered from " | " to " | "<high> "
                   (yn "Global" ("global [init]")))
          ("avg"   "average " ("Element") " for " ("Entity") |)
          ("boo"   "boolean")
          ("bstrip" "string-function BfStrip " (help . "bfstrip"))
          ("call"  o "call " ("Procedure") " exporting " |
                   (: . 3) "importing" |)
          ("chr"   "@CHAR@")
          ("clof"  o "close file with unit ")
          ("conc"  "string-function Concat " (help . "concat"))
          ("dam"   o "display Acknowledge message " | " reply from "
           (help . "notvist"))
          ("dberr" o "if DB-error" / | / (= . "fi"))
          ("dbfu"  "dbfunction " (help . "dbfunctions"))
          ("dcl"   o "declare " | *)
          ("def"   "is defined")
          ("del"   o "delete " ("Entity/role") /
                   "if DB-error or not deleted" / | (= . "msg") | / (= . "fi"))
          ("delall" "delete all " ("Entity/role") " with " | /
                   "if DB-error" / (= . "msg") | / (= . "fi"))
          ("dis"   o (yn "In current dialog" ("DisableObject(" | ")")
                         ("disable " | " in " ("Dialog"))))
          ("disp"  o "display " | " in " ("Dialog" . "DialogName"))
          ("dispval" o "display value list of " | " in "
           ("Dialog" . "DialogName"))
          ("div"   (tplsub--inc "/"))
          ("dlgopen" "dialog " ("Dialog" . "DialogName") " is open")
          ("einc"  "*V END-INCLUDE  ; " (help . "includes"))
          ("elinc" "*V ELSE-INCLUDE" (help . "includes"))
          ("ena"   o (yn "In current dialog" ("EnableObject(" | ")")
                         ("enable " | " in " ("Dialog"))))
          ("eproc" "end procedure")
          ("eq"    "equal to ")
          ("evloc" (yn "In current dialog" ("TadIsLoc(" | ")")
                       ("event-location is " | " in " ("Dialog"))))
          ("evproc" "event-procedure " ("Procname") "employs "
                   ("Currency set"))
          ("evtype" "event-type is ")
          ("ewhi"  "end while" *)
          ("exp"   "exporting ")
          ("fi"    o "end if" *)
          ("FINX"  ("Entity/role")  " with " |
                   (yn "That-clause" ((: . 4) "that " | ("Relationship"))))
          ("FINE"  / "if DB-error or not found" /
                   (= . "msg") / | (= . "fi"))
          ("find"  o "find " ("Entity/role")  " with " | (: . 3)
                   (yn "That-clause" ("that " | ("Relationship") (:)))
                   "and get " | "DATA" (= . "FINE"))
          ("finf"  o "find first " (= . "FINX") (= . "FINE"))
          ("finl"  o "find last " (= . "FINX") (= . "FINE"))
          ("fna"   "@FILENAME@")
          ("fno"   "@FILENO@")
          ("fstrip" o "string-function FrontStrip " (help . "frontstrip"))
          ("getprop" o "set " | " to property " ("Property")
                     " of " | "OBJECT with " | "KEY in " ("Dialog"))
          ("gli"   "global init")
          ("glo"   "global")
          ("goot"  "restart from ")
          ("goto"  "terminate from ")
          ("ida"   o "identify all " ("Entity/role") " with " | (: . 4)
                   (yn "That-clause" ("that " ("Relationship") | (:)))
                   "select " | "LIST" (yn "Sort-clause" ((:) "sorted by " |)))
          ("IDPX"  ("Entity/role") (: . 3) "and get " | "LIST")
          ("idc"   o "identify current " (= . "IDPX"))
          ("idf"   o "identify first " (= . "IDPX"))
          ("idl"   o "identify last " (= . "IDPX"))
          ("idn"   o "identify next " (= . "IDPX"))
          ("idp"   o "identify previous " (= . "IDPX"))
          ("idx"   o "string-function Index" (help . "index"))
          ("imp"   "importing ")
          ("if"    "if " | / | (= . "fi"))
          ("inc"   "*V INCLUDE IF " | " = " (help . "includes"))
          ("ins"   o "insert " | " line " | "N in " ("Dialog" . "DialogName") |)
          ("int"   "@INT@")
          ("isalnum" "string-function IsAlNum " (help . "isalnum"))
          ("isalpha" "string-function IsAlpha " (help . "isalpha"))
          ("ischg" "value of " | " in " ("Dialog" . "DialogName") |
           " is changed")
          ("isdis" "in " ("Dialog" . "DialogName") | " is disabled")
          ("isena" "in " ("Dialog" . "DialogName") | " is enabled")
          ("islow" "string-function IsLower " (help . "isuplow"))
          ("isnum" "string-function IsNum " (help . "isnum"))
          ("isup"  "string-function IsUpper " (help . "isuplow"))
          ("ioe"   o "if " "IO" "-error" / | / (= . "fi"))
          ("labd"  "@LABEL_DB@")
          ("labe"  "@LABEL_END@")
          ("labg"  "@LABEL_GUI@")
          ("labi"  "@LABEL_IO@")
          ("labx"  "@LABEL_EXIT@")
          ("len"   "string-function Length " (help . "length"))
          ("loc"   "@LOCAL@_" ("Macro") " = ")
          ("lowc"  o "string-function LowerCase " (help . "lowercase"))
          ("mac"   "*V MACRO " ("Macro") " = ")
          ("max"   "maximum " ("Element") " for " ("Entity"))
          ("min"   "minimum " ("Element") " for " ("Entity"))
          ("MOV"   " number " | " in " ("Array") | " <var1,...,varN>")
          ("movf"  o "move from" (= . "MOV"))
          ("movt"  o "move to" (= . "MOV"))
          ("msg"   o "@MSG_INIT@" / + "W_MNO   = " ("Meldingsnummer")
                     "  ; " | "Komm..." "\n" * - "@MSG_DISPLAY@")
          ("mult"   (tplsub--inc "*"))
          ("ntch"  o "note that " | " in " ("Dialog") "is changed")
          ("ntsu"  o "note that selection for " ("Entity/role") " is undefined")
          ("ntu"   o "note that " ("Entity/role") " is undefined")
          ("ntuch" o "note that " | " in " ("Dialog" . "DialogName")
                     "is unchanged")
          ("num"   "number of " ("Entity") " with ")
          ("obt"   o "obtain value of " |
                   (yn "Line-clause" (" line " | "N "))
                   " in " ("Dialog" . "DialogName"))
          ("ofi"   o "open file      " ("Variable for name") (: . 2)
                   "with unit " ("Variable for number") (: . 1)
                   "for       ")
          ("oif"   o "or if " *)
          ("oinc"  "*V OR-INCLUDE IF " | " = " (help . "includes"))
          ("pad"   "string-function Pad " (help . "pad"))
          ("proc"  o "procedure " ("Procname") "imports " | (: . 3)
                   "exports " | "W_RETSTAT" (:) "employs " |
                   ("Currency set" . "AUTOMARKING"))
          ("prompt" o "write \'$\', " |  " to terminal")
          ("rall"  o "read all " | " from " | / "end read" *)
          ("rec"   o "define " ("Record name") " as record of ")
          ("rem"   o "remember that " ("Entity/role") " has " | /
                   "if not remembered" / (= . "msg") / (= . "fi"))
          ("rlab"  "restart-label ")
          ("rtty"  o "read " | " from terminal")
          ("setprop" o "set property " ("Property") " of " | " in "
                     ("Dialog" . "DialogName") " to " | (help . "dlgprop"))
          ("sfu"   "string-function " (help . "stringfunctions"))
          ("st0"   "@STAT_NULL@")
          ("stdb"  "@STAT_DBERR@")
          ("sterr" "@STAT_ERR@")
          ("stgui" "@STAT_GUIERR@")
          ("stio"  "@STAT_IOERR@")
          ("stok"  "@STAT_OK@")
          ("sto"   o "store " ("Entity/role") " with " | (: . 4) | "..." /
                   "if not stored" / (= . "msg") / | (= . "fi"))
          ("strip" o "string-function Strip " (help . "strip"))
          ("subs"  o "string-function SubString " (help . "substring"))
          ("sum"   "sum " ("Element") " for " ("Entity"))
          ("td"    o "TadDateToday(" ("Variabel") ")")
          ("tlab"  "terminate-label " | *)
          ("tfu"   "time-function " (help . "timefunctions"))
          ("udef"  "is undefined")
          ("upc"   o "string-function UpperCase " (help . "uppercase"))
          ("updall" o "update all " ("Entity/role") " with " | "KEYSPEC" (: . 4)
                   "set " | /
                   "if DB-error" / (= . "msg") | / (= . "fi"))
          ("vchr"  "@VARCHAR@")
          ("wend"  (= . "ewhi"))
          ("wh"    o "while " | / (= . "ewhi"))
          ("wrerr" o "write " | " to error-file"
           (help . "notvist"))
          ("wret"  "W_RETSTAT")
          ("wrtty" o "write 1X, " | " to terminal")
          ("wrvar" o "write 1X, \'" ("Variable") "=\', 1X, " ("Variable")
                   " to terminal")
          ("x"     "@X_" ("Macro") "@")
          )
        "Templates for `sysdul-mode'.")

(defvar tplsub-sysdul-help-list
        '(
          ("notvist" "Skal ikke brukes i TVIST!!!!")
          ;
          ("bfstrip" "str [res]")
          ("concat" "str1 str2 [res]")
          ("frontstrip" "str [res]")
          ("index" "str substr - returns start of substr in str")
          ("isalnum" "str - TRUE if alphanumeric only")
          ("isalpha" "str - TRUE if alphabetic only")
          ("isnum" "str - TRUE if numeric only")
          ("isuplow" "str - TRUE if upper/lowercase only")
          ("length" "str")
          ("lowercase" "str [res]")
          ("movbl" "from-string to-string form-index to-index no-of-chars")
          ("pad" "str padding startAt count [res]")
          ("strip" "str [res]")
          ("substring" "str startAt noChars [res]")
          ("uppercase" "str [res]")
          )
        "Syntax-help for `sysdul-mode'.")

(defvar tplsub-makefile-list
  '(
    ("inc"    "include ")
    ("make"   "$(MAKE)")
    ("mkh"    "$(MAKE_HOME)")
    ("mkr"    "include " (= . "mkh") "/Makerules")
    )
  "Templates for `makefile-mode'.")

;; (defvar tplsub-java--hdr-1 "/**
;;  * Handle events
;;  *
;;  * @param evt Event that triggered the action
;;  * @param arg Additional event information
;;  * @return true if the event is handled, false to pass it on
;;  **/")
;;
;;

(defun tplsub--java-fun ()
  "Reads data and insert a method definition."
  (let ((mname (read-string "Method name: " nil tplsub-hist-list))
        (mdesc (read-string "Description:" nil tplsub-hist-list))
        (nop (string-to-number
              (read-string "Number of parameters: " nil tplsub-hist-list "0")))
        (nox (string-to-number
              (read-string "Number of exceptions: " nil tplsub-hist-list "0")))
        (isvo (y-or-n-p "Void function? "))
        (rtype "")
        e
        p-list
        x-list)
    (mapc 'tplsub--tmpl-exec (list "/**" '/ "* " mdesc '/ "*" '/))
    (while (> nop 0)
      (decf nop)
      (let* ((pna (read-string "Parameter name: " nil tplsub-hist-list))
             (pty (read-string (concat "Type for `" pna "': ")
                               nil tplsub-hist-list))
             (pda (read-string (concat "Short desc. of `" pna "': ")
                               nil tplsub-hist-list)))
        (insert "* @param " pna " " pda)
        (tplsub-indent-new-line)
        (setq e (cons pty pna))
        (if p-list
            (setq p-list (append (list e) p-list))
          (setq p-list (list e)))))
    (while (> nox 0)
      (decf nox)
      (let* ((xna (read-string "Exception class: " nil tplsub-hist-list))
             (xuc (y-or-n-p (concat "`" xna "' is unchecked? ")))
             (xda (read-string (concat "Short desc. of `" xna "': ")
                               nil tplsub-hist-list)))
        (insert "* @throws " xna " " xda)
        (tplsub-indent-new-line)
        (unless xuc
          (setq x-list (if x-list (concat x-list ", " xna)
                         (concat "throws " xna))))))
    (unless isvo
      (setq rtype (concat (read-string "Return type: " nil tplsub-hist-list)
                          " "))
      (insert  "* @return " (read-string "Return value desc.:"
                                         nil tplsub-hist-list))
      (tplsub-indent-new-line))
    (mapc 'tplsub--tmpl-exec (list "*/" '/ '(= . "vis") (or rtype "")
                                   mname "("))
    (when p-list
      (setq p-list (nreverse p-list))
      (while p-list
        (insert (caar p-list) " " (cdar p-list))
        (setq p-list (cdr p-list))
        (if p-list (insert ", "))))
    (insert ")")
    (if x-list (insert " " x-list))
    (mapc 'tplsub--tmpl-exec (list " {" '/ '| '/ "} //" mname '*))))

(defvar tplsub-java-list
  '(
    ("HDR"    "/**" / " * " ("Description") / " *" /
              (for "?#no params" (" * @param x d..." /))
              " * @return ..." /
              " * @exception ..." / " **/" /)
    ("BODY"   " {" * / * | / "}" *)
    ("0"      "false")
    ("1"      "true")
    ("abs"    "abstract ")
    ("cls"    (tmpl "basic.java"))
    ("code"   "<code>" | "</code>")
    ("constr" "/**" / "* Constructor - ..." /
	      (for "?Antall parametere" (" * @param x d" /))
	      (for "?Antall exceptions" (" * @throws x d" /))
	      " */" /
	      ("Class") (= . "BODY") " // " ("Class") "()" /)
    ("cook"   "Cookie " ("Name") "= new Cookie(" | ",);" /
	      "resp.addCookie(" ("Name") ");")
    ("dtains" "DataInputStream " ("Varname" . "inS") "= null;" /
	      ("Varname") "= new DataInputStream(new BufferedInputStream("
	      | "));")
    ("dtaouts" "DataOutputStream " ("Varname" . "outS") "= null;" /
	       ("Varname") "= new DataOutputStream(new BufferedOutputStream("
	       | "));")
    ("destroy" "/**" / "* Runs on exit" / "*/" /
	      "public void destroy()" (= . "BODY") " // destroy()" /)
    ("eq"     "equals(" | ")")
    ("err"    "System.err.println(\"" | "\");")
    ("final"  "/**" / "* Class destructor" / "*/" / "protected void finalize() "
              "throws Throwable {" / | "super.finalize();" / "} // finalize()"
              *)
    ("for"    "for (int i=0; i<" | "; i++)" (= . "BODY") (help))
    ("forit"  "for (Iterator it=" | "; it.hasNext();) {" / * "it.next();" / "}"
              * (help . "for"))
    ("fun"    (tplsub--java-fun))
    ("getset" (q "Variable" nil t)
              (q "Type" nil t)
              (yn "declare variable"
                  (= . "var"))
              (yn "getter"
                  (/ "/**" / "* Get the value of " ("Variable") "." /
                   " * @return value of " ("Variable") "." /
                   " * @see #set" (q "Variable" nil nil nil capitalize) /
                   "**/" /
                   "public " ("Type") " " (yn "Boolean" ("is") ("get"))
                   (q "Variable" nil nil nil capitalize) "() {" / "return "
                   ("Variable") ";" / "} // " (yn "Boolean" ("is") ("get"))
                   (q "Variable" nil nil nil capitalize) "()" /))
              (yn "setter"
                  (/ "/**" / "* Set the value of " ("Variable") "." /
                   " * @param newVal New value of " ("Variable") /
                   " * @see #" (yn "Boolean" ("is") ("get"))
                   (q "Variable" nil nil nil capitalize) /
                   "**/" /
                   "public void set" (q "Variable" nil nil nil capitalize)
                   "(final"
                   ("Type") " newVal) {" / "this." ("Variable") "= newVal;" /
                   "} //set" (q "Variable" nil nil nil capitalize) "()" * /)))
    ("hash"   "/**" / "* Returns a hash code for this instance."
              / "*" / "* @return An <code>int</code> containing the hash code" /
              "*/" / "public int hashCode() {" / "int res=0," / "c;" /
              "// for each field..." / "// bool: c= (f? 0:1);" /
              "// byte/char/short/int: c= (int)f;" /
              "// long: c=(int)(f^(f>>>32));" /
              "// float: c= Float.floatToIntBits(f);" /
              "// double: c=(int)(Float.floatToIntBits(f)^(Float."
              "floatToIntBits(f)>>>32));" / "// objekter: c= f.hashCode();" /
              |
              (for "?antall felter" ("c= x;" / "res= 37*res + c;" /))
              "return res;" / "} // hashCode()" *)
    ("if"     "if (" | ")" (= . "BODY") / "else" (= . "BODY"))
    ("imp"    "import java." | ";")
    ("interf" (tmpl "basic.java"))
    ("inst"   "instanceof ")
    ("jsp"    "<% " | " %>")
    ("jspc"   "<%-- " | " --%>")
    ("jspd"   "<%! " | " %>")
    ("jspdir" "<%@ " | " %>")
    ("jspe"   "<%=" | "%>")
    ("jspf"   "<jsp:forward page=\"" | "\"/>")
    ("jspg"   "<jsp:getProperty name=\"" ("Property") "\" property=\"" | "\"/>")
    ("jspi"   "<jsp:include page=\"" | "\"/>")
    ("jsppa"  "<jsp:param name=\"" | "\" value=\"\"/>")
    ("jsppl"  "<jsp:plugin>" | / "</jsp:plugin>")
    ("jsps"   "<jsp:setProperty name=\"" ("Property") "\" property=\"" |
              "\" value=\"\"/>")
    ("jspu"   "<jsp:useBean>" | / "</jsp:useBean>")
    ("main"    "/**" / "* Main program (app runnable)!."
              / "*" / "* @return Exit status" /
              "*/" / "public void main(String args[]) {" / | /
              "} // main()" *)
    ("n"      "null")
    ("nobj"   ("Object") " " | "= new " ("Object") "();")
    ("print"  "System.out.println(\"" | "\");")
    ("proc"   (= . "HDR") "void " ("Name") "() {" / | / "} // " ("Name") "()" /)
    ("priv"   "private " (help . "modifiers"))
    ("prot"   "protected " (help . "modifiers"))
    ("pub"    "public " (help . "modifiers"))
    ("ret"    "return " | ";" /)
    ("rline"  "readLine)")
    ("run"    "/**" / "* Called to run the applet" / "*/" /
	      "public void run() {" / "while (true) {" / | "repaint();" /
	      "try { Thread.sleep(1000); }" /
	      "catch (InterruptedException e)  {}" / "}" / "} //run()" *)
    ("see"    "{@see #" | "}")
    ("sw"     "switch (" | ") {" / "case " * | ":" / * "break;" / *
              "default:" / "}" *)
    ("synch"  "synchronized " (help . "modifiers"))
    ("t"      "this.")
    ("tos"    "/**" / "* Returns a string representation of this instance."
              / "*" / "* @return This as <code>String</code>" /
              "*/" / "public String toString() {" / | / "} // toString()" *)
    ("try"    "try {" / | / "}" /
              (for "?antall catch" ("catch (" | "Exception e) {" / * | "}" /))
              (yn "finally" ("finally {" / * | / "}" *)))
    ("var"    "/**" / "* " ("Description") / "*/" /
              (= . "vis") " " ("Type") " " ("Variable") ";" /)

    ("vis"    (opt "Visibility" (("i" . "private") ("o" . "protected")
                                 ("u" . "public")  ("x" . ""))) " ")
    ("vol"    "volatile " (help . "modifiers"))
    ("wh"     "while (" | ") {" / * | / "}" *)
    )
  "Templates for `java-mode'.")

(defvar tplsub-java-help-list
  '(
    ("exceptions" "{Interrupted,MalformedURL,IO,Socket}Exception")
    ("for"    "for (init \; increment\; condition)")
    ("modifiers" "<vis> static abstract synchronized volatile final native"
                 "<vis> is one of public, protected or private")
    ("url"    "URL(\"{http,gtp,gopher,file}\",host:String[,port:int],path:String),"
              "URL(base:String,rel:String), URL(adr:String)")
    )
  "Help for `java-mode'.")

(defvar tplsub-ksh-list
  '(
    ("ARGERR" "exit 101")
    ("ERROR"  "Error")
    ("HELPME" "(-h gir hjelp)")
    ("case"   "case " ("Expr") " in" /
     "(" | ")   " | ";; /
     "(" | ")   " | ";; /
     "esac" *)
    ("for"     "for f in " | "; do" / /
               "done")
    ("fun"    "function " ("Function name") " { # " |
              "\n   # \n} # " ("Function name") "\n")
    ("if"     "if [[ " | " ]]; then" / | /
              "fi" *)
    ("int"    "typeset -i " | "=0")
    ("noarg"  "if [[ $# -eq 0 ]]; then" /
              (= . "ERROR")
                 " \"mangler " | "kommandolinjeargumenter "
                 (= . "HELPME")
                 "!\"" /
              (= . "ARGERR") /
              "fi" *)
    ("opt"    "while getopts " ("Optstring" .":h") " opt; do" /
              "case \"$opt\" in" /
              "(h) Help;;" /
              "(" | ") ...;;" /
              "(:)  "
                    (= . "ERROR")
                    " \"mangler argument til -$OPTARG  "
                    (= . "HELPME")
                    "\"" /
              (= . "ARGERR") ";;" /
              "(\?) "
                    (= . "ERROR")
                    " \"ukjent opsjon -- -$OPTARG  "
                    (= . "HELPME")
                    "\"" /
              (= . "ARGERR") ";;" /
              "esac" * /
              "done" /
              "shift $OPTIND-1")
    ("pr"     "print \"" | "\"")
    ("prn"    "print -n \"" | "\"")
    ("rd"     "read " ("variabel") "?\"" ("prompt") ": \"")
    ("rdfl"   (= . "rd") / (= . "zrd"))
    ("ro"     "typeset -r ")
    ("sep"    "# ----------------------------------------------------------"
              "------------")
    ("wh"     "while [[ " | " ]]; do" / /
              "done")
    ("zrd"    "[[ -z \"$" ("variabel") "\" ]] && " ("variabel") "=\""
              ("defaultverdi") "\"" /)
    )
  "Templates for `ksh-mode'.")

(defvar tplsub-wml-tmpl-list
  '(
    ("a"    "<a href=\"" ("URL") "\" title=\"" ("title") "\">"  | "</a>")
    ("anch"  "<anchor title=\"" ("title") "\""
             (yn "Accesskey" (" accesskey=\"" ("Access key") "\""))
             ">\n" * + | "\n" * - "</anchor>")
    ("card"  "<card id=\"" ("ID") "\" title=\"" ("Title") "\""
             (yn "Newcontext" (" newcontext=\"true\""))
             (yn "Unordered"  (" ordered=\"false\""))
             (yn "On back"    (" oneventbackward=\"" ("URLback") "\""))
             (yn "On forward" (" oneventforward=\"" ("URLforw") "\""))
             ">" | / "</card>")
    ("do"    "<do type=\""
             (help . "doopt")
             (opt "Accept" (("a" . "accept") ("p" . "prev") ("h" . "help")
                            ("r" . "reset") ("o" . "options") ("d" . "delete")
                            ("u" . "unknown")))
             "\" label=\"" ("Label") "\"" (yn "Name" (" name=\"" ("Name") "\""))
             (yn "Optional" ("optional=\"true\""))
             ">\n" * + | "\n" * - "</do>" (help))
    ("fset"  "<fieldset title=\"" ("Title") "\">" | / "</fieldset>")
    ("go"    "<go href=\"" ("URL") "\"" (yn "Post" (" method=\"post\""))
             ">\n" * + | "\n" * - "</go>"
             (help))
    ("img"   "<img src=\""  ("URL") "\""
             (yn "localsrc" (" localsrc=\"" ("localsrc") "\""))
             (yn "figtxt" (" figtext=\"yes\""))
             " alt=\"" ("ALT") "\""
             (yn "Alignment" (" align=\"" (opt "Alignment" (("t" . "top")
                                              ("m" . "middle"))) "\""))
             "/>"
             (help))
    ("input" ("Title") ": " "<input name=\"" ("Name") "\" title=\"" ("Title")
             "\"" (yn "Value" (" value=\"" ("Value") "\""))
             (yn "Empty OK" (" emptyok=\"true\""))
             (yn "Password" (" type=\"password\""))
             (yn "Format" ((help . "informat") " format=\"" ("Format")
                           "\""))
             (yn "Max length" (" maxlength=\"" ("Max length") "\""))
             (yn "Size" (" size=\"" ("Size") "\""))
             "/><br/>" (help))
    ("noop"  "<noop/>")
    ("onev"  "<onevent type=\"on"
             (help . "onevev")
             (opt "Event" (("f" . "eventforward") ("b" . "eventbackward")
                           ("t" . "timer") ("p" . "pick")))
             "\">\n" * + | "\n" * - "</onevent>" (help))
    ("opt"   "<option title=\"" ("Title") "\" value=\"" ("Value") "\""
             (yn "Onpick" (" onpick=\"" ("URL") "\"")) "/>"
             (help))
    ("optg"  "<optgroup title=\"" ("Title") "\">" (help))
    ("p"     "<p" (yn "nowrap" (" mode=\"nowrap\""))
             (yn "Alignment"
                 (" align=\"" (opt "Alignment" (("L" . "left") ("R" . "right")
                                                ("C" . "center")))
                  "\"")) ">" | "</p>")
    ("postf"  "<postfield name=\"" ("name") "\" value=\"" ("value") "\"/>")
    ("prev"  "<prev>" (= . "svar") "</prev>")
    ("refr"  "<refresh>" (= . "svar") "</refresh>")
    ("sel"   "<select name=\"" ("Name") "\" title=\"" ("Title") "\""
             (yn "Index variable" (" iname=\"" ("Index-var") "\""))
             (yn "Init. value" (" ivalue=\"" (" Default index") "\""))
             (yn "Multiple select" (" multiple=\"true\""))
             (yn "Default value" (" value=\"" ("Default value") "\""))
             ">" / + (= . "opt") | / - "</select>" (help))
    ("svar"  "<setvar name=\"" ("name") "\" value=\"" ("value") "\"/>")
    ("table" "<table columns=\"" ("Columns") "\""
             (yn "Title" (" title=\"" ("Title") "\""))
             (yn "Alignment" (" align=\""
                              (opt "Alignment" (("L") ("C") ("R"))) "\""))
             ">\n" * + "<tr>\n" * + "<td>" | "</td>\n" * - "</tr>" / -
             "</table>")
    ("tmpl"  "<template"
             (yn "Event backward" (" oneventbackward=\"" ("URL back") "\""))
             (yn "Event forward" (" oneventforward=\"" ("URL forw.") "\""))
             (help))
    ("timer" "<timer value=\"" ("value") "\""
             (yn "Name" (" name=\"" ("Name") "\"")) "/>" (help))
    )
  "Templates for `wml-mode'.")

(defvar tplsub-wml-help-list
  '(
    ("do"     "Elements: go/prev/noop/refresh")
    ("doopt"  "a=accept p=prev h=help r=reset o=options d=delete u=unknown")
    ("go"     "Fields: postfield and setvar")
    ("img"    "Additional attributes: height, width, hspace,vspace")
    ("informat" "Formats: AaNXxMm \\c *<f> <n><f>")
    ("input"  "Other attributes: tabindex")
    ("onev"   "Elements: go/prev/noop/refresh")
    ("onevev" "f=eventforward b=eventbackward t=timer p=pick")
    ("opt"    "Elements: text, <onevent>")
    ("optg"   "Elements: optgroup, option")
    ("sel"    "Other attributes: tabindex\nElements: option, optgroup")
    ("timer"  "Times are in 1/10th second")
    ("tmpl"   "Elements: do, onevent")
  )
  "Help for `wml-mode'.")

(defvar tplsub-Xml-tmpl-list
  '(
    ("ALIGN" (yn "Alignment"
                 (" align=\"" (opt "Alignment" (("L" . "left") ("R" . "right")
                                                ("C" . "center"))))))
    ("addl"  "<dt>" ("Description") / "<dd><a href=\"" ("URL") "\">" /
             "\t" ("URL") "</a>" /)
    ("ah"    "<a href=\"" ("URL") "\""
             (yn "target" (" target=\"_"
                           (opt "Target" (("B" . "blank") ("S" . "self")
                                          ("P" . "parent") ("T" . "top")))
                           "\""))
             ">" | "</a>")
    ("an"    "<a name=\"" ("Id") "\">" | "</a>")
    ("att"   "<!ATTLIST " ("Element") "\n          " | "\n          >")
    ("art"   "<article" (yn "ID" (" id='" ("Id") "'")) ">" / +
             "<header>" / | / "</header>" / |
             "<footer>" / | / "</footer>" / "</article>" *)
    ("audio" "<audio" (yn "Controls" " controls")
             (yn "Single src" ("src='" ("src") "'")) ">" / +
             (for "?antall source" ("<source src='" | "' />"))
             | / - "</audio>")
    ("b"     "<b>" | "</b>")
    ("bmk"   "<br/><a href=\"" | "\">" | "</a>")
    ("br"    "<br" (yn "Clear" (" clear=\"" (opt "Side" (("L" . "left")
                                                         ("R" . "right")))
                                "\"")) " />")
    ("canvas" "<canvas id='" ("Id") "' width='" | "' heigth='" | "'>"
              / + | "<!-- fallback -->" / "</canvas>" *)
    ("cdata" "<![CDATA[" | "]]>")
    ("c"   "<code>" | "</code>")
    ("css" "<link href=\"" | "\" rel=\""
           (yn "Alternate" ("alternate ")) "stylesheet\" "
           "type=\"text/css\""
           (yn "Media" (" media=\""
                        (opt "Type" (("A" . "aural") ("S" . "screen")
                                     ("P" . "print")))
                        "\""))
           (yn "Title" ("cssTitle"))
           " />")
    ("doct"  o "<!DOCTYPE " ("Doctype")
             (yn "System"
                 (" SYSTEM \"" ("File") "\"")) | ">")
    ("dt"    "<dt>" | "</dt>" / "<dd>" (= . "n") | / - "</dd>")
    ("dtd"   "<!-- Document type: " ("Element") /
             (1) "Created:       " (= . "fdate") /
             (1) "By:            " (= . "author") / /
             "     This DTD..." / "-->" / /
             (= . "elem"))
    ("dl"    o "<dl>" / + (for "?antall termer" ((= . "dt") / )) / "</dl>" *)
    ("elem"  "<!ELEMENT " ("Element") " (" | ") >"
             (yn "Attlist" ("\n" (= . "att") "\n")))
    ("em"    "<em>" | "</em>")
    ("ent"   "<!ENTITY " (yn "Intern" ("% ")) ("Entity") " \"" | "\" >")
    ("font"  "<font" (yn "Size" (" size=\"" ("Size spec") "\""))
                     (yn "Name" (" name=\"" ("Font name") "\""))
             ">" | "</font>")
    ("hd"    "<head>" (yn "Title" ((= . "tit"))) | "</head>")
    ("hr"    "<hr" (yn "Size" (" size=\"" ("Size (1-9)") "\""))
                   (yn "Noshade" (" noshade")) (= . "ALIGN"))
    ("html"  o "<html>" / + (= . "hd") / "<body>" / + | "<h1>" ("Title text")
             "</h1>" / - "</body>" / - "</html>")
    ("i"     "<i>" | "</i>")
    ("img"   "<img src=\"" | "\"" / "alt=\"" ("alt") "\""
             (yn "Figtext" (/ " figtext=\"yes\"/>") (" />")))
    ("imp"   "#IMPLIED")
    ("inp"   "<input" (yn "Size" (" size=\"" ("Field size") "\""))
                      (yn "Maxlength" (" maxlength=\"" ("Max chars") "\""))
                      (yn "Name" (" name=\"" ("Field name") "\""))
                      (yn "Value" (" value=\"" ("Field value") "\""))
                      (yn "Checked" (" checked"))
             " type=\"" (opt "Type" (("X" . "text") ("P" . "password")
                                     ("C" . "checkbox") ("A" . "radio")
                                     ("B" . "button") ("S" . "submit")
                                     ("R" . "reset")))
             " />")
    ("jsp"   "<%" / + | / - "%>")
    ("jspc"   "<%-- " | " --%>")
    ("jspd"   "<%! " | " %>")
    ("jspdir" "<%@ " | " %>")
    ("jspe"   "<%=" | "%>")
    ("jspf"   "<jsp:forward page=\"" | "\"/>")
    ("jspg"   "<jsp:getProperty name=\"" ("Property") "\" property=\"" | "\"/>")
    ("jspi"   "<jsp:include page=\"" | "\"/>")
    ("jsppa"  "<jsp:param name=\"" | "\" value=\"\"/>")
    ("jsppl"  "<jsp:plugin>" | / "</jsp:plugin>")
    ("jsps"   "<jsp:setProperty name=\"" ("Property") "\" property=\"" |
              "\" value=\"\"/>")
    ("jspu"   "<jsp:useBean>" | / "</jsp:useBean>")
    ("li"    "<li>" | "</li>")
    ("link"  "<a href=\"" ("URL") "\">" ("URL") "</a>")
    ("lotab" o "<table width=\"100%\" border=\"0\" "
               "cellspacing=\"0\" summary=\"layout\">" / +
             "<tr>" / +
             "<td>" | "</td>" /
             "<td>" | "</td>" / -
             "</tr>" / -
             "</table>")
    ("n"     [return tab home left return tab])
    ("ol"    o "<ol>" / + (for "?antall items" ("<li>" | "</li>" /)) - "</ol>")
    ("olt"   o "<ol type=\""
             (opt "List type"
                  (("A") ("a") ("I") ("i") ("1")))
             "\">" / +
             (for "?antall items" ("<li>" | "</li>" /)) - "</ol>")
    ("p1"    "<p>" | "</p>")
    ("p"     "<p>" / + $ | / - "</p>")
    ("pcd"   "#PCDATA")
    ("pgdir" "<%@ page " | ">")
    ("prod"  "<product>" | "</product>")
    ("prop"  "${" | "}")
    ("pp"    "<p>" / + | / - "</p>")
    ("q"     "<q>" | "</q>")
    ("qs"     "<q single=\"yes\">" | "</q>")
    ("req"   "#REQUIRED")
    ("script" "<script type='text/javascript'>" / + | / "</script>")
    ("sect"  "<sect>" / + "<title>" | "</title>" /
             "<description>" / + | / - "</description>" / - "</sect>")
    ("see"   "<see refto=\"" ("Id") "\"/>")
    ("t"     o "<" ("Tag") ">" / + | / - "</" ("Tag") ">")
    ("td"    "<td>" | "</td>")
    ("tit"   "<title>" ("Title text") "</title>")
    ("tr"    o "<tr>" / + (for "?antall kolonner" ("<td>" | "</td>" /))
             "</tr>" *)
    ("tab"   "<table summary=\"" ("Summary") "\""
             (yn "Width" (" width=\"" ("Width" . "100%") "\""))
             (yn "Border" (" border=\"1\""))
             ">" / +
             (yn "Caption" ((yn "copySummary"
                                ("<caption copySummary=\"yes\"/>" /)
                                ("<caption>" ("Summary") "</caption>" /))))
             "<thead>" / +
             "<tr>" / +
             (for "?antall kolonner" ("<th>" | "</th>" /)) -
             "</tr>" / -
             "</thead>" /
             "<tbody>" / +
             (for "?antall rader"
                  ("<tr>" / +
                   (for "?antall kolonner" ("<td>" | "</td>" /))
                   - "</tr>" /))
             - "</tbody>" / -
             "</table>")
    ("ul"    o "<ul>" / +
             (for "?antall items" ("<li>" | "</li>" /)) -
             "</ul>")
    ("ult"   o "<ul type=\"" (opt "List type"
                                  (("D" . "disc") ("c" . "circle")
                                   ("S" . "square")))
             "\">" /
             + (for "?antall items" ("<li>" | "</li>" /)) - "</ul>")
    ("video" "<video" (yn "Controls" " controls")
             (yn "Single src" ("src='" ("src") "'")) ">" / +
             (for "?antall source" ("<source src='" | "' />"))
             | / - "</video>")
    ("xml"   o "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" /)
    ("xsap"  "<xsl:apply-templates"
             (yn "Select" (" select=\"" ("Select") "\"")) "/>")
    ("xsat"  "<xsl:attribute name=\"" ("Attr.name") "\">" | "</xsl:attribute>")
    ("xsc"   "<xsl:comment>" | "</xsl:comment>")
    ("xsca"  "<xsl:call-template name=\"" ("Template name") "\""
             (yn "Params" (">" / (for "?antall param" ((= . "xswp") /)) /
                           "</xsl:call-template>") ("/>")))
    ("xsch"  o "<xsl:choose>" /
             (for "?antall when" ((= . "xswh") /))
             (yn "otherwise" ("<xsl:otherwise>" | "</xsl:otherwise>" /))
             "</xsl:choose>" *)
    ("xsco"  "<xsl:copy>" | "</xsl:copy>")
    ("xsfor" "<xsl:for-each select=\"" | "\">" | "</xsl:for-each>")
    ("xsif"  "<xsl:if test=\"" | "\">" | "</xsl:if>")
    ("xsifa" "<xsl:if test=\"@" ("Attribute") "\"><xsl:value-of select=\"@"
             ("Attribute") "\"/></xsl:if>")
    ("xsimp" "<xsl:import href=\"" | "\"/>")
    ("xsinc" "<xsl:include href=\"" | "\"/>")
    ("xsnum" "<xsl:number"
             (yn "Value" (" value=\"" ("Value") "\"")
                         (" level=\""
                          (opt "Level" (("a" . "any") ("s" . "single")
                                        ("m" . "multiple")))
                          "\""
                          (yn "Count" (" count=\"" | "\""))
                          (yn "From" (" from=\"" | "\""))))
             (yn "Format" (" format=\"" ("format string") "\""))
             "/>")
    ("xso"   o "<xsl:output method=\""
               (opt "Type" (("h" . "html") ("x" . "xml") ("t" . "text"))) "\"\n"
               (2) "version=\"" (opt "Type" (("h" . "4.0") ("x" . "1.0")
                                             ("t" . ""))) "\""
               (yn "Media type" ("\n" (2) "media-type=\""
                                 ("Type" . "text/html; ISO-8859-1") "\""))
               (yn "Encoding" ("\n" (2)  "encoding=\"" | "ISO-8859-1\""))
               (yn "Doctype" ("\n" (2)  "doctype-public=\"" |
                              "-//W3C//DTD HTML 4.0//EN\""))
               "/>")
    ("xsp"   "<xsl:param name=\"" ("Parameter name") "\""
             (yn "Select" (" select=\"" | "\"")) "/>")
    ("xspar"  (= . "xsp"))
    ("xss"   o "<xsl:stylesheet version=\"1.0\"" / (1)
             " xmlns:xsl=\"http://www.w3.org/1999/XSL/Transform\">"<
             "\n\n</xsl:stylesheet>")
    ("xst"   "<xsl:template"
             (yn "Name" (" name=\"" ("Name") "\""))
             (yn "Match" (" match=\"" ("Match") "\""))
             (yn "Mode" (" mode=\"" ("Mode") "\""))
             ">"
             (yn "Name" ((for "?antall param" ((= . "xsp") /))))
             / | /
             "</xsl:template>" *)
    ("xstx"  "<xsl:text" (yn "Disable escape"
                             (" disable-output-escaping=\"yes\"")) ">" |
             "</xsl:text>")
    ("xsty"   o "<?xml-stylesheet type=\"text/xsl\"" (: . 1)
               "href=\"" ("File") "\"?>")
    ("xsv"   "<xsl:value-of"
             (yn "Select" (" select=\"" | "\"/>")
                          (">" | "</xsl:value-of>")))
    ("xsvar" "<xsl:variable name=\"" ("Variable name") "\""
             (yn "Variable select" (" select=\"" | "\"")) "/>")
    ("xswh"  "<xsl:when test=\"" | "\">" | "</xsl:when>")
    ("xswp"  "<xsl:with-param name=\"" ("Param name") "\""
             (yn "Param select" (" select=\"" | "\"/>") (">" |
                                                         "</xsl:with-param>")))
    ;; JavaPres
    ("page"  o "<page id=\"" | "\">" / + "<ptitle>" | "</ptitle>" /
             "<ptext>" / + | "<ul>" / + "<li>" | "</li>" / - "</ul>" / -
             "</ptext>" / - "</page>\n")
    ;; Tips & quotes
    ("ttip"   o "<tip id=\"" ("ID")  "\" class=\""
             (opt "Class" (("s" . "serious") ("e" . "error") ("m" . "minor")
                           ("n" . "need") ("w" . "want") ("t" . "tip")))
             "\""
             (yn "Prod.ver." (" pversion=\"" ("Version") "\""))
             ">" / + "<ttitle>" ("Title") "</ttitle>" /
             "<tcats>" /
             (for "?antall kategorier" ("<tcat>" ("Kategori") "</tcat>" /))
             "</tcats>" /
             (yn "Source" ("<tsource>" ("Source") "</tsource>" /))
             "<ttext>"  / + | / -
             "</ttext>" / -
             "</tip>")
    ;; her kommer utility.dtd & rulez.dtd
    ("UTILTYPE" (yn "Type"
                    (" type=\""
                     (opt "vartype" (("S" . "STD")  ("O" . "OTHER")
                                     ("V" . "VOID") ("N" . "NONE")
                                     ("i" . "i")         ("u" . "u")
                                     ("r" . "r")    ("l" . "l")
                                     ("x" . "ir")))
                     "\""))
                 (yn "Default"
                     (" defval=\"" ("defval") "\""))
                 (yn "Docdefval" (" docdefval=\"yes\"")))
    ("alias" "<alias name=\"" ("Name") "\" class=\""
             (opt "Class" (("r" . "ref") ("s" . "std") ("d" . "def")))
             "\""
             (yn "Definition" (" command=\"" | "\""))
             (yn "Doc" (">" | "</alias>")
                 ((yn "Desc" (" desc=\"" | "\""))
                  "/>")))
    ("aref"   "<argref arg=\"" ("Arg") "\"/>")
    ("che"    "<check cat=\"" ("Cat" . "Kodelesing") "\""
              (yn "Sub1" (" subcat1=\"" ("Subcat1") "\""
                          (yn "Sub2" (" subcat2=\"" ("Subcat2") "\""))))
              ">" | "</check>")
    ("cw"     "<code><keyw case=\"keep\">" | "</keyw></code>")
    ("eref"   "<see refto=\"" ("id") "\"/>")
    ("exref"  "<extref id=\"" ("id") "\""
              (yn "URL" (" url=\"" ("url") "\""))
              | "/>")
    ("evar"   "<envvar var=\"" ("Var") "\""
               (yn "Dollar" () (" dollar=\"no\""))
               (yn "Braces" (" brace=\"yes\""))
               (yn "Noref" (" noref=\"yes\""))
               (yn "Doc" (">" | "</envvar>") ("/>")))
    ("fparam"  "<fparam name=\"" ("Name") "\""
               (yn "Mult" (" mult=\"" ("mult=") "\""))
               (= . "UTILTYPE")
               (yn "Autodecl" (" autoDecl=\"yes\""))
               ">" | "</fparam>")
    ("fref" "<fileref" (yn "noref" (" noref='yes'")) ">" |
            "</fileref>")
    ("fun"     "<function name=\"" ("FuncName") "\""
               (= . "UTILTYPE")
               ">"
               (yn "Short desc" (/ "<shortdesc>" | "</shortdesc>"))
               (yn "Long desc" (/ "<description>" / | / "</description>" *))
               (yn "Returns" (/ "<returns>" / | / "</returns>" *))
               (for "?# param" (/ (= . "fparam")))
               / "<fbody"
               (yn "Shift out param" (" shift=\"yes\""))
               ">" / | / "</fbody>" / "</function>")
    ("gref" "<glossref refto=\"" ("RefID") "\"/>")
    ("gwref" "<glossref weakref=\"" ("RefID") "\"/>")
    ("gui"  "<guielement>" | "</guielement>")
    ("gvar" "<globvar varname=\"" ("Var") "\""
            (= . "UTILTYPE")
            (yn "Doc" (">" | "</globvar>") ("/>")))
    ("kp"   "<keypress" (yn "Mod" (" mod=\"" | "\"")) ">" | "</keypress>")
    ("kw"   "<keyw" (yn "Alt" (" alt=\"" | "\""))
            (yn "keep case" (" case=\"keep\""))
            ">" | "</keyw>")
    ("kwnn"   "<keyw>" | "</keyw>")
    ("kwny"   "<keyw case=\"keep\">" | "</keyw>")
    ("mref" "<manref"
              (yn "Local"
                  (" local=\"yes\"")
                  ((yn "Section" (" section=\"" ("section") "\""))))
              (yn "Noref" (" noref=\"yes\""))
              ">" | "</manref>" |)
    ("mpath" "<menupath>" / +
             (for "?no. of steps" ("<menusel>" | "</menusel>" /))
             - "</menupath>")
    ("msel" "<menusel>" | "</menusel>")
    ("oref" "<optref opt=\"" ("Opt") "\"/>")
    ("ppp"   "pp:prog=\"" ("Prog.lang") "\"")
    ("ppv"   "pp:var=\"" ("Variant" . "TAD.") "\"")
    ("prod"  "<product>" | "</product>")
    ("qw"    "<keyw q=\"yes\">" | "</keyw>")
    ("rref"  "<ruleref refto=\"" ("Refto") "\""
             (yn "Fallback" ("><fallback>" | "</fallback></ruleref>")
                            ("/>")))
    ("rstat" "<retstat value=\"" ("Value") "\""
             (yn "Desc" (" desc=\"" | "\""))
             (yn "Doc" (">" | "</retstat>")
                       ("/>")))
    ("RULEMODEL" + "<title>" | "</title>" /
             (yn "Shortdesc"
                 ("<shortdesc>" | "</shortdesc>" /
                  "<longdesc inclShort=\"no\">" / + | / - "</longdesc>" /)
                 ("<description>" / | / - "</description>" /))
             "<adapt pp:var=\"" | "\">" / + | / - "</adapt>" /
             )
    ("rule"  o "<" (yn "Subrule" ("sub")) "rule id=\"" ("id") "\">" /
             (= . "RULEMODEL")
             "<because>" / + | / - "</because>" /
             "<listing>" / +
             "<p>" / + | / - "</p>" /
             "<pre ugly=\"no\">" / + | / - "</pre>" /
             - "</listing>" /
             "<checks>" / + | / - "</checks>" /
             "<seealso>" / + | / - "</seealso>" /
             - "</" (yn "Subrule" ("sub")) "rule>")
    ("scref" "<sectref refto=\"" ("Refto") "\""
             (yn "Fallback" ("><fallback>" | "</fallback></sectref>")
                            ("/>")))
    ("spp"   "<span pp:prog=\"" ("Prog.lang") "\">" | "</span>")
    ("spv"   "<span pp:var=\"" ("Variant" . "TAD.") "\">" | "</span>")
    ("sref" "<stringref " (yn "Local" ("s") ("std")) "=\"" ("StringID") "\"/>")
    ("step" "<step>" | "</step>")
    ("steps" "<steps>" / +
             (for "?no. of steps" ("<step>" | "</step>" /))
             - "</steps>")
    ("tip"   o "<" (yn "Subtip" ("sub")) "tip id=\"" ("id") "\">" /
             (= . "RULEMODEL")
             "<seealso>" / + | / - "</seealso>" /
             - "</" (yn "Subtip" ("sub")) "tip>")
    ("hlp"   "<a href=\"javascript:displayHelp('" | "')\">" | "</a>")
    )
  "*ML templates.")

(defvar tplsub-Xml-help-list
  '(
    ("?"    "?")
    )
  "*ML syntax help.")

(defvar tplsub-vbscript-tmpl-list
  '(
    ("red"    "ReDim " (yn "Preserve" ("Preserve ")) | "()")
    )
  "VBScript templates.")

(defvar tplsub-vbscript-help-list
  '(
    ("?"    "?")
    )
  "VBscript syntax help.")


(defvar tplsub-sql-tmpl-list
  '(
    ("++"    (tplsub--inc "+"))
    ("--"    (tplsub--inc "-"))
    ("0"    "isnull(" | ",)")
    ("btran" "begin transaction " ("Transname"))
    ("c"    "@c_")
    ("chr"  "char(" | ")")
    ("cnt"  "count(" | "*)")
    ("ctran" "commit transaction " ("Transname"))
    ("curr" "where current of cur")
    ("dcl"  "declare ")
    ("dcur" o "declare cur" | " cursor for\n" (2) (= . "sel") / -1
            "for " (yn "Read only" ("read only") ("update of ")))
    ("del"  o "delete from " ("Table") / 1 "where ")
    ("drp"  o "drop proc " ("Procname") "\ngo\n")
    ("err"  "@@error")
    ("ex"   "exec " ("Procname") " ")
    ("g"    "go\n")
    ("if"   o "if " | / "begin" / | "end -- if")
    ("ins"  o "insert into " ("Table") "(" | ")" / 7 "values (" | ")")
    ("ocur" o "open cur" | / "fetch cur" | " into " | /
            "while @@sqlstatus=0" / "begin" / "fetch cur" | " into " | /
            "end -- sqlstatus=0" / "close cur" | /
            "deallocate cursor cur")
    ("p"    "@p_")
    ("proc" o "create proc " ("Procname") | "\nas\nbegin\n   declare " | "...\n"
            | "\nend -- " ("Procname") "\ngo")
    ("rc"   "@@rowcount")
    ("rtran" "rollback transaction " ("Transname"))
    ("sel"  o "select " | / "  from " ("Table") | / -1 "where " | /
            "order by ")
    ("un"   "union\n")
    ("upd"  o "update " ("Table") / 3 "set " | "=" / -1 "from " | / -1 "where ")
    ("v"    "@v_")
    ("vc"   "varchar(" | ")")
    ("wh"   "where ")
    ("whi"  o "while " | / "begin" / | "end -- while")
    )
  "SQL templates.")


(defun tplsub--inc (op)
  "Set preceding_var = preceding_var OP prefix."
  (let (var-name
        cpos)
    (save-excursion
      (skip-chars-backward "- \t,:\n._")
      (skip-chars-backward "-a-zA-Z0-9_@./")
      (setq cpos (point))
      (skip-chars-forward "-a-zA-Z0-9_@./")
      (setq var-name (buffer-substring cpos (point))))
    (insert "= " var-name " " op " "
            (number-to-string (prefix-numeric-value
                               current-prefix-arg)))))

(provide 'tplsub-x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commentary:
;;
;;  History:
;;    $Log: tplsub-x.el,v $
;;    Revision 1.11  2007/06/19 20:06:55  larsr
;;    E22
;;
;;    Revision 1.10  2005/11/09 14:01:09  larsr
;;    Misc
;;
;;    Revision 1.9  2005/10/19 05:19:01  larsr
;;    n,gwref,qw
;;
;;    Revision 1.8  2005/07/05 07:13:40  larsr
;;    Misc
;;
;;    Revision 1.7  2004/12/11 19:46:07  larsr
;;    not prin1-to-string
;;
;;    Revision 1.6  2004/09/10 19:24:52  larsr
;;    keywords
;;
;;    Revision 1.5  2004/07/30 05:50:34  larsr
;;    minor
;;
;;    Revision 1.4  2004/05/26 20:32:05  larsr
;;    misc...
;;
;;    Revision 1.3  2004/05/24 21:23:59  larsr
;;    Minifix
;;
;;    Revision 1.2  2004/03/15 21:30:04  larsr
;;    LF
;;
;;    Revision 1.1.1.1  2004/03/15 20:50:35  larsr
;;    Initial RCS revision
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; tplsub-x.el ends here
