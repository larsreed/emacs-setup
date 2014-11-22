package xae;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.BufferedWriter;
import java.io.OutputStreamWriter;
import java.io.IOException;
import java.util.StringTokenizer;
import com.icl.saxon.StyleSheet;

public class XMLToolServer {


  public static void main (String[] args) {

    new XMLToolServer().run();
     
  } // end of main ()


  public void run() {

    sendMessage("XAE Services\n");

    String commandLine = null;

    sendMessage("> ", false);

    while (continueExecution) {

      commandLine = receiveMessage();

      StringTokenizer tokens = new StringTokenizer(commandLine);

      if (tokens != null && tokens.hasMoreElements()) {
	processCommand(tokens);
	sendMessage("> ", false);
      } // end of while ()
    } 
  }  

  void processCommand(StringTokenizer tokens) {
    // sendMessage("Command line: " + commandLine);
   
    String command = tokens.nextToken();
    if (command.equals("quit"))
      quit();
    else if (command.equals("apply_stylesheet")) 
      applyStylesheet(tokens);
    else if (command.equals("apply_assoc_stylesheet")) 
      applyAssociatedStylesheet(tokens);
    else sendMessage("Unknown command: " + command);
  }

  void quit() {
    sendMessage("Bye");
    continueExecution = false;
  }

  /**
   * Executes apply_stylesheet command.
   *
   * <p>Command syntax:</p>
   *
   * <code>apply_stylesheet input_url style_url output_path
   *
   * @param tokens a <code>StringTokenizer</code> value
   */
  void applyStylesheet(StringTokenizer tokens) {
    
    String inputDocURL = tokens.nextToken();
    String stylesheetURL = tokens.nextToken();
    String outputFilePath = tokens.nextToken();

    if (xslt == null)
      xslt = new XSLTransformer();

    xslt.applyStylesheet(inputDocURL, stylesheetURL, outputFilePath);

  }


  /**
   * Executes apply_assoc_stylesheet command.
   *
   * <p>Command syntax:</p>
   *
   * <code>apply_assoc_stylesheet input_url output_path
   *
   * @param tokens a <code>StringTokenizer</code> value
   */
  void applyAssociatedStylesheet(StringTokenizer tokens) {
    
    String inputDocURL = tokens.nextToken();
    String outputFilePath = tokens.nextToken();

    if (xslt == null)
      xslt = new XSLTransformer();

    xslt.applyAssociatedStylesheet(inputDocURL, outputFilePath);

  }

    

  public void sendMessage(String msg, boolean newLine) {
    try {
    writer.write(msg);
    if (newLine) writer.newLine();
    writer.flush();
    }
    catch (IOException ex) {
      System.err.println(ex);
    }
  }

  public void sendMessage(String msg) {
      sendMessage(msg, true);
  }


  public String receiveMessage() {
    String msg = null;
    try {
      msg = reader.readLine();
    }
    catch (IOException ex) {
      System.err.println(ex);
    }
    return msg;
  }
  
  boolean continueExecution = true;
  BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
  BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(System.out));
  XSLTransformer xslt;

}
