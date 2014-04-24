§rem -*-html-*-
§rem ; Hi-lock: (("^§[a-z]*" (0 (quote hi-yellow) t)))
§do noTagL No of taglibs
§ask uri§R§ URI no.§R§
§ask pfx§R§ Prefix no.§R§
<%@ taglib uri="§uri§R§§" prefix="§pfx§R§§" %>
§done
<%@ page session="true"
	 import="java.util.* java.net.*"
§ask errPage Errorpage
	 errorPage="§errPage§" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.0//EN">
<%! // Declarations
%>
<%  // Intro
   try {
      String param;
      param= request.getParameter("action");
      if (param==null) {
      }
      else {
      }
      xxx= config.getInitParameter("xxx");
      session.setAttribute("xxx",xxx);
   }
   catch (Exception e) {
      out.print("JSP:"+e);
   }
%>
<html>
<head>

§ask title Page title
   <title>§title§</title>

   <style>
§if useCss Use CSS
§ask ssheet Style sheet
      <%@ include file="§ssheet§" %>
   </style>
</head>

<body>
</body>

</html>
