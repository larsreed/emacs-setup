§rem -*-wml-*-
§rem ; Hi-lock: (("^§[a-z]*" (0 (quote hi-yellow) t)))
<%
    Option Explicit
§if UseDB Bruk database

    Dim gO_ADODBconn ' As ADODB.Connection
    Set gO_ADODBconn = Server.CreateObject("ADODB.Connection")
    gO_ADODBconn.ConnectionString= "DRIVER=SQL Server;SERVER=ECSOFT_WAP;UID=sa;APP=Microsoft Development Environment;WSID=ECSOFT_WAP;DATABASE=wap_db_email;User Id=sa;PASSWORD=;"
    gO_ADODBconn.Open
§if select Opprette SELECT

    Dim lSqlQuery ' As String
    Dim lO_rs ' As RecordSet
§fi Select
§else
§defbool select nil
§fi
§if WAP

    Response.ContentType = "text/vnd.wap.wml"    'send the right MIME type
§fi

§if WAP
%><?xml version="1.0"?>
<!DOCTYPE wml PUBLIC "-//WAPFORUM//DTD WML 1.1//EN" "http://www.wapforum.org/DTD/wml_1.1.xml">
<wml>
	<template>
	...
	</template>

§ask ID0 ID kort 0
§ask title0 Tittel kort 0
   	<card id="§ID0§" title="§title0§">
	<p>
§else
%>
§fi

§if select
§ask fieldList Feltliste
§ask table Tabell
§ask where WHERE-klausul

<%
    lSqlQuery = "SELECT §fieldList§ FROM §table§ WHERE §where§"
    Set lO_rs = gO_ADODBconn.Execute(lSqlQuery)
    If lO_rs.EOF Then
        lO_rs.Close
        Set lO_rs = Nothing
        Response.Write("feilmelding")
§if WML
        Response.Write("</p></card></wml>")
§fi WML
        Response.End
    End If
%>


		<p>
			<select title="???" name="???">
<%
§ask IDfield ID i løkke
    Do While Not lO_rs.EOF
        response.write("<option value="""& lO_rs("§IDfield§")&""">" & lO_rs("felt1")&"&nbsp;"& rsTelefon("felt2")& "</option>"&vbLF)
        lO_rs.MoveNext
    Loop
%>
			</select>
		</p>
<%
    lO_rs.Close
    Set lO_rs = Nothing
%>
§fi Select
§if WML
	</card>
</wml>
§fi
