<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: messages.xsl,v 1.3 2000/08/15 20:39:46 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.
     
     ******************************************************************** -->

<xsl:variable name="user.message.doc"
  select="document(concat($user.message.lang,'.xml'))/localization" />

<xsl:template name="user.message">
  <xsl:param name="node" select="." />
  <xsl:param name="arg" />
  <xsl:param name="key" />

  <xsl:variable name="message" 
    select="$user.message.doc/message[@key=$key]" />
  <xsl:variable name="text"
    select="$message/* | $message/text()" />
  
  <xsl:message>
    <!-- Just being more helpful -->
    <xsl:value-of select="name($node/..)" />
    <xsl:text>/</xsl:text>
  
    <xsl:value-of select="name($node)" />
    <xsl:text>: </xsl:text>

    <xsl:if test="$arg != ''">
      <xsl:copy-of select="$arg" />
      <xsl:text>: </xsl:text>
    </xsl:if>
    
    <xsl:choose>
      <xsl:when test="$text">
        <xsl:copy-of select="$text" />
      </xsl:when>
      <xsl:otherwise>
        <!-- Allow fallback if text is not available -->
        <xsl:value-of select="$key" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:message>
</xsl:template>
   
</xsl:stylesheet>

