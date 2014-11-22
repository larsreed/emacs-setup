<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: verbatim.xsl,v 1.2 2000/08/21 20:19:55 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<xsl:template match="literallayout[@class='monospaced']" priority="1">
  <example><xsl:apply-templates mode="synopsis.mode" /></example>
</xsl:template>

<xsl:template match="literallayout">
  <display><xsl:apply-templates /></display>
</xsl:template>
  
<xsl:template match="programlisting|screen">
  <example><xsl:apply-templates mode="synopsis.mode" /></example>
</xsl:template>

<xsl:template match="address">
  <format><xsl:apply-templates /></format>
</xsl:template>

</xsl:stylesheet>
