<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version='1.0'>

<!-- ********************************************************************
     $Id: callout.xsl,v 1.4 1999/11/03 20:39:45 nwalsh Exp $
     ********************************************************************

     This file is part of the XSL DocBook Stylesheet distribution.
     See ../README or http://nwalsh.com/docbook/xsl/ for copyright
     and other information.

     ******************************************************************** -->

<xsl:template match="programlistingco|screenco">
  <fo:block>
    <xsl:apply-templates/>
  </fo:block>
</xsl:template>

<xsl:template match="areaspec|areaset|area">
</xsl:template>

<xsl:template match="co">
  <xsl:variable name="conum">
    <xsl:number count="co" format="1"/>
  </xsl:variable>
  <xsl:text>(</xsl:text>
  <xsl:value-of select="$conum"/>
  <xsl:text>)</xsl:text>
</xsl:template>

</xsl:stylesheet>
