<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: texinode-base-id.xsl,v 1.3 2000/08/21 20:19:55 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->


<xsl:template name="texinfo.nodename">
  <xsl:param name="node" select="." />
  <xsl:param name="sugname">
    <xsl:choose>
      <xsl:when test="$texinfo.nodename.fallback.object-id">
        <xsl:call-template name="object.id">
          <xsl:with-param name="object" select="$node" />
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="generate-id($node)" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:param>
 
  <xsl:choose>
    <xsl:when test="$node/../book or generate-id($node)=generate-id(/*)">
      <xsl:value-of select="'Top'" />
    </xsl:when>
    <xsl:otherwise><xsl:value-of select="$sugname" /></xsl:otherwise>
  </xsl:choose>
</xsl:template>
  
</xsl:stylesheet>

