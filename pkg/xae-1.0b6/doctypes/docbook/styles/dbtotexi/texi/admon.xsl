<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: admon.xsl,v 1.5 2000/08/20 03:16:34 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<xsl:template match="note|important|warning|caution|tip">
  <quotation>
    <xsl:call-template name="texinfo.anchor" />
  
    <xsl:call-template name="caption">
      <xsl:with-param name="content">
        <xsl:choose>
          <xsl:when test="./title">
            <xsl:apply-templates select="./title" mode="admonition.title.mode" />
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="gentext.element"/>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:with-param>
    </xsl:call-template>
  
    <xsl:apply-templates />
  
  </quotation>
</xsl:template>


<xsl:template match="title" mode="admonition.title.mode">
  <xsl:apply-templates />
</xsl:template>

</xsl:stylesheet>
