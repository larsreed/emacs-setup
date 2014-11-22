<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: formal.xsl,v 1.3 2000/08/14 04:33:20 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<xsl:template name="formal.object">
  <xsl:call-template name="texinfo.anchor" />
  <xsl:call-template name="formal.object.heading">
    <xsl:with-param name="title">
      <xsl:apply-templates select="." mode="title.ref"/>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template name="formal.object.heading">
  <xsl:param name="title"></xsl:param>
  <xsl:call-template name="caption">
    <xsl:with-param name="content" select="$title" />
  </xsl:call-template>
</xsl:template>

<xsl:template name="informal.object">
  <xsl:call-template name="texinfo.anchor" />
  <xsl:apply-templates/>
</xsl:template>

<xsl:template name="semiformal.object">
  <xsl:choose>
    <xsl:when test="title">
      <xsl:call-template name="formal.object"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="informal.object"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="figure|table|example">
  <xsl:call-template name="formal.object"/>
</xsl:template>

<xsl:template match="equation">
  <xsl:call-template name="semiformal.object"/>
</xsl:template>

<xsl:template match="figure/title"></xsl:template>
<xsl:template match="table/title"></xsl:template>
<xsl:template match="example/title"></xsl:template>
<xsl:template match="equation/title"></xsl:template>

<xsl:template match="informalfigure">
  <xsl:call-template name="informal.object"/>
</xsl:template>

<xsl:template match="informalexample">
  <xsl:call-template name="informal.object"/>
</xsl:template>

<xsl:template match="informaltable">
  <xsl:call-template name="informal.object"/>
</xsl:template>

<xsl:template match="informalequation">
  <xsl:call-template name="informal.object"/>
</xsl:template>

</xsl:stylesheet>
