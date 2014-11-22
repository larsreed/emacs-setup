<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: division.xsl,v 1.8 2000/08/21 16:17:18 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->
<xsl:template match="/">
  <xsl:variable name="isnode">
    <xsl:apply-templates select="child::*" mode="texinfo.isnode.mode" />
  </xsl:variable>
              
  <xsl:choose>

    <!-- Set entails multiple files, so special case -->
    <xsl:when test="/set">
      <xsl:apply-templates />
    </xsl:when>
    
    <!-- Otherwise, document element is always texinfo -->
    <xsl:when test="$isnode = '1'">
      <texinfo>
        <xsl:apply-templates />
      </texinfo>
    </xsl:when>

    <!-- Non-node elements need to have a Top node created for them -->
    <xsl:otherwise>
      <texinfo>
        <xsl:for-each select="child::*">
          <xsl:call-template name="texinfo.node" />
          <xsl:call-template name="texinfo.section" />
        </xsl:for-each>

        <xsl:apply-templates />
      </texinfo>
    </xsl:otherwise>

  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="set">
  <texinfoset>
    <xsl:apply-templates />
  </texinfoset>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="set/book">
  <texinfo>
    <xsl:attribute name="file">
      <xsl:call-template name="texinfo.filename" />
    </xsl:attribute>

    <xsl:call-template name="texinfo.node" />
    <xsl:call-template name="texinfo.section" />

    <xsl:apply-templates />
  </texinfo>
</xsl:template>

<xsl:template match="book">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="part">
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="partintro">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section">
    <xsl:with-param name="level">majorheading</xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->


</xsl:stylesheet>

