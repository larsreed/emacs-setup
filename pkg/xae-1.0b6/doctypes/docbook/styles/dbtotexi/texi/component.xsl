<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: component.xsl,v 1.5 2000/08/21 16:17:18 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->


<!-- ==================================================================== -->

<xsl:template match="dedication">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section">
    <xsl:with-param name="level">
      <xsl:call-template name="texinfo.section.level">
        <xsl:with-param name="heading.class" select="'unnumbered'" />
      </xsl:call-template>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="colophon">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="preface">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section">
    <xsl:with-param name="level">
      <xsl:call-template name="texinfo.section.level">
        <xsl:with-param name="heading.class" select="'unnumbered'" />
      </xsl:call-template>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="chapter">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="appendix">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section">
    <xsl:with-param name="level">
      <xsl:call-template name="texinfo.section.level">
        <xsl:with-param name="heading.class" select="'appendix'" />
      </xsl:call-template>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->


<xsl:template match="article">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>
 
<xsl:template match="bibliography">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="glossary">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="index">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
 
