<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: info.xsl,v 1.2 2000/08/21 16:17:18 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- These templates define the "default behavior" for info
     elements.  Even if you don't process the *info wrappers,
     some of these elements are needed because the elements are
     processed from named templates that are called with modes.
     Since modes aren't sticky, these rules apply. 
     (TODO: clarify this comment) -->

<!-- ==================================================================== -->
<!-- called from named templates in a given mode -->

<xsl:template match="corpauthor">
    <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="jobtitle">
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="orgname">
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="orgdiv">
    <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

<!-- Ignore meta-info wrappers -->

<xsl:template 
  match="docinfo|prefaceinfo|chapterinfo|appendixinfo|articleinfo|artheader|glossaryinfo" />
<xsl:template 
  match="bookinfo|setinfo|seriesinfo" />
<xsl:template 
  match="referenceinfo|refentryinfo|refsect1info|refsect2info|refsect3info|refsynopsisdivinfo" />
<xsl:template 
  match="sect1info|sect2info|sect3info|sect4info|sect5info|sectioninfo" />
<xsl:template 
  match="objectinfo" />

<!-- ==================================================================== -->

</xsl:stylesheet>
