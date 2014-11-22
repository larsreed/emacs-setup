<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: docbook.xsl,v 1.10 2000/08/22 01:23:58 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:include href="VERSION" />

<xsl:include href="param.xsl"/>

<xsl:include href="../lib/lib.xsl"/>
<xsl:include href="../common/l10n.xsl"/>
<xsl:include href="../common/common.xsl"/>

<xsl:include href="autotoc.xsl"/>
<xsl:include href="lists.xsl"/>
<xsl:include href="callout.xsl"/>
<xsl:include href="verbatim.xsl"/>
<xsl:include href="graphics.xsl"/>
<xsl:include href="xref.xsl"/>
<xsl:include href="formal.xsl"/>
<xsl:include href="table.xsl"/>
<xsl:include href="sections.xsl"/>
<xsl:include href="inline.xsl"/>
<xsl:include href="footnote.xsl"/>
<xsl:include href="info.xsl"/>
<xsl:include href="keywords.xsl"/>
<xsl:include href="division.xsl"/>
<xsl:include href="toc.xsl"/>
<xsl:include href="index.xsl"/>
<xsl:include href="refentry.xsl"/>
<xsl:include href="math.xsl"/>
<xsl:include href="admon.xsl"/>
<xsl:include href="component.xsl"/>
<xsl:include href="biblio.xsl"/>
<xsl:include href="glossary.xsl"/>
<xsl:include href="block.xsl"/>
<xsl:include href="qandaset.xsl"/>
<xsl:include href="synop.xsl"/>
<!--
<xsl:include href="titlepage.xsl"/>
<xsl:include href="titlepage.templates.xsl"/>
-->
<xsl:include href="pi.xsl"/>

<xsl:include href="texinode-base.xsl" />
<xsl:include href="texinode.xsl" />
<xsl:include href="beginpage.xsl" />
<xsl:include href="caption.xsl" />

<xsl:include href="force-inline.xsl" />
<xsl:include href="menudescrip.xsl" />

<!-- ==================================================================== -->

<xsl:output method="xml" />

<!-- ==================================================================== -->

<xsl:template match="*">
  <xsl:call-template name="user.message">
    <xsl:with-param name="key">Element not matched by any template</xsl:with-param>
  </xsl:call-template>

  <xsl:comment>
    <xsl:call-template name="gentext.notmatched" />
  </xsl:comment>
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="text()">
  <xsl:value-of select="."/> 
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
