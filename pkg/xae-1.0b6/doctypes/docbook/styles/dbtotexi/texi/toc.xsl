<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: toc.xsl,v 1.1 2000/07/08 21:26:41 steve Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="*" mode="toc">
</xsl:template>

<xsl:template match="toc">
</xsl:template>

<xsl:template match="tocpart|tocchap|tocfront|tocback|tocentry">
</xsl:template>

<xsl:template match="toclevel1|toclevel2|toclevel3|toclevel4|toclevel5">
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="lot|lotentry">
</xsl:template>

</xsl:stylesheet>
