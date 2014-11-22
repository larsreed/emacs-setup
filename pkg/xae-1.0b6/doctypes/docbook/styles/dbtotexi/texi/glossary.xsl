<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:gt="http://docbook2x.sourceforge.net/xmlns/xslt/gentext"
                exclude-result-prefixes="gt"
                version='1.0'>

<!-- ********************************************************************
     $Id: glossary.xsl,v 1.8 2000/08/21 22:01:39 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="glossary">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="glosslist">
  <table>
    <xsl:apply-templates />
  </table>
</xsl:template>
  
<!-- ==================================================================== -->

<xsl:template match="glossdiv">
  <xsl:call-template name="texinfo.anchor" />
  <xsl:call-template name="texinfo.section">
    <xsl:with-param name="level">
      <xsl:call-template name="texinfo.section.level">
        <xsl:with-param name="heading.class" select="'chapheading'" />
      </xsl:call-template>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates />
</xsl:template>



<!-- ==================================================================== -->

<!--
GlossEntry ::=
  GlossTerm, Acronym?, Abbrev?,
  (IndexTerm)*,
  RevHistory?,
  (GlossSee | GlossDef+)
-->

<xsl:template match="glossentry">
  <xsl:apply-templates/>
</xsl:template>
  
<xsl:template match="glossentry/glossterm">
  <item><xsl:apply-templates/></item>
</xsl:template>

<xsl:template match="glossentry/acronym">
  <item><xsl:apply-templates/></item>
</xsl:template>
  
<xsl:template match="glossentry/abbrev">
  <item><xsl:apply-templates/></item>
</xsl:template>
  
<xsl:template match="glossentry/revhistory">
  <item><xsl:apply-templates/></item>
</xsl:template>
  
<xsl:template match="glossentry/glosssee">
  <para>
    <xsl:call-template name="gentext.element"/>
  </para>
</xsl:template>

<xsl:template match="gt:element[@name='glosssee']/gt:term"
              mode="gentext.mode">
  <xsl:param name="node" />
  <xsl:choose>
    <xsl:when test="$node/@otherterm">
      <xsl:variable name="targets" select="id($node/@otherterm)" />
      <xsl:variable name="target" select="$targets[1]"/>
      <xsl:apply-templates select="$target" mode="xref" /><!-- FIXME -->
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="$node" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
<xsl:template match="gt:element[@name='glossseealso']/gt:term"
              mode="gentext.mode">
  <xsl:param name="node" />
  <xsl:choose>
    <xsl:when test="$node/@otherterm">
      <xsl:variable name="targets" select="id($node/@otherterm)" />
      <xsl:variable name="target" select="$targets[1]"/>
      <xsl:apply-templates select="$target" mode="xref" /><!-- FIXME -->
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates select="$node" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="glossentry/glossdef">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="glossseealso">
  <para>
    <xsl:call-template name="gentext.element"/>
  </para>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="glossentry" mode="xref">
  <xsl:apply-templates select="./glossterm[1]" mode="xref"/>
</xsl:template>

<xsl:template match="glossterm" mode="xref">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
