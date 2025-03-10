<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: admon.xsl,v 1.9 2000/03/23 19:06:08 nwalsh Exp $
     ********************************************************************

     This file is part of the XSL DocBook Stylesheet distribution.
     See ../README or http://nwalsh.com/docbook/xsl/ for copyright
     and other information.

     ******************************************************************** -->

<xsl:template match="note|important|warning|caution|tip">
  <xsl:choose>
    <xsl:when test="$admon.graphics != 0">
      <xsl:call-template name="graphical.admonition"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="nongraphical.admonition"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="admon.graphic.width">
  <xsl:param name="node" select="."/>
  <xsl:text>25</xsl:text>
</xsl:template>

<xsl:template name="admon.graphic">
  <xsl:param name="node" select="."/>
  <xsl:value-of select="$admon.graphics.path"/>
  <xsl:choose>
    <xsl:when test="name($node)='note'">note.gif</xsl:when>
    <xsl:when test="name($node)='warning'">warning.gif</xsl:when>
    <xsl:when test="name($node)='caution'">caution.gif</xsl:when>
    <xsl:when test="name($node)='tip'">tip.gif</xsl:when>
    <xsl:when test="name($node)='important'">important.gif</xsl:when>
    <xsl:otherwise>note.gif</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="graphical.admonition">
  <div class="{name(.)}">
  <xsl:if test="$admon.style">
    <xsl:attribute name="style">
      <xsl:value-of select="$admon.style"/>
    </xsl:attribute>
  </xsl:if>
  <table border="0">
    <tr>
      <td rowspan="2" align="center" valign="top">
        <xsl:attribute name="width">
          <xsl:call-template name="admon.graphic.width"/>
        </xsl:attribute>
        <img>
          <xsl:attribute name="src">
            <xsl:call-template name="admon.graphic"/>
          </xsl:attribute>
        </img>
      </td>
      <th>
        <xsl:choose>
          <xsl:when test="./title">
            <xsl:apply-templates select="./title" 
                                 mode="graphic.admonition.title.mode"/>
          </xsl:when>
          <xsl:otherwise>
            <a>
              <xsl:attribute name="name">
                <xsl:call-template name="object.id"/>
              </xsl:attribute>
              <xsl:call-template name="gentext.element.name"/>
            </a>
          </xsl:otherwise>
        </xsl:choose>
      </th>
    </tr>
    <tr>
      <td colspan="2" align="left" valign="top">
        <xsl:apply-templates/>
      </td>
    </tr>
  </table>
  </div>
</xsl:template>

<xsl:template name="nongraphical.admonition">
  <div class="{name(.)}">
  <xsl:if test="$admon.style">
    <xsl:attribute name="style">
      <xsl:value-of select="$admon.style"/>
    </xsl:attribute>
  </xsl:if>
  <xsl:choose>
    <xsl:when test="./title">
      <xsl:apply-templates select="./title" mode="admonition.title.mode"/>
    </xsl:when>
    <xsl:otherwise>
      <h3 class="title">
        <a>
          <xsl:attribute name="name">
            <xsl:call-template name="object.id"/>
          </xsl:attribute>
          <xsl:call-template name="gentext.element.name"/>
        </a>
      </h3>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:apply-templates/>
  </div>
</xsl:template>

<xsl:template match="note/title"></xsl:template>
<xsl:template match="important/title"></xsl:template>
<xsl:template match="warning/title"></xsl:template>
<xsl:template match="caution/title"></xsl:template>
<xsl:template match="tip/title"></xsl:template>

<xsl:template match="title" mode="admonition.title.mode">
  <xsl:variable name="id">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select=".."/>
    </xsl:call-template>
  </xsl:variable>
  <h3 class="title">
    <a name="{$id}">
      <xsl:apply-templates/>
    </a>
  </h3>
</xsl:template>

<xsl:template match="title" mode="graphic.admonition.title.mode">
  <xsl:variable name="id">
    <xsl:call-template name="object.id">
      <xsl:with-param name="object" select=".."/>
    </xsl:call-template>
  </xsl:variable>
  <b class="title">
    <a name="{$id}">
      <xsl:apply-templates/>
    </a>
  </b>
</xsl:template>

</xsl:stylesheet>
