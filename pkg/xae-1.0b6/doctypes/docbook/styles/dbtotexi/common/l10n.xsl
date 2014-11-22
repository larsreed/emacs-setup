<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:gt="http://docbook2x.sourceforge.net/xmlns/xslt/gentext"
                exclude-result-prefixes="gt"
                version='1.0'>

<!-- ********************************************************************
     $Id: l10n.xsl,v 1.4 2000/08/21 22:01:40 stevecheng Exp $
     ********************************************************************

     This file is part of the XSL DocBook Stylesheet distribution.
     See ../README or http://nwalsh.com/docbook/xsl/ for copyright
     and other information.

     This file contains localization templates (for internationalization)
     ******************************************************************** -->

<xsl:variable name="l10n.xml" select="document('l10n.xml')"/>

<xsl:variable name="l10n.gentext.language"></xsl:variable>
<xsl:variable name="l10n.gentext.default.language">en</xsl:variable>

<xsl:template name="l10n.language">
  <xsl:param name="node" select="." />
  <xsl:choose>
    <xsl:when test="$l10n.gentext.language != ''">
      <xsl:value-of select="$l10n.gentext.language"/>
    </xsl:when>

    <xsl:otherwise>
      <xsl:variable name="lang-attr" 
                    select="(ancestor-or-self::*/@lang
                             |ancestor-or-self::*/@xml:lang)[last()]"/>
      <xsl:choose>
        <xsl:when test="string($lang-attr) = ''">
          <xsl:value-of select="$l10n.gentext.default.language"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$lang-attr"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="gentext.element">
  <xsl:param name="node" select="." />
  <xsl:param name="element.name" select="local-name($node)" />
  <xsl:param name="lang">
    <xsl:call-template name="l10n.language">
      <xsl:with-param name="node" select="$node" />
    </xsl:call-template>
  </xsl:param>
  
  <xsl:variable name="l10n.text"
                select="($l10n.xml/gt:internationalization/gt:localization[@lang=$lang]/gt:element[@name=$element.name])[last()]" />
  
  <xsl:choose>
    <xsl:when test="$l10n.text=''">
      <xsl:call-template name="user.message">
        <xsl:with-param name="key">Localized text not found</xsl:with-param>
        <xsl:with-param name="node" select="$node" />
        <xsl:with-param name="arg" select="$lang" />
      </xsl:call-template>
    </xsl:when>

    <xsl:otherwise>
      <xsl:apply-templates select="$l10n.text/node()" mode="gentext.mode">
        <xsl:with-param name="node" select="$node" />
      </xsl:apply-templates>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="gt:nodename" mode="gentext.mode">
  <xsl:param name="node" />
  <xsl:value-of select="name($node)" />
</xsl:template>

<xsl:template match="text()" mode="gentext.mode">
  <xsl:value-of select="." />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="gentext.notmatched">
  <xsl:param name="node" select="." />
  <xsl:param name="lang">
    <xsl:call-template name="l10n.language">
      <xsl:with-param name="node" select="$node" />
    </xsl:call-template>
  </xsl:param>
  <xsl:apply-templates select="($l10n.xml/gt:internationalization/gt:localization[@lang=$lang]/gt:notmatched)[last()]" mode="gentext.mode">
    <xsl:with-param name="node" select="$node" />
  </xsl:apply-templates>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="gentext.char">
  <xsl:param name="node" select="." />
  <xsl:param name="lang">
    <xsl:call-template name="l10n.language">
      <xsl:with-param name="node" select="$node" />
    </xsl:call-template>
  </xsl:param>
  <xsl:param name="key" />
  
  <xsl:value-of
    select="($l10n.xml/gt:internationalization/gt:localization[@lang=$lang]/gt:char[@key=$key])[last()]" />
</xsl:template>

<!-- ==================================================================== -->

<!-- Backward compatability(!) with common.xsl -->
<xsl:template name="gentext.space">
  <xsl:text> </xsl:text>
</xsl:template>
<xsl:template name="gentext.element.name">
  <xsl:param name="node" select="." />
  <xsl:param name="element.name" select="local-name($node)" />
  <xsl:param name="lang">
    <xsl:call-template name="l10n.language">
      <xsl:with-param name="node" select="$node" />
    </xsl:call-template>
  </xsl:param>
  <xsl:call-template name="gentext.element">
    <xsl:with-param name="node" select="$node" />
    <xsl:with-param name="element.name" select="$element.name" />
    <xsl:with-param name="lang" select="$lang" />
  </xsl:call-template>
</xsl:template>

<!-- ==================================================================== -->

<xsl:variable name="message.doc"
  select="$l10n.xml/gt:internationalization/gt:localization[@lang=$user.message.lang]" />

<xsl:template name="user.message">
  <xsl:param name="node" select="." />
  <xsl:param name="arg" />
  <xsl:param name="key" />

  <xsl:variable name="message" 
    select="$message.doc/gt:message[@key=$key]" />
  
  <xsl:message>
    <!-- Just being more helpful -->
    <xsl:value-of select="name($node/..)" />
    <xsl:text>/</xsl:text>
  
    <xsl:value-of select="name($node)" />
    <xsl:text>: </xsl:text>

    <xsl:if test="$arg != ''">
      <xsl:copy-of select="$arg" />
      <xsl:text>: </xsl:text>
    </xsl:if>
    
    <xsl:choose>
      <xsl:when test="$message != ''">
        <xsl:copy-of select="$message/node()" />
      </xsl:when>
      <xsl:otherwise>
        <!-- Allow fallback if text is not available -->
        <xsl:value-of select="$key" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:message>
</xsl:template>
   
</xsl:stylesheet>

