<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: l10n.xsl,v 1.12 2000/02/16 16:18:28 nwalsh Exp $
     ********************************************************************

     This file is part of the XSL DocBook Stylesheet distribution.
     See ../README or http://nwalsh.com/docbook/xsl/ for copyright
     and other information.

     This file contains localization templates (for internationalization)
     ******************************************************************** -->

<xsl:variable name="l10n.xml" select="document('../common/l10n.xml')"/>

<xsl:variable name="l10n.gentext.language"></xsl:variable>
<xsl:variable name="l10n.gentext.default.language">en</xsl:variable>
<xsl:variable name="l10n.gentext.use.xref.language" select="false()"/>

<xsl:template name="l10n.language">
  <xsl:param name="target" select="."/>
  <xsl:param name="xref-context" select="false()"/>

  <xsl:variable name="language">
    <xsl:choose>
      <xsl:when test="$l10n.gentext.language != ''">
        <xsl:value-of select="$l10n.gentext.language"/>
      </xsl:when>

      <xsl:when test="$xref-context or $l10n.gentext.use.xref.language">
        <xsl:variable name="lang-attr"
                      select="($target/ancestor-or-self::*/@lang
                               |$target/ancestor-or-self::*/@xml:lang)[last()]"/>
        <xsl:choose>
          <xsl:when test="string($lang-attr) = ''">
            <xsl:value-of select="$l10n.gentext.default.language"/>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="$lang-attr"/>
          </xsl:otherwise>
        </xsl:choose>
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
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="contains($language,'-')">
      <xsl:value-of select="substring-before($language,'-')"/>
      <xsl:text>_</xsl:text>
      <xsl:value-of select="substring-after($language,'-')"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$language"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="gentext.xref.text">
  <xsl:param name="element.name" select="name(.)"/>
  <xsl:param name="default"></xsl:param>
  <xsl:param name="lang">
    <xsl:call-template name="l10n.language"/>
  </xsl:param>

  <xsl:variable name="l10n.text">
    <xsl:value-of select="($l10n.xml/internationalization/localization[@language=$lang]/xref[@element=$element.name])[1]/@text"/>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$l10n.text=''">
      <xsl:choose>
        <xsl:when test="$default=''">
          <xsl:message>
            <xsl:text>No "</xsl:text>
            <xsl:value-of select="$lang"/>
            <xsl:text>" cross reference text for "</xsl:text>
            <xsl:value-of select="$element.name"/>
            <xsl:text>" exists and no default specified.</xsl:text>
          </xsl:message>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$default"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$l10n.text"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="gentext.element.name">
  <xsl:param name="element.name" select="name(.)"/>
  <xsl:param name="lang">
    <xsl:call-template name="l10n.language"/>
  </xsl:param>

  <xsl:variable name="l10n.name">
    <xsl:value-of select="($l10n.xml/internationalization/localization[@language=$lang]/gentext[@key=$element.name])[1]/@text"/>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$l10n.name=''">
      <xsl:message>
        <xsl:text>No "</xsl:text>
        <xsl:value-of select="$lang"/>
        <xsl:text>" localization of "</xsl:text>
        <xsl:value-of select="$element.name"/>
        <xsl:text>" exists; using "en".</xsl:text>
      </xsl:message>

      <xsl:value-of select="($l10n.xml/internationalization/localization[@language='en']/gentext[@key=$element.name])[1]/@text"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$l10n.name"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="gentext.space">
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template name="gentext.edited.by">
  <xsl:call-template name="gentext.element.name">
    <xsl:with-param name="element.name">Editedby</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.by">
  <xsl:call-template name="gentext.element.name">
    <xsl:with-param name="element.name">by</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.dingbat">
  <xsl:param name="dingbat">bullet</xsl:param>
  <xsl:param name="lang">
    <xsl:call-template name="l10n.language"/>
  </xsl:param>

  <xsl:variable name="l10n.dingbat">
    <xsl:value-of select="($l10n.xml/internationalization/localization[@language=$lang]/dingbat[@key=$dingbat])[1]/@text"/>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$l10n.dingbat=''">
      <xsl:message>
        <xsl:text>No "</xsl:text>
        <xsl:value-of select="$lang"/>
        <xsl:text>" localization of dingbat </xsl:text>
        <xsl:value-of select="$dingbat"/>
        <xsl:text> exists; using "en".</xsl:text>
      </xsl:message>

      <xsl:value-of select="($l10n.xml/internationalization/localization[@language='en']/dingbat[@key=$dingbat])[1]/@text"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$l10n.dingbat"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template name="gentext.startquote">
  <xsl:call-template name="gentext.dingbat">
    <xsl:with-param name="dingbat">startquote</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.endquote">
  <xsl:call-template name="gentext.dingbat">
    <xsl:with-param name="dingbat">endquote</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.nestedstartquote">
  <xsl:call-template name="gentext.dingbat">
    <xsl:with-param name="dingbat">nestedstartquote</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.nestedendquote">
  <xsl:call-template name="gentext.dingbat">
    <xsl:with-param name="dingbat">nestedendquote</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.nav.prev">
  <xsl:call-template name="gentext.element.name">
    <xsl:with-param name="element.name">nav-prev</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.nav.next">
  <xsl:call-template name="gentext.element.name">
    <xsl:with-param name="element.name">nav-next</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.nav.home">
  <xsl:call-template name="gentext.element.name">
    <xsl:with-param name="element.name">nav-home</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.nav.up">
  <xsl:call-template name="gentext.element.name">
    <xsl:with-param name="element.name">nav-up</xsl:with-param>
  </xsl:call-template>
</xsl:template>

</xsl:stylesheet>

