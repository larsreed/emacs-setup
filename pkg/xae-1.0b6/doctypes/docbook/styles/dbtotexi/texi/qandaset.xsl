<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: qandaset.xsl,v 1.6 2000/08/22 16:29:35 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="qandaset">
  <xsl:variable name="title" select="title" />
  <xsl:variable name="rest" select="*[not(self::title)]" />

  <xsl:apply-templates select="$title" />

  <xsl:if test="$generate.qandaset.toc != '0'">
    <para>
      <xsl:call-template name="process.qanda.toc"/>
    </para>
  </xsl:if>

  <xsl:apply-templates select="$rest"/>
</xsl:template>

<xsl:template match="qandaset/title">
  <xsl:call-template name="caption" />
</xsl:template>

<xsl:template match="qandadiv">
  <xsl:variable name="title" select="title" />
  <xsl:variable name="rest" select="*[not(self::title)]" />

  <xsl:apply-templates select="$title" />

  <xsl:if test="$generate.qandadiv.toc != '0'">
    <xsl:call-template name="process.qanda.toc"/>
  </xsl:if>

  <xsl:apply-templates select="$rest"/>
</xsl:template>

<xsl:template match="qandadiv/title">
  <!-- FIXME qalevel numbering -->
  <para>
    <strong><xsl:apply-templates /></strong>
  </para>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="qandaentry">
  <xsl:call-template name="texinfo.anchor" />
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="question">
  <xsl:variable name="default.label" select="(ancestor::qandaset/@defaultlabel[string(.) != ''])[1]" />
  
  <para>
    <xsl:choose>
      <xsl:when test="./label">
        <xsl:apply-templates select="label" mode="qanda.label.mode" />
      </xsl:when>
    
      <xsl:when test="not($default.label) or $default.label = 'qanda'">
        <xsl:call-template name="gentext.element" />
      </xsl:when>

      <xsl:when test="$default.label = 'number'">
        <!-- FIXME numbering -->
      </xsl:when>

      <xsl:when test="$default.label = 'none'">
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="user.message">
          <xsl:with-param name="key">Unrecognized label type</xsl:with-param>
          <xsl:with-param name="arg" select="$default.label" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </para>
  <xsl:apply-templates />

</xsl:template>

<xsl:template match="answer">
  <xsl:variable name="default.label" select="(ancestor::qandaset/@defaultlabel[string(.) != ''])[1]" />
  
  <para>
    <xsl:choose>
      <xsl:when test="./label">
        <xsl:apply-templates select="label" mode="qanda.label.mode" />
      </xsl:when>

      <xsl:when test="not($default.label) or $default.label = 'qanda'">
        <xsl:call-template name="gentext.element" />
      </xsl:when>

      <xsl:when test="$default.label = 'number'">
        <!-- FIXME numbering -->
      </xsl:when>

      <xsl:when test="$default.label = 'none'">
      </xsl:when>

      <xsl:otherwise>
        <xsl:call-template name="user.message">
          <xsl:with-param name="key">Unrecognized label type</xsl:with-param>
          <xsl:with-param name="arg" select="$default.label" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </para>
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="label"></xsl:template>
<xsl:template match="label" mode="qanda.label.mode">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

<!-- ALL FIXME -->
<xsl:template name="process.qanda.toc">
</xsl:template>

<xsl:template match="qandadiv" mode="qandatoc.mode">
</xsl:template>

<xsl:template match="qandadiv/title" mode="qandatoc.mode">
</xsl:template>

<xsl:template match="qandaentry" mode="qandatoc.mode">
</xsl:template>

<xsl:template match="question" mode="qandatoc.mode">
</xsl:template>

<xsl:template match="answer" mode="qandatoc.mode">
  <!-- nop -->
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="*" mode="no.wrapper.mode">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
