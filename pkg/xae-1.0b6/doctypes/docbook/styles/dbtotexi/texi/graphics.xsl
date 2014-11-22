<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: graphics.xsl,v 1.5 2000/08/15 20:22:28 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="screenshot">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="screeninfo">
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="graphic[@fileref]">
  <para>
    <image file="{@fileref}" />
  </para>
</xsl:template>

<xsl:template match="graphic[@entityref]">
  <xsl:call-template name="user.message">
    <xsl:with-param name="key">Unparsed entity reference not supported</xsl:with-param>
  </xsl:call-template>

  <para>
    <image file="{unparsed-entity-uri(@entityref)}" />
  </para>
</xsl:template>

<xsl:template match="inlinegraphic[@fileref]">
  <image file="{@fileref}" />
</xsl:template>

<xsl:template match="inlinegraphic[@entityref]">
  <xsl:call-template name="user.message">
    <xsl:with-param name="key">Unparsed entity reference not supported</xsl:with-param>
  </xsl:call-template>

  <image file="{unparsed-entity-uri(@entityref)}" />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="select.graphic.object">
  <xsl:param name="olist"
             select="imageobject|videoobject|audioobject|textobject"/>
  <xsl:param name="count">1</xsl:param>

  <xsl:if test="$count &lt;= count($olist)">
    <xsl:variable name="object" select="$olist[position()=$count]"/>

    <xsl:variable name="useobject">
      <xsl:choose>
        <xsl:when test="$object/self::textobject and $object/phrase">
          <xsl:text>0</xsl:text>
        </xsl:when>
        <xsl:when test="$object/self::textobject">
          <xsl:text>1</xsl:text>
        </xsl:when>
        <xsl:otherwise>
          <xsl:call-template name="is.graphic.object">
            <xsl:with-param name="object" select="$object"/>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:variable>

    <xsl:choose>
      <xsl:when test="$useobject='1'">
        <xsl:apply-templates select="$object"/>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="select.graphic.object">
          <xsl:with-param name="olist" select="$olist"/>
          <xsl:with-param name="count" select="$count + 1"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
</xsl:template>

<xsl:template name="is.graphic.object">
  <xsl:param name="object"></xsl:param>

  <xsl:variable name="data" select="$object/videodata
                                    |$object/imagedata
                                    |$object/audiodata"/>

  <xsl:variable name="filename">
    <xsl:call-template name="mediaobject.filename">
      <xsl:with-param name="object" select="$object"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="ext">
    <xsl:call-template name="filename-extension">
      <xsl:with-param name="filename" select="$filename"/>
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="format" select="$data/@format"/>

  <xsl:variable name="graphic.format">
    <xsl:if test="$format">
      <xsl:call-template name="is.graphic.format">
        <xsl:with-param name="format" select="$format"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:variable>

  <xsl:variable name="graphic.ext">
    <xsl:if test="$ext">
      <xsl:call-template name="is.graphic.extension">
        <xsl:with-param name="ext" select="$ext"/>
      </xsl:call-template>
    </xsl:if>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="$graphic.format = '1'">1</xsl:when>
    <xsl:when test="$graphic.ext = '1'">1</xsl:when>
    <xsl:otherwise>0</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="mediaobject">
  <para>
    <xsl:call-template name="select.graphic.object"/>
    <xsl:apply-templates select="caption"/>
  </para>
</xsl:template>

<xsl:template match="inlinemediaobject">
  <xsl:call-template name="select.graphic.object"/>
</xsl:template>

<xsl:template match="imageobject">
  <xsl:apply-templates select="imagedata"/>
</xsl:template>

<xsl:template match="imagedata">
  <xsl:variable name="filename">
    <xsl:call-template name="mediaobject.filename">
      <xsl:with-param name="object" select=".."/>
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="alt">
    <xsl:apply-templates select="(../../textobject/phrase)[1]"/>
  </xsl:variable>

  <image file="{$filename}" />
</xsl:template>

<xsl:template match="videoobject">
  <xsl:apply-templates select="videodata"/>
</xsl:template>

<xsl:template match="videodata">
  <xsl:call-template name="user.message">
    <xsl:with-param name="key">Video data not supported</xsl:with-param>
  </xsl:call-template>
</xsl:template>


<xsl:template match="audioobject">
  <xsl:apply-templates select="audiodata"/>
</xsl:template>

<xsl:template match="audiodata">
  <xsl:call-template name="user.message">
    <xsl:with-param name="key">Audio data not supported</xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="textobject">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="caption">
  <para>
    <xsl:apply-templates/>
  </para>
</xsl:template>

</xsl:stylesheet>
