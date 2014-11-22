<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: xref.xsl,v 1.8 2000/08/20 03:16:34 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="anchor">
  <xsl:call-template name="texinfo.anchor" />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="xref">
  <xsl:variable name="targets" select="id(@linkend)"/>
  <xsl:variable name="target" select="$targets[1]"/>

  <xsl:call-template name="check.id.unique">
    <xsl:with-param name="linkend" select="@linkend"/>
  </xsl:call-template>

  <xsl:choose>
    <xsl:when test="not($target)">
      <xsl:call-template name="user.message">
        <xsl:with-param name="arg" select="@linkend" />
        <xsl:with-param name="key">Reference to non-existant ID</xsl:with-param>
      </xsl:call-template>

      <!-- FIXME: i18n? ;) -->
      <xsl:text>[XRef to non-existent ID </xsl:text>
      <xsl:value-of select="@linkend" />
      <xsl:text>]</xsl:text>
    </xsl:when>

    <xsl:otherwise>
    <ref>
      <xsl:attribute name="node">
        <xsl:call-template name="texinfo.nodename">
          <xsl:with-param name="node" select="$target" />
        </xsl:call-template>
      </xsl:attribute>

      <xsl:attribute name="file">
        <xsl:call-template name="texinfo.filename">
          <xsl:with-param name="node" select="$target" />
        </xsl:call-template>
      </xsl:attribute>

      <!-- FIXME Printed manual name -->

      <xsl:choose>
        <xsl:when test="$target/@xreflabel">
          <xsl:call-template name="xref.xreflabel">
            <xsl:with-param name="target" select="$target" />
          </xsl:call-template>
        </xsl:when>

        <xsl:otherwise>
          <!-- norm's stylesheets has a whole bunch of cases for this,
               but (1) some of this is done by Texinfo
                   (2) Texinfo nodenames are required to be descriptive anyways -->
          <xsl:apply-templates select="$target" mode="title.content" />
        </xsl:otherwise>
      </xsl:choose>
    </ref>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

        
<!-- ==================================================================== -->

<xsl:template match="link">
  <xsl:variable name="targets" select="id(@linkend)"/>
  <xsl:variable name="target" select="$targets[1]"/>

  <xsl:call-template name="check.id.unique">
    <xsl:with-param name="linkend" select="@linkend"/>
  </xsl:call-template>

  <xsl:choose>
    <xsl:when test="$link.use.pxref">
      <xsl:apply-templates />
      <xsl:text> (</xsl:text>
    
      <pxref>
        <xsl:attribute name="node">
          <xsl:call-template name="texinfo.nodename">
            <xsl:with-param name="node" select="$target" />
          </xsl:call-template>
        </xsl:attribute>

        <xsl:attribute name="file">
          <xsl:call-template name="texinfo.filename">
            <xsl:with-param name="node" select="$target" />
          </xsl:call-template>
        </xsl:attribute>
      </pxref>
    
      <xsl:text>)</xsl:text>
    </xsl:when>

    <xsl:otherwise>
      <ref>
        <xsl:attribute name="node">
          <xsl:call-template name="texinfo.nodename">
            <xsl:with-param name="node" select="$target" />
          </xsl:call-template>
        </xsl:attribute>

        <xsl:attribute name="file">
          <xsl:call-template name="texinfo.filename">
            <xsl:with-param name="node" select="$target" />
          </xsl:call-template>
        </xsl:attribute>

        <!-- FIXME Printed manual name -->

        <xsl:apply-templates />
      </ref>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="ulink">
  <uref>
    <xsl:attribute name="url"><xsl:value-of select="@url"/></xsl:attribute>
    <xsl:apply-templates/>
  </uref>
</xsl:template>

<xsl:template match="olink">
  <xsl:apply-templates/>
</xsl:template>


<!-- ==================================================================== -->

<xsl:template name="xref.xreflabel">
  <!-- called to process an xreflabel...you might use this to make  -->
  <!-- xreflabels come out in the right font for different targets, -->
  <!-- for example. -->
  <xsl:param name="target" select="."/>
  <xsl:value-of select="$target/@xreflabel"/>
</xsl:template>

<xsl:template name="title.xref" />
<xsl:template name="number.xref" />


<!-- ==================================================================== -->

</xsl:stylesheet>
