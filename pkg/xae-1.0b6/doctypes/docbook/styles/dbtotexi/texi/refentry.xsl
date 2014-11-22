<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<!DOCTYPE xsl:stylesheet [
<!ENTITY mdash "&#x2014;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: refentry.xsl,v 1.12 2000/08/28 03:38:42 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="reference">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="refentry">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="refentrytitle|refname" mode="title">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="refmeta">
</xsl:template>

<xsl:template match="manvolnum">
  <xsl:if test="$refentry.xref.manvolnum">
    <xsl:text>(</xsl:text>
    <xsl:apply-templates/>
    <xsl:text>)</xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template match="refmiscinfo">
</xsl:template>

<xsl:template match="refentrytitle">
  <xsl:call-template name="inline.charseq"/>
</xsl:template>

<xsl:template match="refnamediv">
  <xsl:choose>
    <xsl:when test="$refentry.generate.name">
      
      <xsl:call-template name="texinfo.anchor" />
      
      <xsl:call-template name="texinfo.section">
        <xsl:with-param name="level">
          <xsl:call-template name="texinfo.section.level">
            <xsl:with-param name="heading.class" select="'chapheading'" />
          </xsl:call-template>
        </xsl:with-param>
        <xsl:with-param name="title">
          <xsl:call-template name="gentext.element" />
        </xsl:with-param>
      </xsl:call-template>

      <xsl:apply-templates />
      
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="block.object"/>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="refname">
  <xsl:if test="preceding-sibling::refname">
    <xsl:call-template name="gentext.char">
      <xsl:with-param name="key" select="'inline.separator'" />
    </xsl:call-template>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="refpurpose">
  <xsl:text> &mdash; </xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="refdescriptor">
  <!-- todo: finish this -->
</xsl:template>

<xsl:template match="refclass">
  <para>
      <xsl:if test="@role">
        <xsl:value-of select="@role"/>
        <xsl:text>: </xsl:text>
      </xsl:if>
      <xsl:apply-templates/>
  </para>
</xsl:template>

<xsl:template match="refsynopsisdiv">
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

<xsl:template match="refsect1|refsect2|refsect3">
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
<!-- FIXME move to common -->
<xsl:template match="refsynopsisdiv"
              mode="title.content">
  <xsl:param name="text-only" select="false()"/>
  
  <xsl:choose>
    <xsl:when test="./title">
      <xsl:apply-templates select="./title" mode="title.content">
        <xsl:with-param name="text-only" select="$text-only"/>
      </xsl:apply-templates>
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="gentext.element.name" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
