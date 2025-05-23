<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:fo="http://www.w3.org/1999/XSL/Format"
                version='1.0'>

<!-- ********************************************************************
     $Id: titlepage.article.xsl,v 1.6 2000/08/08 12:05:11 nwalsh Exp $
     ********************************************************************

     This file is part of the XSL DocBook Stylesheet distribution.
     See ../README or http://nwalsh.com/docbook/xsl/ for copyright
     and other information.

     ******************************************************************** -->

<xsl:template name="article.titlepage.recto">
  <xsl:variable name="ptitle" select="./title"/>
  <xsl:variable name="ititle" select="./artheader/title|./articleinfo/title"/>

  <!-- handle the title -->
  <xsl:choose>
    <xsl:when test="$ptitle">
      <xsl:apply-templates mode="article.titlepage.recto.mode"
       select="$ptitle"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$ititle">
        <xsl:apply-templates mode="article.titlepage.recto.mode"
         select="$ititle"/>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:variable name="psubtitle" select="./subtitle"/>
  <xsl:variable name="isubtitle" select="./artheader/subtitle|./articleinfo/subtitle"/>

  <!-- handle the subtitle -->
  <xsl:choose>
    <xsl:when test="$psubtitle">
      <xsl:apply-templates mode="article.titlepage.recto.mode"
       select="$psubtitle"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$isubtitle">
        <xsl:apply-templates mode="article.titlepage.recto.mode"
         select="$isubtitle"/>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/corpauthor|./articleinfo/corpauthor"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/authorgroup|./articleinfo/authorgroup"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/author|./articleinfo/author"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/releaseinfo|./articleinfo/releaseinfo"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/copyright|./articleinfo/copyright"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/pubdate|./articleinfo/pubdate"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/revision|./articleinfo/revision"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/revhistory|./articleinfo/revhistory"/>

  <xsl:apply-templates mode="article.titlepage.recto.mode"
   select="./artheader/abstract|./articleinfo/abstract"/>
</xsl:template>

<xsl:template name="article.titlepage.verso">
  <xsl:variable name="ptitle" select="./title"/>
  <xsl:variable name="ititle" select="./artheader/title|./articleinfo/title"/>

  <!-- handle the title -->
  <!--
  <xsl:choose>
    <xsl:when test="$ptitle">
      <xsl:apply-templates mode="article.titlepage.verso.mode"
       select="$ptitle"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$ititle">
        <xsl:apply-templates mode="article.titlepage.verso.mode"
         select="$ititle"/>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
  -->

  <xsl:variable name="psubtitle" select="./subtitle"/>
  <xsl:variable name="isubtitle" select="./artheader/subtitle|./articleinfo/subtitle"/>

  <!-- handle the subtitle -->
  <!--
  <xsl:choose>
    <xsl:when test="$psubtitle">
      <xsl:apply-templates mode="article.titlepage.verso.mode"
       select="$psubtitle"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:if test="$isubtitle">
        <xsl:apply-templates mode="article.titlepage.verso.mode"
         select="$isubtitle"/>
      </xsl:if>
    </xsl:otherwise>
  </xsl:choose>
  -->

  <!--
  <xsl:apply-templates mode="article.titlepage.verso.mode"
   select="./artheader/authorgroup|./articleinfo/authorgroup"/>
  -->
</xsl:template>

<xsl:template name="article.titlepage">
  <fo:block>
    <xsl:call-template name="article.titlepage.before">
       <xsl:with-param name="side">recto</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="article.titlepage.recto"/>
    <xsl:call-template name="article.titlepage.before">
       <xsl:with-param name="side">verso</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="article.titlepage.verso"/>
    <xsl:call-template name="article.titlepage.separator"/>
  </fo:block>
</xsl:template>

<xsl:template name="article.titlepage.separator">
</xsl:template>

<xsl:template name="article.titlepage.before">
  <xsl:param name="side">recto</xsl:param>
</xsl:template>

<!-- article titlepage recto mode ======================================== -->

<xsl:template match="*" mode="article.titlepage.recto.mode">
  <!-- if an element isn't found in this mode, -->
  <!-- try the generic titlepage.mode -->
  <xsl:apply-templates select="." mode="titlepage.mode"/>
</xsl:template>

<!-- /article titlepage recto mode ======================================= -->

<!-- article titlepage verso mode ======================================== -->

<xsl:template match="*" mode="article.titlepage.verso.mode">
  <!-- if an element isn't found in this mode, -->
  <!-- try the generic titlepage.mode -->
  <xsl:apply-templates select="." mode="titlepage.mode"/>
</xsl:template>

<!-- /article titlepage verso mode ======================================= -->

</xsl:stylesheet>
