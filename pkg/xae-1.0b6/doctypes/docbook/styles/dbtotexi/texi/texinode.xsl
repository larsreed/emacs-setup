<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: texinode.xsl,v 1.7 2000/08/22 00:11:45 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->


<!-- ==================================================================== -->
<doc:mode mode="texinfo.isnode.mode">
<refpurpose>Determine if given element is a Texinfo node</refpurpose>
<refdescription>
<para>
Which elements are nodes?
</para>
<para>
We use this mainly only to allow <function>texinfo.section</function> to easily 
test if there is a previous Texinfo node.  And may be as some sort of 
documentation...
</para>
<para>
Note that because apply-templates results in a result tree fragment
(which is equivalent to a non-empty node-set), always return the
value of '1' and explicitly compare against it.
</para>
<para>
Elements that generate anchors are not considered nodes by this mode.
</para>
</refdescription>
</doc:mode>

<xsl:template match=
  "book|reference|partintro|dedication|preface|chapter|appendix|glossary|bibliography|article|refentry|sect1|sect2|sect3|sect4|sect5|section"
  mode="texinfo.isnode.mode">
  <xsl:value-of select="1" />
</xsl:template>

<!-- match="*" returns nothing -->
  

<!-- ==================================================================== -->

<doc:mode mode="texinfo.nodename.mode">
<refpurpose>Return a suggested nodename</refpurpose>
<refdescription>
<para>
Processing an element using texinfo.nodename.mode returns a suggested
nodename (or anchor name) for that element.
</para>
</refdescription>
</doc:mode>

<xsl:template match="*" mode="texinfo.nodename.mode">
  <xsl:if test="@xreflabel">
    <xsl:apply-templates select="@xreflabel" 
                         mode="texinfo.nodename-string.mode" />
  </xsl:if>
</xsl:template>

<!-- Top node.
     FIXME: Other nodes aren't allowed to be named 'Top', but we don't
     check that!  On the other hand, it's not a problem in most cases
     because the Top node will get referenced first and later ones will
     conflict with it. -->
<xsl:template match="book|/*" priority="5" mode="texinfo.nodename.mode">
  <xsl:value-of select="'Top'" />
</xsl:template>

<xsl:template match="reference|preface|chapter|appendix|glossary|bibliography" 
              mode="texinfo.nodename.mode">
  
  <!-- ARGH. What's the point of spliting docinfo into another thousand
       elements !? -->
  <xsl:variable name="info" 
    select="./docinfo|*[local-name(.)=concat(local-name(current()),'info')]" />
  
  <xsl:choose>
    <xsl:when test="$info/title[@role='texinfo-node']">
      <xsl:apply-templates select="$info/title[@role='texinfo-node']"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    
    <xsl:when test="@xreflabel">
      <xsl:apply-templates select="@xreflabel"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./titleabbrev">
      <xsl:apply-templates select="./titleabbrev"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./title">
      <xsl:apply-templates select="./title"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="article"
              mode="texinfo.nodename.mode">
  
  <xsl:variable name="info" select="./artheader|./articleinfo" />
  
  <xsl:choose>
    <xsl:when test="$info/title[@role='texinfo-node']">
      <xsl:apply-templates select="$info/title[@role='texinfo-node']"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    
    <xsl:when test="@xreflabel">
      <xsl:apply-templates select="@xreflabel"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./titleabbrev">
      <xsl:apply-templates select="./titleabbrev"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./title">
      <xsl:apply-templates select="./title"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>

    <!-- Titles on articles are optional.  It may be specified in 
         the article "header" -->
    <xsl:when test="$info/title[1]">
      <xsl:apply-templates select="$info/title[1]"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="refentry"
              mode="texinfo.nodename.mode">

  <xsl:variable name="info" select="./docinfo|./refentryinfo" />
  
  <xsl:choose>
    <xsl:when test="$info/title[@role='texinfo-node']">
      <xsl:apply-templates select="$info/title[@role='texinfo-node']"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    
    <xsl:when test="@xreflabel">
      <xsl:apply-templates select="@xreflabel"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./refmeta/refentrytitle">
      <xsl:apply-templates select="./refmeta/refentrytitle[1]"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./refnamediv/refname[1]">
      <xsl:apply-templates select="./refnamediv/refname[1]"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="sect1|sect2|sect3|sect4|sect5|section|refsect1|refsect2|refsect3"
              mode="texinfo.nodename.mode">

  <xsl:variable name="info" 
    select="*[local-name(.)=concat(local-name(current()),'info')]" />
  
  <xsl:choose>
    <xsl:when test="$info/title[@role='texinfo-node']">
      <xsl:apply-templates select="$info/title[@role='texinfo-node']"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    
    <xsl:when test="@xreflabel">
      <xsl:apply-templates select="@xreflabel"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./titleabbrev">
      <xsl:apply-templates select="./titleabbrev"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./title">
      <xsl:apply-templates select="./title"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="partintro" mode="texinfo.nodename.mode">
  <xsl:variable name="info" select="../docinfo" />
  
  <xsl:choose>
    <xsl:when test="$info/title[@role='texinfo-node']">
      <xsl:apply-templates select="$info/title[@role='texinfo-node']"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>

    <xsl:when test="./title">
      <xsl:apply-templates select="./title"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="./titleabbrev">
      <xsl:apply-templates select="./titleabbrev"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    
    <!-- Titles on partintro are optional.  Since it's not specified,
         check the parent part. -->
    <xsl:when test="../titleabbrev">
      <xsl:apply-templates select="../titleabbrev"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
    <xsl:when test="../title">
      <xsl:apply-templates select="../title"
                           mode="texinfo.nodename-string.mode" />
    </xsl:when>
  </xsl:choose>
</xsl:template>

<xsl:template match="varlistentry" mode="texinfo.nodename.mode">
  <xsl:apply-templates select="./term[1]"
                       mode="texinfo.nodename-string.mode" />
</xsl:template>

</xsl:stylesheet>

