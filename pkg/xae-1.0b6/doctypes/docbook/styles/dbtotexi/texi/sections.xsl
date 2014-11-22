<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: sections.xsl,v 1.8 2000/08/21 16:17:18 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<doc:template name="texinfo.section.level" xmlns="">
<refpurpose>Give Texinfo structuring command for given section
level</refpurpose>
<refdescription>
<para>
This template returns the name of the Texinfo command that is mapped to
the given DocBook element.
</para>
<para>
This implementation internally uses numbers that reflect the DocBook
hierarchy, like Norm's <function>section.level</function> template.
However, <function>section.level</function> only maps the DocBook 
sectioning elements (<sgmltag>sect<replaceable>n</replaceable></sgmltag>
and <sgmltag>section</sgmltag>).
</para>
<para>
This simplistic mapping of numbers to Texinfo commands sometimes yield
results that do not make a lot of sense; e.g. if the document element is
section, then the first section is mapped to <markup>@top</markup>, but
the next nested section would be a Texinfo 'chapter'.  I have thought about
letting each template specify their own structuring element, but from my
testing, jumping around the hierarchy breaks at least the TeX portion of
Texinfo.  Besides, this neccessitated writing a 
<sgmltag>xsl:choose</sgmltag>s with non-trivial cases in many templates.
</para>
<para>
The only thing I can say is that if you use 'weird' document elements,
you can't reasonably transform to Texinfo (whose top level is
semantically similar to <sgmltag>book</sgmltag>).  
<command>docbook2texixml</command> takes the same approach.
</para>
<para>
I also have a suspicion that calling this template will be slow, but 
there's nothing we can do about it :-(
</para>
</refdescription>
<refparameter>
<variablelist>
  <varlistentry>
    <term><parameter>node</parameter></term>
    <listitem><para>
      The node to get the Texinfo structuring command mapping for.
      Default is the context node.
    </para></listitem>
  </varlistentry>
  <varlistentry>
    <term><parameter>heading.class</parameter></term>
    <listitem><para>
      The class of heading commands to use:
      <simplelist type="inline">
        <member><literal>chapter</literal></member>
        <member><literal>unnumbered</literal></member>
        <member><literal>appendix</literal></member>
        <member><literal>chapheading</literal></member>
      </simplelist>
      See the chart in the Texinfo manual for the meanings of each class.
      The default is <literal>chapter</literal>
    </para></listitem>
  </varlistentry>
</variablelist>
</refparameter>
</doc:template>

<xsl:template name="texinfo.section.level">
  <xsl:param name="node" select="." />
  <xsl:param name="heading.class" select="'chapter'" />

  <xsl:variable name="count">
    <!-- part special case is needed because components which
         are children of part should be at the same level as
         components that are not enclosed in a part. -->
    <xsl:choose>
      <xsl:when test="/part">
        <xsl:value-of select="count($node/ancestor::*)" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of 
          select="count($node/ancestor::*[not(self::set | self::part)])" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <xsl:choose>
    <xsl:when test="$heading.class = 'chapter'">
      <xsl:choose>
        <xsl:when test="$count = 0">top</xsl:when>
        <xsl:when test="$count = 1">chapter</xsl:when>
        <xsl:when test="$count = 2">section</xsl:when>
        <xsl:when test="$count = 3">subsection</xsl:when>
        <xsl:when test="$count = 4">subsubsection</xsl:when>
        <!-- Note: we use subsubheading so that it won't screw up the TOC -->
        <xsl:otherwise><xsl:text>subsubheading</xsl:text>
          <xsl:call-template name="user.message">
            <xsl:with-param name="key">Section is too deep</xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="$heading.class = 'unnumbered'">
      <xsl:choose>
        <xsl:when test="$count = 0">top</xsl:when>
        <xsl:when test="$count = 1">unnumbered</xsl:when>
        <xsl:when test="$count = 2">unnumberedsec</xsl:when>
        <xsl:when test="$count = 3">unnumberedsubsec</xsl:when>
        <xsl:when test="$count = 4">unnumberedsubsubsec</xsl:when>
        <xsl:otherwise><xsl:text>subsubheading</xsl:text>
          <xsl:call-template name="user.message">
            <xsl:with-param name="key">Section is too deep</xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="$heading.class = 'appendix'">
      <xsl:choose>
        <xsl:when test="$count = 0">top</xsl:when>
        <xsl:when test="$count = 1">appendix</xsl:when>
        <xsl:when test="$count = 2">appendixsec</xsl:when>
        <xsl:when test="$count = 3">appendixsubsec</xsl:when>
        <xsl:when test="$count = 4">appendixsubsubsec</xsl:when>
        <xsl:otherwise><xsl:text>subsubheading</xsl:text>
          <xsl:call-template name="user.message">
            <xsl:with-param name="key">Section is too deep</xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
    <xsl:when test="$heading.class = 'chapheading'">
      <xsl:choose>
        <xsl:when test="$count = 0">majorheading</xsl:when>
        <xsl:when test="$count = 1">chapheading</xsl:when>
        <xsl:when test="$count = 2">heading</xsl:when>
        <xsl:when test="$count = 3">subheading</xsl:when>
        <xsl:when test="$count = 4">subsubheading</xsl:when>
        <xsl:otherwise><xsl:text>subsubheading</xsl:text>
          <xsl:call-template name="user.message">
            <xsl:with-param name="key">Section is too deep</xsl:with-param>
          </xsl:call-template>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>
  </xsl:choose>
</xsl:template>


<!-- ==================================================================== -->

<doc:template name="texinfo.section" xmlns="">
<refpurpose>Standard template for Texinfo section structures</refpurpose>
<refdescription>
<para>
This is the standard template for the corresponding Texinfo sectioning
command for DocBook sectioning elements.
</para>
</refdescription>
<refparameter>
<variablelist>
<varlistentry>
<term><parameter>level</parameter></term>
<listitem><para>
The name of the Texinfo structuring command.  If not specified,
automatically uses <function>texinfo.section.level</function>.
</para></listitem>
</varlistentry>
<varlistentry>
<term><parameter>title</parameter></term>
<listitem><para>
The title to use for the section.  The default is found by applying
<function>title.content</function> mode templates to the context node.
</para></listitem>
</varlistentry>
</variablelist>
</refparameter>
</doc:template>

<xsl:template name="texinfo.section">
  <xsl:param name="level">
    <xsl:call-template name="texinfo.section.level" />
  </xsl:param>
  <xsl:param name="title">
    <xsl:apply-templates select="." mode="title.content" />
  </xsl:param>
 
  <xsl:element name="{$level}">
    <xsl:copy-of select="$title" />
  </xsl:element>

  <!-- FIXME: @documentlanguage hack -->
  <xsl:if test="@lang">
    <documentlanguage>
      <xsl:attribute name="lang">
        <xsl:value-of select="@lang" />
      </xsl:attribute>
    </documentlanguage>
  </xsl:if>
  
</xsl:template>


<!-- ==================================================================== -->

<xsl:template match="sect1|sect2|sect3|sect4|sect5|simplesect|section">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>
      
<!-- ==================================================================== -->

<xsl:template match="title">
  <!-- ignore -->
</xsl:template>
<xsl:template match="titleabbrev">
  <!-- ignore -->
</xsl:template>
<xsl:template match="subtitle">
  <!-- ignore -->
</xsl:template>

</xsl:stylesheet>

