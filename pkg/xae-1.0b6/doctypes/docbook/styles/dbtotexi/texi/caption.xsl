<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<!DOCTYPE xsl:stylesheet [
<!ENTITY lsquo "&#x2018;">
<!ENTITY rsquo "&#x2019;">
<!ENTITY ldquo "&#x201C;">
<!ENTITY rdquo "&#x201D;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: caption.xsl,v 1.6 2000/08/21 16:17:18 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<doc:template name="caption" xmlns="" xml:lang="en">
<refpurpose>Render as a &lsquo;caption&rsquo;</refpurpose>
<refdescription>
<para>
This template renders content (usually titles in certain block objects) 
as a &lsquo;caption&rsquo;.  What this exactly means depends on the value 
of the <varname>$caption.use.heading </varname> parameter.
</para>
</refdescription>
<refparameter>
<variablelist>
<varlistentry>
<term><parameter>content</parameter></term>
<listitem><para>
The result tree fragment to render.  If not specified, defaults to
applying <function>caption.title.mode</function> to the 
<emphasis>context</emphasis> node.  (It uses the context node because
the usual application of this template is from <sgmltag class="element">
title</sgmltag> element templates.
</para></listitem>
</varlistentry>
</variablelist>
</refparameter>
</doc:template>

<xsl:template name="caption">
  <xsl:param name="content">
    <xsl:apply-templates select="." mode="caption.title.mode" />
  </xsl:param>

  <xsl:choose>
    <xsl:when test="$caption.use.heading">
      <xsl:call-template name="texinfo.section">
        <xsl:with-param name="level">
          <xsl:call-template name="texinfo.section.level">
            <xsl:with-param name="heading.class" select="'chapheading'" />
          </xsl:call-template>
        </xsl:with-param>
        <xsl:with-param name="title" select="$content" />
      </xsl:call-template>
    </xsl:when>

    <xsl:otherwise>
      <para>
        <strong>
          <xsl:copy-of select="$content" />
        </strong>
      </para>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
    
<!-- ==================================================================== -->

<xsl:template match="title" mode="caption.title.mode">
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
