<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: force-inline.xsl,v 1.2 2000/08/21 20:19:55 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<doc:mode mode="force.inline.mode" xmlns="">
<refpurpose>Process only inline content</refpurpose>
<refdescription>
<para>A few elements in DocBook, namely <sgmltag class="element">entry</sgmltag>
and <sgmltag class="element">footnote</sgmltag>, have content models
that allow block-level content (as well as inline-level content), but
the mapping to Texinfo does not allow block-level content.
This mode selects only the inline content and loudly complains to the
user if block-level elements are found.
</para>
<para>
Additionally, simple paragraphs are converted to inline content.
</para>
</refdescription>
</doc:mode>

<xsl:template 
  match="calloutlist|glosslist|itemizedlist|orderedlist|segmentedlist|simplelist|variablelist|caution|important|note|tip|warning|literallayout|programlisting|programlistingco|screen|screenco|screenshot|synopsis|cmdsynopsis|funcsynopsis|formalpara|address|blockquote|graphic|graphicco|mediaobject|mediaobjectco|informalequation|informalexample|informalfigure|informaltable|equation|example|figure|table|msgset|procedure|qandaset"
  mode="force.inline.mode">
  <xsl:call-template name="user.message">
    <xsl:with-param name="key">Non-inline content not supported</xsl:with-param>
  </xsl:call-template>
  <xsl:call-template name="gentext.char">
    <xsl:with-param name="key" select="'block.object.placeholder'" />
  </xsl:call-template>
</xsl:template>

<xsl:template match="para|simpara" mode="force.inline.mode">
  <xsl:if test="preceding-sibling::para | preceding-sibling::simpara">
    <xsl:call-template name="user.message">
      <xsl:with-param name="key">Non-inline content not supported</xsl:with-param>
    </xsl:call-template>
    <xsl:call-template name="gentext.char">
      <xsl:with-param name="key" select="'block.object.placeholder'" />
    </xsl:call-template>
  </xsl:if>
  <xsl:apply-templates mode="force.inline.mode" />
</xsl:template>

<xsl:template match="*|text()" mode="force.inline.mode">
  <xsl:apply-templates select="." />
</xsl:template>
 
</xsl:stylesheet>

