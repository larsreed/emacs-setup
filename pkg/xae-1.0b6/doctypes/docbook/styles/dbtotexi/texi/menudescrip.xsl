<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: menudescrip.xsl,v 1.1 2000/08/22 01:23:58 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->
<doc:mode mode="menu.description.mode">
<refpurpose>Return description for Texinfo node</refpurpose>
<refdescription>
<para>
Processing an element using menu.description.mode returns a result tree
fragment that is the description for its menu entry.
</para>
<para>
This mode is still experimental.
</para>
</refdescription>
</doc:mode>

<xsl:template match="refentry"
              mode="menu.description.mode">
  <xsl:apply-templates select="refnamediv/refpurpose/node()" />
</xsl:template>

<xsl:template match="reference|preface|chapter|appendix|glossary|bibliography|article|
                     sect1|sect2|sect3|sect4|sect5|refsect1|refsect2|refsect3|partintro"
              mode="menu.description.mode">
  <xsl:variable name="info"
    select="./docinfo|*[local-name(.)=concat(local-name(current()),'info')]" />
  <xsl:if test="$info/abstract">
    <xsl:apply-templates select="$info/abstract/node()"
                         mode="force.inline.mode" />
  </xsl:if>
</xsl:template>

</xsl:stylesheet>
