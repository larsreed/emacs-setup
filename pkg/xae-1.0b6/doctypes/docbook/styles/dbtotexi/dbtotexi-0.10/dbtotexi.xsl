<?xml version='1.0'?>

<!--
  ====================================================================
  XSL Stylesheet for converting DocBook XML to Texinfo using James
  Clark's Java XSL processor xt.

  $Id: dbtotexi.xsl,v 1.1 2000/07/06 16:51:21 steve Exp $

  ====================================================================
  Copyright & License.

  Copyright (C) 1999-2000 Mark Burton (markb@ordern.com)

  This file is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2, or (at your option)
  any later version.

  This file is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with GNU Emacs; see the file COPYING.  If not, write to
  the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

  ====================================================================
  Typical Usage

  To convert foo.xml into foo.texinfo:

  jre -cp .:xp.jar:xt.jar:sax.jar com.jclark.xsl.sax.Driver \
    foo.xml dbtotexi.xsl foo.texinfo infofilename=foo.info

  James Clark's sx program can be used to convert an SGML DocBook file
  into XML like this:

  sx -xlower foo.sgm > foo.xml
  
  ====================================================================
  -->

<!-- ==================================================================== -->
<!-- Top level -->

<xsl:stylesheet
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:dbtt="d:/xml/docbook2X/xslt/dbtotexi-0.10/DBToTexiSupport"
  extension-element-prefixes="dbtt"  version='1.0'>

<xsl:output method="text" encoding="ISO-8859-1"/>

<xsl:strip-space elements="row"/>

<xsl:param name="infofilename" select="undefined.info"/>

<xsl:variable name="list-mark-0">
  <xsl:text>@bullet</xsl:text>
</xsl:variable>

<xsl:variable name="list-mark-1">
  <xsl:text>+</xsl:text>
</xsl:variable>

<xsl:variable name="list-mark-2">
  <xsl:text>@minus</xsl:text>
</xsl:variable>

<xsl:template match="/">
  <xsl:text>\input texinfo&#10;</xsl:text>
  <xsl:text>@setfilename </xsl:text>
  <xsl:value-of select="$infofilename"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates select="processing-instruction('dircategory')" mode="live"/>
  <xsl:apply-templates select="processing-instruction('direntry')" mode="live"/>
  <xsl:text>@setchapternewpage odd&#10;</xsl:text>
  <xsl:text>@macro none&#10;@end macro&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Book -->

<xsl:template match="book">
  <xsl:if test="title|bookinfo/title|bookinfo/bookbiblio/title">
    <xsl:text>@settitle </xsl:text>
    <xsl:apply-templates select="(title|bookinfo/title|bookinfo/bookbiblio/title)[1]" mode="for-title"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:if>
  <xsl:apply-templates select="bookinfo" mode="for-title-page"/>
  <xsl:text>@node Top, , (dir), (dir)&#10;</xsl:text>
  <xsl:text>&#10;@menu&#10;</xsl:text>
  <xsl:apply-templates select="preface/title|chapter/title|reference/title|appendix/title" mode="make-menu"/>
  <xsl:text>@end menu&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@contents&#10;@bye&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Bookinfo -->

<xsl:template match="bookinfo" mode="for-title-page">
  <xsl:text>@titlepage&#10;</xsl:text>

  <xsl:text>@title </xsl:text>
  <xsl:apply-templates select="(../title|title|bookbiblio/title)[1]" mode="for-title"/>
  <xsl:if test="subtitle|bookbiblio/subtitle">
    <xsl:text>&#10;@subtitle </xsl:text>
    <xsl:apply-templates select="(subtitle|bookbiblio/subtitle)[1]" mode="for-title"/>
  </xsl:if>
  <xsl:if test="date">
    <xsl:text>&#10;@subtitle </xsl:text>
    <xsl:apply-templates select="date" mode="for-subtitle"/>
  </xsl:if>
  <xsl:text>&#10;@author </xsl:text>
  <xsl:apply-templates select="author" mode="for-title-page"/>
  <xsl:apply-templates select="authorgroup" mode="for-title-page"/>
  <xsl:apply-templates select="corpauthor" mode="for-title-page"/>
  <xsl:text>&#10;@page&#10;@vskip 0pt plus 1filll&#10;</xsl:text>
  <xsl:if test="copyright">
    <xsl:apply-templates select="copyright"/>
  </xsl:if>
  <!--<xsl:apply-templates/>-->
  <xsl:text>&#10;@end titlepage&#10; </xsl:text>
</xsl:template>

<xsl:template match="bookinfo/date" mode="for-subtitle">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="bookinfo/date">
  <!-- just ignore stuff we can't yet handle -->
</xsl:template>

<xsl:template match="copyright">
  <xsl:text>Copyright @copyright{} </xsl:text>
  <xsl:apply-templates select="year"/>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="holder"/>
  <xsl:text>@sp 2&#10;</xsl:text>
</xsl:template>

<xsl:template match="legalnotice">
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="author" mode="for-title-page">
  <xsl:if test="position() > 1">
    <xsl:text>, </xsl:text>
  </xsl:if>
  <xsl:apply-templates select="honrofic"/>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="firstname"/>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="surname"/>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select=".//email"/>
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="authorblurb/*"/>
</xsl:template>

<xsl:template match="authorgroup" mode="for-title-page">
  <xsl:apply-templates mode="for-title-page"/>
</xsl:template>

<xsl:template match="corpauthor" mode="for-title-page">
  <xsl:apply-templates mode="for-title-page"/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Article -->

<xsl:template match="article">
  <xsl:if test="title|artheader/title|articleinfo/title">
    <xsl:text>@settitle </xsl:text>
    <xsl:apply-templates select="(title|artheader/title|articleinfo/title)[1]" mode="for-title"/>
    <xsl:text>&#10;</xsl:text>
  </xsl:if>
  <xsl:text>@node Top, , (dir), (dir)&#10;</xsl:text>
  <xsl:choose>
    <xsl:when test="artheader|articleinfo"/>
    <xsl:otherwise>
      <xsl:text>@unnumbered </xsl:text>
      <xsl:apply-templates select="title" mode="for-title"/>
      <xsl:text>&#10;</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:apply-templates/>
  <xsl:text>&#10;@contents&#10;@bye&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Articleinfo (was Artheader) -->

<xsl:template match="artheader|articleinfo">
  <xsl:text>@titlepage&#10;</xsl:text>
  <xsl:text>@title </xsl:text>
  <xsl:apply-templates select="(../title|title)[1]" mode="for-title"/>
  <xsl:if test="subtitle">
    <xsl:text>&#10;@subtitle </xsl:text>
    <xsl:apply-templates select="subtitle" mode="for-title"/>
  </xsl:if>
  <xsl:if test="date">
    <xsl:text>&#10;@subtitle </xsl:text>
    <xsl:apply-templates select="date" mode="for-subtitle"/>
  </xsl:if>
  <xsl:text>&#10;@author </xsl:text>
  <xsl:apply-templates select="//author" mode="for-title-page"/>
  <xsl:apply-templates select="//corpauthor" mode="for-title-page"/>
  <xsl:text>&#10;@page&#10;@vskip 0pt plus 1filll&#10;</xsl:text>
  <xsl:apply-templates select="copyright"/>
  <!--  <xsl:apply-templates/> -->
  <xsl:text>&#10;@end titlepage&#10;</xsl:text>
  <xsl:text>@unnumbered </xsl:text>
  <xsl:apply-templates select="(../title|title)[1]" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Major divisions -->

<xsl:template match="preface">
  <xsl:call-template name="make-node">
    <xsl:with-param name="this" select="."/>
    <xsl:with-param name="prev" select="preceding-sibling::preface[1]"/>
    <xsl:with-param name="next" select="(following-sibling::preface[1]|following-sibling::chapter[1])[1]"/>
    <xsl:with-param name="up" select=".."/>
  </xsl:call-template>
  <xsl:text>&#10;@unnumbered </xsl:text>
  <xsl:choose>
    <xsl:when test="./title">
      <xsl:apply-templates select="title" mode="for-title"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Preface</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="chapter">
  <xsl:call-template name="make-node">
    <xsl:with-param name="this" select="."/>
    <xsl:with-param name="prev" select="(preceding-sibling::preface[1]|preceding-sibling::chapter[1])[last()]"/>
    <xsl:with-param name="next" select="(following-sibling::chapter[1]|following-sibling::reference[1]|following-sibling::appendix[1])[1]"/>
    <xsl:with-param name="up" select=".."/>
  </xsl:call-template>
  <xsl:text>&#10;@chapter </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="reference">
  <xsl:call-template name="make-node">
    <xsl:with-param name="this" select="."/>
    <xsl:with-param name="prev" select="(preceding-sibling::chapter[1]|preceding-sibling::reference[1])[last()]"/>
    <xsl:with-param name="next" select="(following-sibling::reference[1]|following-sibling::appendix[1])[1]"/>
    <xsl:with-param name="up" select=".."/>
  </xsl:call-template>
  <xsl:text>&#10;@chapter </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="appendix">
  <xsl:call-template name="make-node">
    <xsl:with-param name="this" select="."/>
    <xsl:with-param name="prev" select="(preceding-sibling::chapter[1]|preceding-sibling::reference[1]|preceding-sibling::appendix[1])[last()]"/>
    <xsl:with-param name="next" select="following-sibling::appendix[1]"/>
    <xsl:with-param name="up" select=".."/>
  </xsl:call-template>
  <xsl:text>&#10;@appendix </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Sectioning -->

<xsl:template match="partintro/sect1">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@section </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="sect1">
  <xsl:if test="not(preceding-sibling::sect1)">
    <xsl:text>&#10;@menu&#10;</xsl:text>
    <xsl:apply-templates select="../sect1/title|../appendix/title" mode="make-menu"/>
    <xsl:text>@end menu&#10;</xsl:text>
  </xsl:if>
  <xsl:call-template name="make-node">
    <xsl:with-param name="this" select="."/>
    <xsl:with-param name="prev" select="preceding-sibling::sect1[1]"/>
    <xsl:with-param name="next" select="following-sibling::sect1[1]"/>
    <xsl:with-param name="up" select=".."/>
  </xsl:call-template>
  <xsl:text>&#10;@section </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="sect2">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@subsection </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="sect3">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@subsubsection </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="sect4|formalpara|blockquote">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@subsubheading </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="para">
  <xsl:call-template name="make-anchor"/>
  <xsl:variable name="paracontent">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:text>&#10;</xsl:text>
  <xsl:value-of select="dbtt:trim(string($paracontent))"/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Abstracts -->

<xsl:template match="abstract">
  <xsl:if test="not(./title)">
    <xsl:choose>
      <xsl:when test="../sect2">
	<xsl:text>@subheading </xsl:text>
      </xsl:when>
      <xsl:when test="../sect3">
	<xsl:text>@subsubheading </xsl:text>
      </xsl:when>
      <xsl:otherwise>
	<xsl:text>@heading </xsl:text>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:text>Abstract&#10;</xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="abstract/title">
  <xsl:choose>
    <xsl:when test="../../sect2">
      <xsl:text>@subheading </xsl:text>
    </xsl:when>
    <xsl:when test="../../sect3">
      <xsl:text>@subsubheading </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>@heading </xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:apply-templates/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Titling -->

<xsl:template match="title|subtitle"/>

<xsl:template match="title|subtitle" mode="for-title">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="title" mode="for-node-name">
  <xsl:value-of select="normalize-space(string(.))"/>
</xsl:template>

<xsl:template match="title|refname" mode="make-menu">
  <xsl:variable name="content" select="normalize-space(string(.))"/>
  <xsl:text>* </xsl:text>
  <xsl:choose>
    <xsl:when test="../@id">
      <xsl:value-of select="dbtt:makeNodeName($content)"/>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="dbtt:makeNodeName(string(../@id))"/>
      <xsl:text>.&#10;</xsl:text>
    </xsl:when>
    <xsl:when test="dbtt:makeUniqueNodeName($content, string(generate-id(..))) = dbtt:makeNodeName($content)">
      <xsl:value-of select="dbtt:makeNodeName($content)"/>
      <xsl:text>::&#10;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="dbtt:makeNodeName($content)"/>
      <xsl:text>: </xsl:text>
      <xsl:value-of select="dbtt:makeUniqueNodeName($content, string(generate-id(..)))"/>
      <xsl:text>.&#10;</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="bridgehead[@renderas='sect1']">
  <xsl:text>&#10;@heading </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="bridgehead[@renderas='sect2']">
  <xsl:text>&#10;@subheading </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="bridgehead">
  <xsl:text>&#10;@subsubheading </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Footnotes -->

<xsl:template match="title//footnote">
  <xsl:value-of select="dbtt:report('Sorry, cannot handle footnote in title')"/>
</xsl:template>

<xsl:template match="thead/row/entry//footnote|tfoot/row/entry//footnote">
  <xsl:text>}@footnote{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}@b{</xsl:text>
</xsl:template>

<xsl:template match="footnote">
  <xsl:text>@footnote{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Font changers -->

<xsl:template match="acronym">
  <xsl:text>@acronym{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="emphasis">
  <xsl:text>@emph{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="firstterm|replaceable|guibutton|guilabel|guimenu|guimenuitem|guisubmenu">
  <xsl:text>@i{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="application|classname|command|computeroutput|constant|literal|type|varname">
  <xsl:text>@code{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="userinput">
  <xsl:text>@b{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="keycombo">
  <xsl:text>@kbd{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="keycap">
  <xsl:text>@key{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="filename|option">
  <xsl:text>@samp{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="quote">
  <xsl:text>"</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>"</xsl:text>
</xsl:template>

<xsl:template match="citetitle">
  <xsl:text>@cite{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="email">
  <xsl:text>@email{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- sgmltag -->

<xsl:template match="sgmltag[@class='starttag']">
  <xsl:text>@code{&lt;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&gt;}</xsl:text>
</xsl:template>

<xsl:template match="sgmltag[@class='endtag']">
  <xsl:text>@code{&lt;/</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&gt;}</xsl:text>
</xsl:template>

<xsl:template match="sgmltag[@class='attribute']">
  <xsl:text>@code{&amp;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>;}</xsl:text>
</xsl:template>

<xsl:template match="sgmltag">
  <xsl:text>@code{&lt;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&gt;}</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- funcsynopsis -->

<xsl:template match="funcsynopsis">
  <xsl:text>@code{</xsl:text>
  <xsl:apply-templates select="funcdef"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="funcdef">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="funcdef/function">
  <xsl:apply-templates/>
  <xsl:text>(</xsl:text>
  <xsl:apply-templates select="../../paramdef"/>
  <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="paramdef">
  <xsl:if test="position() > 1">
    <xsl:text>, </xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="void">
  <xsl:text>void</xsl:text>
</xsl:template>

<xsl:template match="parameter">
  <xsl:text>@var{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Links -->

<xsl:template match="xref">
  <xsl:text>@ref{</xsl:text>
  <xsl:value-of select="dbtt:makeNodeName(string(@linkend))"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="ulink">
  <xsl:text>@uref{</xsl:text>
  <xsl:value-of select="@url"/>
  <xsl:text>,</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="link">
  <!-- currently, we just pass content through unchanged and ignore link -->
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Tables -->

<xsl:template match="table">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@subheading </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="informaltable">
  <xsl:call-template name="make-anchor"/>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="tgroup">
  <xsl:text>&#10;@multitable @columnfractions </xsl:text>
  <xsl:variable name="colwidths">
    <xsl:apply-templates select="colspec" mode="get-colwidths"/>
  </xsl:variable>
  <xsl:value-of select="dbtt:columnFractions(number(@cols),string($colwidths))"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates select="thead"/>
  <xsl:apply-templates select="tbody"/>
  <xsl:apply-templates select="tfoot"/>
  <xsl:text>@end multitable&#10;</xsl:text>
</xsl:template>

<xsl:template match="colspec" mode="get-colwidths">
  <xsl:choose>
    <xsl:when test="@colwidth">
      <xsl:value-of select="string(@colwidth)"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>1*</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="thead|tbody|tfoot">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="row">
  <xsl:text>@item </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="thead/row/entry|tfoot/row/entry">
  <xsl:text>@b{</xsl:text>
  <xsl:variable name="entrycontent">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:value-of select="dbtt:trim(string($entrycontent))"/>
  <xsl:text>}</xsl:text>
  <xsl:if test="not(position() = last())">
    <xsl:text> @tab </xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template match="entry">
  <xsl:variable name="entrycontent">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:value-of select="dbtt:trim(string($entrycontent))"/>
  <xsl:if test="not(position() = last())">
    <xsl:text> @tab </xsl:text>
  </xsl:if>
</xsl:template>

<!-- ==================================================================== -->
<!-- Reference pages -->

<xsl:template match="citerefentry">
  <xsl:text>@b{</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="manvolnum">
  <xsl:text>(</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>)</xsl:text>
</xsl:template>

<xsl:template match="refname">
  <xsl:if test="position() > 1">
    <xsl:text>, </xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="refentry">
  <xsl:if test="not(preceding-sibling::refentry)">
    <xsl:text>&#10;@menu&#10;</xsl:text>
    <xsl:apply-templates select="../refentry/refnamediv/refname[1]" mode="make-menu"/>
    <xsl:text>@end menu&#10;</xsl:text>
  </xsl:if>
  <xsl:call-template name="make-node">
    <xsl:with-param name="this" select="."/>
    <xsl:with-param name="prev" select="preceding-sibling::refentry[1]"/>
    <xsl:with-param name="next" select="following-sibling::refentry[1]"/>
    <xsl:with-param name="up" select=".."/>
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="refpurpose|refentrytitle|reference/partintro|synopsis|synopsis/replaceable|synopsis/option">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="refnamediv">
  <xsl:text>&#10;@heading NAME&#10;</xsl:text>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:apply-templates select="refname"/>
  <xsl:text> @minus{} </xsl:text>
  <xsl:apply-templates select="refpurpose"/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="refsynopsisdiv">
  <xsl:text>&#10;@heading SYNOPSIS&#10;</xsl:text>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="refsect1">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@heading </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="refsect2">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@subheading </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="refsect3">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@subsubheading </xsl:text>
  <xsl:apply-templates select="title" mode="for-title"/>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Indexing -->

<xsl:template match="indexterm[@role]">
  <xsl:call-template name="indexentry">
    <xsl:with-param name="indextype" select="@role"/>
  </xsl:call-template>
</xsl:template>

<xsl:template match="indexterm">
  <xsl:call-template name="indexentry"/>
</xsl:template>

<xsl:template name="indexentry">  
  <xsl:param name="indextype" select="'c'"/>
  <xsl:text>&#10;@</xsl:text>
  <xsl:value-of select="$indextype"/>
    <xsl:text>index </xsl:text>
    <xsl:choose>
      <xsl:when test="./secondary">
	<xsl:apply-templates select="primary"/>
	<xsl:text> - </xsl:text>
	<xsl:apply-templates select="secondary"/>
      </xsl:when>
      <xsl:otherwise>
	<xsl:apply-templates select="primary"/>
      </xsl:otherwise>
    </xsl:choose>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="primary|secondary">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="index[@role='f']">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>@unnumbered </xsl:text>
  <xsl:choose>
    <xsl:when test="index/title">
      <xsl:apply-templates select="title"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Function Index</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;@printindex fn&#10;</xsl:text>
</xsl:template>

<xsl:template match="index[@role='k']">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>@unnumbered </xsl:text>
  <xsl:choose>
    <xsl:when test="index/title">
      <xsl:apply-templates select="title"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Keystroke Index</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;@printindex kp&#10;</xsl:text>
</xsl:template>

<xsl:template match="index[@role='p']">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>@unnumbered </xsl:text>
  <xsl:choose>
    <xsl:when test="index/title">
      <xsl:apply-templates select="title"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Program Index</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;@printindex pg&#10;</xsl:text>
</xsl:template>

<xsl:template match="index[@role='t']">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>@unnumbered </xsl:text>
  <xsl:choose>
    <xsl:when test="index/title">
      <xsl:apply-templates select="title"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Data Type Index</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;@printindex tp&#10;</xsl:text>
</xsl:template>

<xsl:template match="index[@role='v']">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>@unnumbered </xsl:text>
  <xsl:choose>
    <xsl:when test="index/title">
      <xsl:apply-templates select="title"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Variable Index</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;@printindex vr&#10;</xsl:text>
</xsl:template>

<xsl:template match="index">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>@unnumbered </xsl:text>
  <xsl:choose>
    <xsl:when test="index/title">
      <xsl:apply-templates select="title"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>Concept Index</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;@printindex cp&#10;</xsl:text>
</xsl:template>

<xsl:template match="index/title">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Lists -->

<xsl:template match="itemizedlist">
  <xsl:text>&#10;&#10;@itemize </xsl:text>
  <xsl:variable name="depth" select="count(ancestor::itemizedlist) mod 3"/>
  <xsl:choose>
    <xsl:when test="@mark='none'">
      <xsl:text>@none</xsl:text>
    </xsl:when>
    <xsl:when test="@mark='bullet'">
      <xsl:text>@bullet</xsl:text>
    </xsl:when>
    <xsl:when test="@mark">
      <xsl:value-of select="@mark"/>
    </xsl:when>
    <xsl:when test='$depth=2'>
      <xsl:value-of select="$list-mark-2"/>
    </xsl:when>
    <xsl:when test='$depth=1'>
      <xsl:value-of select="$list-mark-1"/>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$list-mark-0"/>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end itemize&#10;</xsl:text>
</xsl:template>

<xsl:template match="orderedlist">
  <xsl:text>&#10;&#10;@enumerate&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end enumerate&#10;</xsl:text>
</xsl:template>

<xsl:template match="orderedlist[@numeration='upperalpha']">
  <xsl:text>&#10;&#10;@enumerate A&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end enumerate&#10;</xsl:text>
</xsl:template>

<xsl:template match="orderedlist[@numeration='loweralpha']">
  <xsl:text>&#10;&#10;@enumerate a</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end enumerate&#10;</xsl:text>
</xsl:template>

<xsl:template match="itemizedlist/listitem|orderedlist/listitem">
  <xsl:text>&#10;@item</xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="variablelist">
  <xsl:text>&#10;&#10;@table </xsl:text>
  <xsl:choose>
    <xsl:when test="@role='bold'">
      <xsl:text>@strong</xsl:text>
    </xsl:when>
    <xsl:when test="@role='fixed'">
      <xsl:text>@code</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>@asis</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:text>&#10;</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end table&#10;</xsl:text>
</xsl:template>

<xsl:template match="varlistentry">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="varlistentry/term">
  <xsl:text>&#10;@item </xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="varlistentry/term[position() > 1]">
  <xsl:text>&#10;@itemx </xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="varlistentry/listitem">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Figure like things -->

<xsl:template match="figure|example">
  <xsl:call-template name="make-anchor"/>
  <!--  <xsl:text>&#10;@quotation&#10;</xsl:text> -->
  <xsl:apply-templates/>
  <!-- <xsl:text>&#10;@end quotation&#10;</xsl:text> -->
</xsl:template>

<xsl:template match="figure/title|example/title">
  <xsl:text>@subsubheading </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>
</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Admonishments -->

<xsl:template match="caution">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:if test="not(./title)">
    <xsl:text>@subsubheading Caution&#10;</xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="important">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:if test="not(./title)">
    <xsl:text>@subsubheading Important&#10;</xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="note">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:if test="not(./title)">
    <xsl:text>@subsubheading Note&#10;</xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="tip">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:if test="not(./title)">
    <xsl:text>@subsubheading Tip&#10;</xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="warning">
  <xsl:call-template name="make-anchor"/>
  <xsl:text>&#10;@quotation&#10;</xsl:text>
  <xsl:if test="not(./title)">
    <xsl:text>@subsubheading Warning&#10;</xsl:text>
  </xsl:if>
  <xsl:apply-templates/>
  <xsl:text>&#10;@end quotation&#10;</xsl:text>
</xsl:template>

<xsl:template match="caution/title|important/title|note/title|tip/title|warning/title">
  <xsl:text>@subsubheading </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Graphics -->

<xsl:template match="graphic[@fileref]|inlinegraphic[@fileref]">
  <xsl:text>@image{</xsl:text>
  <xsl:value-of select="@fileref"/>
  <xsl:text>,</xsl:text>
  <xsl:value-of select="@width"/>
  <xsl:text>,</xsl:text>
  <xsl:value-of select="@depth"/>
  <xsl:text>}</xsl:text>
</xsl:template>

<!--
<xsl:template match="graphic[@entityref]|inlinegraphic[@entityref]">
  <xsl:text>@image{</xsl:text>
  <xsl:value-of select="@entityref"/>
  <xsl:text>,,}</xsl:text>
</xsl:template>
-->

<xsl:template match="graphic">
  <xsl:value-of select="dbtt:report('Ignoring graphic element because it does not have a fileref')"/>
</xsl:template>

<xsl:template match="screenshot">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="screeninfo">
  <!-- relax -->
</xsl:template>

<!-- ==================================================================== -->
<!-- Literal Sections -->

<xsl:template match="screen|programlisting|literallayout">
  <xsl:call-template name="make-anchor"/>
  <xsl:variable name="content">
    <xsl:apply-templates/>
  </xsl:variable>
  <xsl:text>&#10;&#10;@example&#10;</xsl:text>
  <xsl:value-of select="dbtt:trim(string($content))"/>
  <xsl:text>&#10;@end example&#10;&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Text handling -->

<xsl:template match="screen/text()|programlisting/text()|literallayout/text()">
  <xsl:value-of select="dbtt:expandCharacters(string(.))"/>
</xsl:template>

<xsl:template match="text()">
  <xsl:value-of select="dbtt:squeezeSpacesAndExpandCharacters(string(.))"/>
</xsl:template>

<!-- ==================================================================== -->
<!-- Processing instructions -->

<xsl:template match="processing-instruction('texinfo')">
  <xsl:value-of select="string(.)"/>
</xsl:template>

<xsl:template match="processing-instruction('dircategory')" mode="live">
  <xsl:text>@dircategory </xsl:text>
  <xsl:value-of select="normalize-space(string(.))"/>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="processing-instruction('direntry')" mode="live">
  <xsl:text>@direntry&#10;</xsl:text>
  <xsl:value-of select="normalize-space(string(.))"/>
  <xsl:text>&#10;@end direntry&#10;</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- Miscellaneous templates -->

<xsl:template match="*">
  <!-- warn user that the element is not yet handled -->
  <xsl:value-of select="dbtt:report(concat('Sorry, cannot yet handle ', name(.), ' element'))"/>
  <xsl:text>@strong{&lt;</xsl:text>
  <xsl:value-of select="name(.)"/>
  <xsl:text>&gt;}</xsl:text>
  <xsl:apply-templates/> 
  <xsl:text>@strong{&lt;/</xsl:text>
  <xsl:value-of select="name(.)"/>
  <xsl:text>&gt;}</xsl:text>
</xsl:template>

<xsl:template name="make-anchor">
  <xsl:if test="@id">
    <xsl:text>&#10;@anchor{</xsl:text>
    <xsl:value-of select="dbtt:makeNodeName(string(@id))"/>
    <xsl:text>}&#10;</xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template name="make-node">
  <xsl:param name="this" select="/"/>
  <xsl:param name="next" select="/"/> 
  <xsl:param name="prev" select="/"/>
  <xsl:param name="up" select="/"/>
  <xsl:text>&#10;@node </xsl:text>
  <xsl:choose>
    <xsl:when test="$this/@id">
      <xsl:value-of select="dbtt:makeNodeName(string($this/@id))"/>
    </xsl:when>
    <xsl:when test="$this/title">
      <xsl:variable name="foo">
	<xsl:apply-templates select="$this/title" mode="for-node-name"/>
      </xsl:variable>
      <xsl:value-of select="dbtt:makeUniqueNodeName(string($foo), string(generate-id($this)))"/>
    </xsl:when>
    <xsl:when test="$this/refnamediv/refname">
      <xsl:variable name="foo">
	<xsl:apply-templates select="$this/refnamediv/refname" mode="for-node-name"/>
      </xsl:variable>
      <xsl:value-of select="dbtt:makeUniqueNodeName(string($foo), string(generate-id($this)))"/>
    </xsl:when>
  </xsl:choose>
  <xsl:text>, </xsl:text>
  <xsl:choose>
    <xsl:when test="$next/@id">
      <xsl:value-of select="dbtt:makeNodeName(string($next/@id))"/>
    </xsl:when>
    <xsl:when test="$next/title">
      <xsl:variable name="foo">
	<xsl:apply-templates select="$next/title" mode="for-node-name"/>
      </xsl:variable>
      <xsl:value-of select="dbtt:makeUniqueNodeName(string($foo), string(generate-id($next)))"/>
    </xsl:when>
    <xsl:when test="$next/refnamediv/refname">
      <xsl:variable name="foo">
	<xsl:apply-templates select="$next/refnamediv/refname" mode="for-node-name"/>
      </xsl:variable>
      <xsl:value-of select="dbtt:makeUniqueNodeName(string($foo), string(generate-id($next)))"/>
    </xsl:when>
  </xsl:choose>
  <xsl:text>, </xsl:text>
  <xsl:choose>
    <xsl:when test="$prev/@id">
      <xsl:value-of select="dbtt:makeNodeName(string($prev/@id))"/>
    </xsl:when>
    <xsl:when test="$prev/title">
      <xsl:variable name="foo">
	<xsl:apply-templates select="$prev/title" mode="for-node-name"/>
      </xsl:variable>
      <xsl:value-of select="dbtt:makeUniqueNodeName(string($foo), string(generate-id($prev)))"/>
    </xsl:when>
    <xsl:when test="$prev/refnamediv/refname">
      <xsl:variable name="foo">
	<xsl:apply-templates select="$prev/refnamediv/refname" mode="for-node-name"/>
      </xsl:variable>
      <xsl:value-of select="dbtt:makeUniqueNodeName(string($foo), string(generate-id($prev)))"/>
    </xsl:when>
  </xsl:choose>
  <xsl:text>, </xsl:text>
  <xsl:choose>
    <xsl:when test="name($up) = 'book' or name($up) = 'article'">
      <xsl:text>Top</xsl:text>
    </xsl:when>
    <xsl:when test="$up/@id">
      <xsl:value-of select="dbtt:makeNodeName(string($up/@id))"/>
    </xsl:when>
    <xsl:when test="$up/title">
      <xsl:variable name="foo">
	<xsl:apply-templates select="$up/title" mode="for-node-name"/>
      </xsl:variable>
      <xsl:value-of select="dbtt:makeUniqueNodeName(string($foo), string(generate-id($up)))"/>
    </xsl:when>
  </xsl:choose>
  <xsl:text>&#10;</xsl:text>
</xsl:template>

<xsl:template match="abbrev|firstname|foreignphrase|holder|productname|surname|year|prompt">
  <!-- pass content through unchanged -->
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="bookbiblio|comment|toc|docinfo|colspec|bookinfo">
  <!-- throw away content -->
</xsl:template>

<xsl:template match="superscript">
  <!--
  <xsl:text>&#10;@tex&#10;\nobreak\raise .5ex \hbox{\sevenrm </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}&#10;@end tex</xsl:text>
  <xsl:text>&#10;@ifinfo&#10;[superscript </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>]&#10;@end ifinfo&#10;</xsl:text>
-->
  <xsl:text>@math{^</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="subscript">
  <!--
  <xsl:text>&#10;@tex&#10;\nobreak\lower .5ex \hbox{\sevenrm </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}&#10;@end tex</xsl:text>
  <xsl:text>&#10;@ifinfo&#10;[subscript </xsl:text>
  <xsl:apply-templates/>
  <xsl:text>]&#10;@end ifinfo&#10;</xsl:text>
-->
  <xsl:text>@math{_</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>}</xsl:text>
</xsl:template>

</xsl:stylesheet>

