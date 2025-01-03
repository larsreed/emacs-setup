<?xml version='1.0'?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
		exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: param.xsl,v 1.26 2000/08/29 16:12:18 nwalsh Exp $
     ********************************************************************

     This file is part of the XSL DocBook Stylesheet distribution.
     See ../README or http://nwalsh.com/docbook/xsl/ for copyright
     and other information.

     ******************************************************************** -->

<!-- ==================================================================== -->
<xsl:param name="author.othername.in.middle" select="1"/>

<doc:param name="author.othername.in.middle" xmlns="">
<refpurpose>Is <sgmltag>othername</sgmltag> in <sgmltag>author</sgmltag> a
middle name?</refpurpose>
<refdescription>
<para>If true (non-zero), the <sgmltag>othername</sgmltag> of an <sgmltag>author</sgmltag>
appears between the <sgmltag>firstname</sgmltag> and
<sgmltag>surname</sgmltag>.  Otherwise, <sgmltag>othername</sgmltag>
is suppressed.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="html.stylesheet"></xsl:param>

<doc:param name="html.stylesheet" xmlns="">
<refpurpose>Name of the stylesheet to use in the generated HTML</refpurpose>
<refdescription>
<para>The name of the stylesheet to place in the HTML <sgmltag>LINK</sgmltag>
tag, or the empty string to suppress the stylesheet <sgmltag>LINK</sgmltag>.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="html.stylesheet.type">text/css</xsl:param>

<doc:param name="html.stylesheet.type" xmlns="">
<refpurpose>The type of the stylesheet used in the generated HTML</refpurpose>
<refdescription>
<para>The type of the stylesheet to place in the HTML <sgmltag>link</sgmltag> tag.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="html.base"></xsl:param>

<doc:param name="html.base" xmlns="">
<refpurpose>An HTML base URI</refpurpose>
<refdescription>
<para>If html.base is set, it is used for the <sgmltag>BASE</sgmltag>
element in the <sgmltag>HEAD</sgmltag> of the HTML documents.
This is useful for dynamically served HTML where the base URI needs
to be shifted.</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="refentry.xref.manvolnum" select="1"/>

<doc:param name="refentry.xref.manvolnum" xmlns="">
<refpurpose>Output <sgmltag>manvolnum</sgmltag> as part of 
<sgmltag>refentry</sgmltag> cross-reference?</refpurpose>
<refdescription>
<para>if true (non-zero), the <sgmltag>manvolnum</sgmltag> is used when cross-referencing
<sgmltag>refentry</sgmltag>s, either with <sgmltag>xref</sgmltag>
or <sgmltag>citerefentry</sgmltag>.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="show.comments">1</xsl:param>

<doc:param name="show.comments" xmlns="">
<refpurpose>Display <sgmltag>comment</sgmltag> elements?</refpurpose>
<refdescription>
<para>If true (non-zero), comments will be displayed, otherwise they are suppressed.
Comments here refers to the <sgmltag>comment</sgmltag> element,
which will be renamed <sgmltag>remark</sgmltag> in DocBook V4.0,
not XML comments (&lt;-- like this --&gt;) which are unavailable.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="funcsynopsis.style">kr</xsl:param>

<doc:param name="funcsynopsis.style" xmlns="">
<refpurpose>What style of 'FuncSynopsis' should be generated?</refpurpose>
<refdescription>
<para>If <varname>funcsynopsis.style</varname> is <literal>ansi</literal>,
ANSI-style function synopses are generated for a
<sgmltag>funcsynopsis</sgmltag>, otherwise K&amp;R-style
function synopses are generated.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="funcsynopsis.decoration" select="1"/>

<doc:param name="funcsynopsis.decoration" xmlns="">
<refpurpose>Decorate elements of a FuncSynopsis?</refpurpose>
<refdescription>
<para>If true (non-zero), elements of the FuncSynopsis will be decorated (e.g. bold or
italic).  The decoration is controlled by functions that can be redefined
in a customization layer.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="function.parens">0</xsl:param>

<doc:param name="function.parens" xmlns="">
<refpurpose>Generate parens after a function?</refpurpose>
<refdescription>
<para>If not 0, the formatting of
a <sgmltag class="starttag">function</sgmltag> element will include
generated parenthesis.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="refentry.generate.name" select="1"/>

<doc:param name="refentry.generate.name" xmlns="">
<refpurpose>Output NAME header before 'RefName'(s)?</refpurpose>
<refdescription>
<para>If true (non-zero), a "NAME" section title is output before the list
of 'RefName's.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="admon.graphics" select="0"/>

<doc:param name="admon.graphics" xmlns="">
<refpurpose>Use graphics in admonitions?</refpurpose>
<refdescription>
<para>If true (non-zero), admonitions are presented in an alternate style that uses
a graphic.  Default graphics are provided in the distribution.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="admon.graphics.path">../images/</xsl:param>

<doc:param name="admon.graphics.path" xmlns="">
<refpurpose>Path to admonition graphics</refpurpose>
<refdescription>
<para>Sets the path, probably relative to the directory where the HTML
files are created, to the admonition graphics.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="admon.style">
  <xsl:text>margin-left: 0.5in; margin-right: 0.5in;</xsl:text>
</xsl:param>

<doc:param name="admon.style" xmlns="">
<refpurpose>CSS style attributes for admonitions</refpurpose>
<refdescription>
<para>Specifies the value of the <sgmltag class="attribute">STYLE</sgmltag>
attribute that should be added to admonitions.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="section.autolabel" select="0"/>

<doc:param name="section.autolabel" xmlns="">
<refpurpose>Are sections enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled sections will be enumerated.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="section.label.includes.component.label" select="0"/>

<doc:param name="section.label.includes.component.label" xmlns="">
<refpurpose>Do section labels include the component label?</refpurpose>
<refdescription>
<para>If true (non-zero), section labels are prefixed with the label of the
component that contains them.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="chapter.autolabel" select="1"/>

<doc:param name="chapter.autolabel" xmlns="">
<refpurpose>Are chapters and appendixes enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled chapters and appendixes will be enumerated.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="preface.autolabel" select="0"/>

<doc:param name="preface.autolabel" xmlns="">
<refpurpose>Are prefaces enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled prefaces will be enumerated.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="part.autolabel" select="1"/>

<doc:param name="part.autolabel" xmlns="">
<refpurpose>Are parts and references enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled parts and references will be enumerated.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="qandadiv.autolabel" select="1"/>

<doc:param name="qandadiv.autolabel" xmlns="">
<refpurpose>Are divisions in QAndASets enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled qandadivs will be enumerated.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="qanda.inherit.numeration" select="1"/>

<doc:param name="qanda.inherit.numeration" xmlns="">
<refpurpose>Does enumeration of QandASet components inherit the numeration of parent elements?</refpurpose>
<refdescription>
<para>If true (non-zero), numbered QandADiv elements and Questions and Answers inherit
the numeration of the ancestors of the QandASet.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="qanda.defaultlabel">number</xsl:param>

<doc:param name="qanda.defaultlabel" xmlns="">
<refpurpose>Sets the default for defaultlabel on QandASet.</refpurpose>
<refdescription>
<para>If no defaultlabel attribute is specified on a QandASet, this
value is used. It must be one of the legal values for the defaultlabel
attribute.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="generate.qandaset.toc">1</xsl:param>

<doc:param name="generate.qandaset.toc" xmlns="">
<refpurpose>Is a Table of Contents created for QandASets?</refpurpose>
<refdescription>
<para>If true (non-zero), a ToC is constructed for QandASets.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="generate.qandadiv.toc">0</xsl:param>

<doc:param name="generate.qandadiv.toc" xmlns="">
<refpurpose>Is a Table of Contents created for QandADivs?</refpurpose>
<refdescription>
<para>If true (non-zero), a ToC is constructed for QandADivs.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="biblioentry.item.separator">. </xsl:param>

<doc:param name="biblioentry.item.separator" xmlns="">
<refpurpose>Text to separate bibliography entries</refpurpose>
<refdescription>
<para>Text to separate bibliography entries
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="toc.section.depth">2</xsl:param>

<doc:param name="toc.section.depth" xmlns="">
<refpurpose>How deep should recursive <sgmltag>section</sgmltag>s appear
in the TOC?</refpurpose>
<refdescription>
<para>Specifies the depth to which recursive sections should appear in the
TOC.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="using.chunker" select="0"/>

<doc:param name="using.chunker" xmlns="">
<refpurpose>Will the output be chunked?</refpurpose>
<refdescription>
<para>In addition to providing chunking, the chunker can cleanup a
number of XML to HTML issues. If the chunker is not being used, the
stylesheets try to avoid producing results that will not appear properly
in browsers.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="generate.component.toc" select="1"/>

<doc:param name="generate.component.toc" xmlns="">
<refpurpose>Should TOCs be genereated in components (Chapters, Appendixes, etc.)?</refpurpose>
<refdescription>
<para>If true (non-zero), they are.
</para>
</refdescription>
</doc:param>
<!-- ==================================================================== -->
<xsl:param name="generate.division.toc" select="1"/>

<doc:param name="generate.division.toc" xmlns="">
<refpurpose>Should TOCs be genereated in divisions (Books, Parts, etc.)?</refpurpose>
<refdescription>
<para>If true (non-zero), they are.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="link.mailto.url"></xsl:param>

<doc:param name="link.mailto.url" xmlns="">
<refpurpose>Mailto URL for the LINK REL=made HTML HEAD element</refpurpose>
<refdescription>
<para>If not the empty string, this address will be used for the
REL=made <sgmltag>LINK</sgmltag> element in the HTML <sgmltag>HEAD</sgmltag>.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="graphic.default.extension"></xsl:param>

<doc:param name="graphic.default.extension" xmlns="">
<refpurpose>Default extension for graphic filenames</refpurpose>
<refdescription>
<para>If a <sgmltag>graphic</sgmltag> or <sgmltag>mediaobject</sgmltag>
includes a reference to a filename that does not include an extension,
and the <sgmltag class="attribute">format</sgmltag> attribute is
<emphasis>unspecified</emphasis>, the default extension will be used.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="toc.list.type">dl</xsl:param>

<doc:param name="toc.list.type" xmlns="">
<refpurpose>Type of HTML list element to use for Tables of Contents</refpurpose>
<refdescription>
<para>When an automatically generated Table of Contents (or List of Titles)
is produced, this HTML element will be used to make the list.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="check.idref">1</xsl:param>

<doc:param name="check.idref" xmlns="">
<refpurpose>Test the target of IDREF attributes?</refpurpose>
<refdescription>
<para>If 1, the target of IDREF attributes are tested for presence
(and uniqueness). This can be very expensive in large documents.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->

<xsl:param name="use.id.function">1</xsl:param>

<doc:param name="use.id.function" xmlns="">
<refpurpose>Use the XPath id() function to find link targets?</refpurpose>
<refdescription>
<para>If 1, the stylesheets use the <function>id()</function> function
to find the targets of cross reference elements. This is more
efficient, but only works if your XSLT processor implements the
<function>id()</function> function, naturally.</para>
<para>THIS PARAMETER IS NOT SUPPORTED. IT IS ALWAYS ASSUMED TO BE 1.
SEE xref.xsl IF YOU NEED TO TURN IT OFF.</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:param name="spacing.paras">1</xsl:param>

<doc:param name="spacing.paras" xmlns="">
<refpurpose>Insert additional &lt;p&gt; elements for spacing?</refpurpose>
<refdescription>
<para>When non-zero, additional, empty paragraphs are inserted in
several contexts (for example, around informal figures), to create a
more pleasing visual appearance in many browsers.
</para>
</refdescription>
</doc:param>

<!-- ==================================================================== -->
<xsl:attribute-set name="body.attrs">
  <xsl:attribute name="bgcolor">white</xsl:attribute>
  <xsl:attribute name="text">black</xsl:attribute>
  <xsl:attribute name="link">#0000FF</xsl:attribute>
  <xsl:attribute name="vlink">#840084</xsl:attribute>
  <xsl:attribute name="alink">#0000FF</xsl:attribute>
</xsl:attribute-set>

<doc:attribute-set name="body.attrs" xmlns="">
<refpurpose>Additional attributes for the HTML body tag</refpurpose>
<refdescription>
<para>The attributes defined by this attribute set are added to the
HTML &lt;body&gt; tag.
</para>
</refdescription>
</doc:attribute-set>

<!-- ==================================================================== -->
<xsl:param name="css.decoration">1</xsl:param>

<doc:param name="css.decoration" xmlns="">
<refpurpose>Enable CSS decoration of elements</refpurpose>
<refdescription>
<para>
If <literal>%css-decoration%</literal> is turned on, then HTML elements
produced by the
stylesheet may be decorated with STYLE attributes.  For example, the
LI tags produced for list items may include a fragment of CSS in the
STYLE attribute which sets the CSS property "list-style-type".
</para>
</refdescription>
</doc:param>

</xsl:stylesheet>
