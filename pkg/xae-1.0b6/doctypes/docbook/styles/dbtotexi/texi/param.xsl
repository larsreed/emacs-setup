<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0">

<!-- ********************************************************************
     $Id: param.xsl,v 1.7 2000/08/21 22:52:45 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->
<xsl:param name="caption.use.heading" select="false()" />

<doc:variable name="caption.use.heading" xmlns="">
<refpurpose>Use heading markup for minor captions?</refpurpose>
<refdescription>
<para>If true (non-zero), <sgmltag class="element">title</sgmltag>
content in some (formal) objects are rendered with the Texinfo
<markup>@<replaceable>heading</replaceable></markup> commands.
</para>
<para>
If false, captions are rendered as an emphasized paragraph.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="user.message.lang" select="'en'" />

<doc:variable name="user.message.lang" xmlns="">
<refpurpose>Language to use for stylesheet messages</refpurpose>
<refdescription>
<para>
This parameter should be set to the desired language for stylesheet
messages (typically warnings and errors).
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="link.use.pxref" select="true()" />

<doc:variable name="link.use.pxref" xmlns="">
<refpurpose>Translate <sgmltag class="element">link</sgmltag> using
<markup>@pxref</markup></refpurpose>
<refdescription>
<para>
If true (non-zero), <sgmltag class="element">link</sgmltag> is translated
with the hypertext followed by the cross reference in parentheses.
</para>
<para>
Otherwise, the hypertext content serves as the cross-reference name
marked up using <markup>@ref</markup>.  This is likely to look
suboptimal in info.
</para>
</refdescription>
</doc:variable>


<!-- ==================================================================== -->
<xsl:param name="texinfo.nodename.fallback.object-id" select="true()" />

<doc:variable name="texinfo.nodename.fallback.object-id" xmlns="">
<refpurpose>Use the ID of the source node if Texinfo nodename resolution
fails</refpurpose>
<refdescription>
<para>
If true (non-zero), the <function>object.id</function> template is used
to derive a Texinfo nodename from any element, if a readable,
non-colliding nodename cannot be generated.
<function>object.id</function> uses the ID specified in the source
element node, which is often more readable than the ID obtained by
<function>generate-id()</function>.  However, the stylesheet currently
does not check for collisions with existing nodenames by source IDs.
</para>
<para>
If this parameter is set to false, <function>generate-id()</function>
is used to derive a Texinfo nodename.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="inlineequation.alt.math" select="false()" />

<doc:variable name="inlineequation.alt.math" xmlns="">
<refpurpose>Use <markup>@math</markup> markup for text 
<sgmltag>inlineequation</sgmltag>s ?</refpurpose>
<refdescription>
<para>If true (non-zero), <sgmltag>alt</sgmltag> content in
<sgmltag>inlineequation</sgmltag> will be wrapped in the
<sgmltag>math</sgmltag> element (corresponding to <markup>@math</markup> in Texinfo).
</para>
<para>
This setting assumes that the <sgmltag>alt</sgmltag> content is already in
Texinfo's <markup>@math</markup> syntax.
</para>
</refdescription>
</doc:variable>


<!-- ==================================================================== -->
<xsl:param name="show.comments" select="true()"/>

<doc:variable name="show.comments" xmlns="">
<refpurpose>Display <sgmltag>comment</sgmltag> elements?</refpurpose>
<refdescription>
<para>If true (non-zero), comments will be displayed, otherwise they are suppressed.
Comments here refers to the <sgmltag>comment</sgmltag> element,
which will be renamed <sgmltag>remark</sgmltag> in DocBook V4.0,
not XML comments (&lt;-- like this --&gt;) which are unavailable.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="funcsynopsis.style">ansi</xsl:param>

<doc:variable name="funcsynopsis.style" xmlns="">
<refpurpose>What style of 'FuncSynopsis' should be generated?</refpurpose>
<refdescription>

<variablelist>

<varlistentry>
<term><literal>ansi</literal></term>
<listitem><para>Function prototypes in ANSI-C style</para></listitem>
</varlistentry>

<varlistentry>
<term><literal>kr</literal></term>
<listitem><para>K&amp;R-style function synopses</para></listitem>
</varlistentry>

</variablelist>

</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="funcsynopsis.decoration" select="true()" />

<doc:variable name="funcsynopsis.decoration" xmlns="">
<refpurpose>Decorate elements of a FuncSynopsis?</refpurpose>
<refdescription>
<para>If true (non-zero), elements of the FuncSynopsis will be decorated (e.g. bold or
italic).  The decoration is controlled by functions that can be redefined
in a customization layer.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="function.parens" select="false()" />

<doc:variable name="function.parens" xmlns="">
<refpurpose>Generate parentheses after a function?</refpurpose>
<refdescription>
<para>If not 0, the formatting of
a <sgmltag class="starttag">function</sgmltag> element will include
generated parenthesis.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="refentry.generate.name" select="true()"/>

<doc:variable name="refentry.generate.name" xmlns="">
<refpurpose>Output NAME header before 'RefName'(s)?</refpurpose>
<refdescription>
<para>If true (non-zero), a "NAME" section title is output before the list
of 'RefName's.
</para>
</refdescription>
</doc:variable>


<!-- ==================================================================== -->
<xsl:param name="refentry.xref.manvolnum" select="true()"/>

<doc:variable name="refentry.xref.manvolnum" xmlns="">
<refpurpose>Output <sgmltag>manvolnum</sgmltag> as part of
<sgmltag>refentry</sgmltag> cross-reference?</refpurpose>
<refdescription>
<para>if true (non-zero), the <sgmltag>manvolnum</sgmltag> is used when cross-referencing
<sgmltag>refentry</sgmltag>s, either with <sgmltag>xref</sgmltag>
or <sgmltag>citerefentry</sgmltag>.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:param name="check.idref" select="true()" />

<doc:variable name="check.idref" xmlns="">
<refpurpose>Test the target of <sgmltag class="attribute">IDREF</sgmltag>
attributes?</refpurpose>
<refdescription>
<para>If true, the target of <sgmltag class="attribute">IDREF</sgmltag> attributes
are tested for presence (and uniqueness). This can be very expensive in large documents.
</para>
</refdescription>
</doc:variable>



<!-- ==================================================================== -->
<!-- FIXME: the following may or may not apply -->

<!-- ==================================================================== -->
<xsl:variable name="author.othername.in.middle" select="1"/>

<doc:variable name="author.othername.in.middle" xmlns="">
<refpurpose>Is <sgmltag>othername</sgmltag> in <sgmltag>author</sgmltag> a
middle name?</refpurpose>
<refdescription>
<para>If true (non-zero), the <sgmltag>othername</sgmltag> of an <sgmltag>author</sgmltag>
appears between the <sgmltag>firstname</sgmltag> and
<sgmltag>surname</sgmltag>.  Otherwise, <sgmltag>othername</sgmltag>
is suppressed.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="section.autolabel" select="0"/>

<doc:variable name="section.autolabel" xmlns="">
<refpurpose>Are sections enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled sections will be enumerated.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="section.label.includes.component.label" select="0"/>

<doc:variable name="section.label.includes.component.label" xmlns="">
<refpurpose>Do section labels include the component label?</refpurpose>
<refdescription>
<para>If true (non-zero), section labels are prefixed with the label of the
component that contains them.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="chapter.autolabel" select="1"/>

<doc:variable name="chapter.autolabel" xmlns="">
<refpurpose>Are chapters and appendixes enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled chapters and appendixes will be enumerated.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="preface.autolabel" select="0"/>

<doc:variable name="preface.autolabel" xmlns="">
<refpurpose>Are prefaces enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled prefaces will be enumerated.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="part.autolabel" select="1"/>

<doc:variable name="part.autolabel" xmlns="">
<refpurpose>Are parts and references enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled parts and references will be enumerated.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="qandadiv.autolabel" select="1"/>

<doc:variable name="qandadiv.autolabel" xmlns="">
<refpurpose>Are divisions in QAndASets enumerated?</refpurpose>
<refdescription>
<para>If true (non-zero), unlabeled qandadivs will be enumerated.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="qanda.inherit.numeration" select="1"/>

<doc:variable name="qanda.inherit.numeration" xmlns="">
<refpurpose>Does enumeration of QandASet components inherit the numeration of parent elements?</refpurpose>
<refdescription>
<para>If true (non-zero), numbered QandADiv elements and Questions and Answers inherit
the numeration of the ancestors of the QandASet.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="qanda.defaultlabel">number</xsl:variable>

<doc:variable name="qanda.defaultlabel" xmlns="">
<refpurpose>Sets the default for defaultlabel on QandASet.</refpurpose>
<refdescription>
<para>If no defaultlabel attribute is specified on a QandASet, this
value is used. It must be one of the legal values for the defaultlabel
attribute.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="generate.qandaset.toc">1</xsl:variable>

<doc:variable name="generate.qandaset.toc" xmlns="">
<refpurpose>Is a Table of Contents created for QandASets?</refpurpose>
<refdescription>
<para>If true (non-zero), a ToC is constructed for QandASets.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="generate.qandadiv.toc">0</xsl:variable>

<doc:variable name="generate.qandadiv.toc" xmlns="">
<refpurpose>Is a Table of Contents created for QandADivs?</refpurpose>
<refdescription>
<para>If true (non-zero), a ToC is constructed for QandADivs.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="biblioentry.item.separator">. </xsl:variable>

<doc:variable name="biblioentry.item.separator" xmlns="">
<refpurpose>Text to separate bibliography entries</refpurpose>
<refdescription>
<para>Text to separate bibliography entries
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="toc.section.depth">2</xsl:variable>

<doc:variable name="toc.section.depth" xmlns="">
<refpurpose>How deep should recursive <sgmltag>section</sgmltag>s appear
in the TOC?</refpurpose>
<refdescription>
<para>Specifies the depth to which recursive sections should appear in the
TOC.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="graphic.default.extension"></xsl:variable>

<doc:variable name="graphic.default.extension" xmlns="">
<refpurpose>Default extension for graphic filenames</refpurpose>
<refdescription>
<para>If a <sgmltag>graphic</sgmltag> or <sgmltag>mediaobject</sgmltag>
includes a reference to a filename that does not include an extension,
and the <sgmltag class="attribute">format</sgmltag> attribute is
<emphasis>unspecified</emphasis>, the default extension will be used.
</para>
</refdescription>
</doc:variable>

<!-- ==================================================================== -->
<xsl:variable name="toc.list.type">dl</xsl:variable>

<doc:variable name="toc.list.type" xmlns="">
<refpurpose>Type of HTML list element to use for Tables of Contents</refpurpose>
<refdescription>
<para>When an automatically generated Table of Contents (or List of Titles)
is produced, this HTML element will be used to make the list.
</para>
</refdescription>
</doc:variable>


</xsl:stylesheet>

