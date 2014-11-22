<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc"
                version='1.0'>

<!-- ********************************************************************
     $Id: texinode-base.xsl,v 1.6 2000/08/16 00:38:38 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->
<!-- Java implementation -->
<!--<xsl:include href="texinode-base-java.xsl" />-->
<xsl:include href="texinode-base-javakludge.xsl" />

<!-- Implementation with ID as node names
     (very user-unfriendly, but portable) -->
<!--<xsl:include href="texinode-base-id.xsl" />-->



<!-- ==================================================================== -->
<!-- Anchors -->

<doc:template name="texinfo.anchor" xmlns="">
<refpurpose>Complete anchor link-end</refpurpose>
<refdescription>
<para>To complete the link end of a (cross) reference to an arbitrary
element that is not mapped to a Texinfo node, the following template
<emphasis>must</emphasis> be called before processing the element's
contents.</para>
</refdescription>
</doc:template>

<xsl:template name="texinfo.anchor">
  <xsl:if test="@id"><!-- see comment below -->
    <anchor>
      <xsl:attribute name="node">
        <xsl:call-template name="texinfo.nodename" />
      </xsl:attribute>
    </anchor>
  </xsl:if>
</xsl:template>

<!-- The above test assumes that any element with ID means that
     it will be referenced and an anchor should be created.
     Ideally, we should use keys (which XT doesn't support) 
     and check for IDREFs, like this:

<xsl:key name="referenced.anchors"
         match="@linkend"
         use="@linkend" />

<xsl:template name="texinfo.anchor">
  <xsl:if test="@id and key('referenced.anchors', @id)">
    <anchor>
      <xsl:attribute name="node">
        <xsl:call-template name="texinfo.nodename" />
      </xsl:attribute>
    </anchor>
  </xsl:if>
</xsl:template>

-->


<!-- ==================================================================== -->
<!-- Node statement -->

<doc:template name="texinfo.node" xmlns="">
<refpurpose>Output node element</refpurpose>
<refdescription>
<para>
This named template creates a node element in the result tree, 
which corresponds to the Texinfo <literal>@node</literal> command.
</para>
<para>
This template also generates menus if necessary.
</para>
</refdescription>
</doc:template>

<xsl:template name="texinfo.node">
  
  <!-- Safeguard code -->
  <xsl:variable name="isnode">
    <xsl:apply-templates select="." mode="texinfo.isnode.mode" />
  </xsl:variable>
  <xsl:if test="$isnode != '1'">
    <xsl:call-template name="user.message">
      <xsl:with-param name="key">texinfo.node called for non-node</xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:variable name="nodename">
    <xsl:call-template name="texinfo.nodename" />
  </xsl:variable>

  <!-- If this is the first 'subnode' of the parent node, then
       the parent node will end here, so make a menu.
  -->
  <xsl:variable name="isprevnode">
    <xsl:apply-templates select="preceding-sibling::*[position()=1]"
                         mode="texinfo.isnode.mode" />
  </xsl:variable>
  <xsl:if test="$isprevnode != '1' and $nodename!='Top'">
    <!-- A part special case is needed since it is not considered a node,
         but is intermingled with the other components.  One could say
         that there is a real FIXME in that this code breaks when
         components don't appear consecutively, but I haven't found
         an acceptable solution to that yet. -->
    <xsl:if test="not(parent::part or preceding-sibling::*[position()=1 and self::part])">
    
    <xsl:for-each select=".."><!-- It's best to let texinfo.menu assume
                                   that its context node is the parent node. -->
      <xsl:call-template name="texinfo.menu" />
    </xsl:for-each>

    </xsl:if>
  </xsl:if>
  
  <node>
    <xsl:attribute name="name">
      <xsl:value-of select="$nodename" />
    </xsl:attribute>
  </node>
</xsl:template>



<!-- ==================================================================== -->
<!-- Nodename stringifying mode, used by the nodename mode -->

<xsl:template match="*" mode="texinfo.nodename-string.mode">
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="text()" mode="texinfo.nodename-string.mode">
  <xsl:value-of select="translate(string(.), &quot;().,:'&quot;, &quot;[];;;_&quot;)" />
</xsl:template>



<!-- ==================================================================== -->
<!-- Texinfo multi-file handling -->

<doc:template name="texinfo.filename" xmlns="">
<refpurpose>Find the file that contains the result</refpurpose>
<refdescription>
<para>
Returns the Texinfo file that contains the result when transforming
the context node.
</para>
<para>
In this implementation, every document element starts a new file, unless
the document element is <sgmltag class="element">set</sgmltag>, in which
case each child <sgmltag class="element">book</sgmltag> starts a new
file.
</para>
</refdescription>
<refparameter>
<variablelist>
<varlistentry>
<term><parameter>node</parameter></term>
<listitem><para>
The node to find information for.  Default is the context node.
</para></listitem>
</varlistentry>
</variablelist>
</refparameter>
</doc:template>

<xsl:template name="texinfo.filename">
  <xsl:param name="node" select="." />
      
  <xsl:variable name="book" select="$node/ancestor-or-self::book" />

  <xsl:choose>
    <xsl:when test="$book">      
      <xsl:choose>
        <xsl:when test="$book/bookinfo/titleabbrev[@role='texinfo-file']">
          <xsl:value-of select="$book/bookinfo/titleabbrev[@role='texinfo-file']" />
        </xsl:when>
        <xsl:when test="$book/titleabbrev">
          <xsl:value-of select="$book/titleabbrev[1]" />
        </xsl:when>
        <xsl:when test="$book/title">
          <xsl:value-of select="$book/title[1]" />
        </xsl:when>
        <xsl:when test="$book/bookinfo/titleabbrev">
          <xsl:value-of select="$book/bookinfo/titleabbrev[1]" />
        </xsl:when>
        <xsl:when test="$book/bookinfo/title">
          <xsl:value-of select="$book/bookinfo/title[1]" />
        </xsl:when>
        <xsl:otherwise></xsl:otherwise>
      </xsl:choose>
    </xsl:when>

    <xsl:otherwise>
      <!-- else nothing.
           Maybe a FIXME -->
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>     

</xsl:stylesheet>

