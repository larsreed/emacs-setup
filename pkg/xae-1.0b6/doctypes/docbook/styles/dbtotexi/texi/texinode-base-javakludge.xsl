<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:hash="http://www.jclark.com/xt/java/XSLHashtable"
                xmlns:doc="http://nwalsh.com/xsl/documentation/1.0"
                exclude-result-prefixes="doc hash"
                version='1.0'>

<!-- ********************************************************************
     $Id: texinode-base-javakludge.xsl,v 1.2 2000/08/21 20:19:55 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     This code uses a simple class, XSLHashtable, which wraps the
     functionality of java.util.Hashtable to work around stupid
     bugs/misfeatures of XT/Saxon/Kaffe.  This class must be on
     the class path when executing the XSLT processor.
     Used in lieu of texinode-base-java.xsl.
     
     ******************************************************************** -->

<!-- ==================================================================== -->
<!-- Variables -->

<!-- ID -> Texinfo nodename map -->
<xsl:variable name="nodeteximap" select="hash:new()" />






<!-- ==================================================================== -->

<doc:template name="texinfo.nodename.compute" xmlns="">
<refpurpose>Compute (part of) the Texinfo nodename</refpurpose>
<refdescription>
<para>
This is a subroutine used by <function>texinfo.nodename</function>
to derive a nodename from the given node.  It checks the suggested
name for collisions with existing names.  If there is a collision, 
it prepends the parent's nodename to the suggested name.
</para>
<para>
If the suggested name is not given, it applies the texinfo.nodename.mode
templates to find one for the given node,
</para>
<para>
This function returns the nodename <emphasis>with the filename and colon</emphasis>
prepended to it, simply to make the <function>texinfo.nodename</function> process 
more efficient.
</para>
</refdescription>
<refparameter>
<variablelist>
<varlistentry>
<term><parameter>sugname</parameter></term>
<listitem><para>
A string which is the suggested name.  If not given, regular templates
are applied.
</para></listitem>
</varlistentry>
</variablelist>
</refparameter>
</doc:template>

<xsl:template name="texinfo.nodename.compute">
  <xsl:param name="node" select="." />

  <xsl:param name="sugname">
    <xsl:apply-templates select="$node" mode="texinfo.nodename.mode" />
  </xsl:param>

  <xsl:variable name="file">
    <xsl:call-template name="texinfo.filename">
      <xsl:with-param name="node" select="$node" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:variable name="qsugname"
                select="concat($file,':',$sugname)" />

  <xsl:choose>
    <xsl:when test="$sugname = ''">
      <xsl:call-template name="user.message">
        <xsl:with-param name="node" select="$node" />
        <xsl:with-param name="key">Cannot generate readable nodename</xsl:with-param>
      </xsl:call-template>

      <xsl:variable name="id">
        <xsl:choose>
          <xsl:when test="$texinfo.nodename.fallback.object-id">
            <xsl:call-template name="object.id">
              <xsl:with-param name="object" select="$node" />
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:value-of select="generate-id($node)" />
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <xsl:value-of select="concat($file,':',$id)" />
    </xsl:when>

    <xsl:when test="not(hash:contains-value($nodeteximap,$qsugname))">
      <xsl:value-of select="$qsugname" />
    </xsl:when>

    <xsl:otherwise>
      <xsl:variable name="parentnodename">
        <xsl:call-template name="texinfo.nodename">
          <xsl:with-param name="node" select="$node/.." />
        </xsl:call-template>
      </xsl:variable>

      <xsl:variable name="qpsugname"
                    select="concat($file,':',$parentnodename,' - ',$sugname)" />

      <xsl:choose>
        <xsl:when test="hash:contains-value($nodeteximap,$qpsugname)">
          <xsl:call-template name="user.message">
            <xsl:with-param name="node" select="$node" />
            <xsl:with-param name="key">Cannot generate readable, non-colliding nodename</xsl:with-param>
          </xsl:call-template>

          <xsl:variable name="id">
            <xsl:choose>
              <xsl:when test="$texinfo.nodename.fallback.object-id">
                <xsl:call-template name="object.id">
                  <xsl:with-param name="object" select="$node" />
                </xsl:call-template>
              </xsl:when>
              <xsl:otherwise>
                <xsl:value-of select="generate-id($node)" />
              </xsl:otherwise>
            </xsl:choose>
          </xsl:variable>

          <xsl:value-of select="concat($file,':',$id)" />
        </xsl:when>

        <xsl:otherwise>
          <xsl:value-of select="$qpsugname" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>






<doc:template name="texinfo.nodename" xmlns="">
<refpurpose>Find the Texinfo nodename</refpurpose>
<refdescription>
<para>
Returns the Texinfo nodename from the given node.  This nodename
is guaranteed to be unique across the target Texinfo file.
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
<varlistentry>
<term><parameter>sugname</parameter></term>
<listitem><para>
If the template needs to create a new name, try to use the
suggested name instead of the default names.
</para></listitem>
</varlistentry>
</variablelist>
</refparameter>
</doc:template>

<xsl:template name="texinfo.nodename">
  <xsl:param name="node" select="." />
  <xsl:param name="sugname" />

  <xsl:variable name="id" select="generate-id($node)" />

  <xsl:choose>
    <xsl:when test="hash:contains-key($nodeteximap,$id)">
      <xsl:value-of select="substring-after(hash:getValue($nodeteximap,$id),':')" />
    </xsl:when>

    <xsl:otherwise>
      <xsl:variable name="newname">
        <xsl:choose>
          <xsl:when test="$sugname">
            <xsl:call-template name="texinfo.nodename.compute">
              <xsl:with-param name="node" select="$node" />
              <xsl:with-param name="sugname" select="$sugname" />
            </xsl:call-template>
          </xsl:when>
          <xsl:otherwise>
            <xsl:call-template name="texinfo.nodename.compute">
              <xsl:with-param name="node" select="$node" />
            </xsl:call-template>
          </xsl:otherwise>
        </xsl:choose>
      </xsl:variable>

      <!-- Evaluate side effects.
           Extension elements are clearly the best way to do this but
           there is unfortunately no simple standard to interface
           Java that way. -->
      <xsl:comment>
        <xsl:value-of select="hash:put($nodeteximap,string($id),string($newname))" />
      </xsl:comment>
      
      <xsl:value-of select="substring-after($newname,':')" />
    </xsl:otherwise>

  </xsl:choose>
</xsl:template>
  
</xsl:stylesheet>

