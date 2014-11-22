<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<!DOCTYPE xsl:stylesheet [
<!ENTITY RE "&#10;">
<!ENTITY nbsp "&#160;">
]>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                xmlns:gt="http://docbook2x.sourceforge.net/xmlns/xslt/gentext"
                exclude-result-prefixes="gt"
                version='1.0'>

<!-- ********************************************************************
     $Id: synop.xsl,v 1.12 2000/08/28 03:38:42 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<!-- Info uses decorations to represent semantic markup, which
     is undesirable in a verbatim synopsis.  So we use
     synopsis.mode to not use Texinfo's semantic markup. 
     However, we lose the ability to make links :(
-->

<xsl:template match="*" mode="synopsis.mode">
  <xsl:value-of select="." />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="synopsis">
  <example>
    <xsl:apply-templates mode="synopsis.mode" />
  </example>
</xsl:template>

<!-- ==================================================================== -->
<!-- Decoration strings for cmdsynopsis -->
<!-- FIXME move to common -->

<xsl:template name="cmdsynopsis.sepchar">
  <xsl:value-of select="ancestor-or-self::*/@sepchar"/>
  <!-- CHECKME: attr defaulting -->
</xsl:template>

<xsl:template name="gentext.arg.choice.start">
  <xsl:call-template name="gentext.char">
    <xsl:with-param name="key" select="concat('arg.choice.',@choice,'.start')" />
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.arg.choice.end">
  <xsl:call-template name="gentext.char">
    <xsl:with-param name="key" select="concat('arg.choice.',@choice,'.end')" />
  </xsl:call-template>
</xsl:template>

<xsl:template name="gentext.arg.rep">
  <xsl:if test="@rep = 'repeat'">
    <xsl:call-template name="gentext.char">
      <xsl:with-param name="key" select="'arg.rep.repeat'" />
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="gentext.group.arg.separator">
  <xsl:call-template name="gentext.char">
    <xsl:with-param name="key" select="'group.arg.separator'" />
  </xsl:call-template>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="cmdsynopsis">
  <quotation> 
    <t>
      <xsl:apply-templates mode="synopsis.mode" />
    </t>
  </quotation>
</xsl:template>

<xsl:template match="sbr" mode="synopsis.mode">
  <sp n="1" />
</xsl:template>

<xsl:template match="cmdsynopis/command" mode="synopsis.mode">
  <xsl:if test="position()>1">
    <xsl:call-template name="cmdsynopsis.sepchar" />
  </xsl:if>

  <xsl:apply-templates mode="synopsis.mode" />
</xsl:template>

<xsl:template match="cmdsynopsis/group|cmdsynopsis/arg"
              mode="synopsis.mode">
  <xsl:if test="position()>1">
    <xsl:call-template name="cmdsynopsis.sepchar" />
  </xsl:if>

  <xsl:call-template name="gentext.arg.choice.start" />
  
  <xsl:apply-templates mode="synopsis.mode" />
  
  <xsl:call-template name="gentext.arg.choice.end" />
  <xsl:call-template name="gentext.arg.rep" />
</xsl:template>

<xsl:template match="group|arg" mode="synopsis.mode">
  <xsl:call-template name="gentext.arg.choice.start" />
  
  <xsl:apply-templates mode="synopsis.mode" />
  
  <xsl:call-template name="gentext.arg.choice.end" />
  <xsl:call-template name="gentext.arg.rep" />
</xsl:template>

<xsl:template match="group/arg" mode="synopsis.mode">
  <xsl:if test="position()>1">
    <xsl:call-template name="gentext.group.arg.separator" />
  </xsl:if>

  <xsl:call-template name="gentext.arg.choice.start" />
  
  <xsl:apply-templates mode="synopsis.mode" />
  
  <xsl:call-template name="gentext.arg.choice.end" />
  <xsl:call-template name="gentext.arg.rep" />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="synopfragmentref" mode="synopsis.mode">
  <xsl:call-template name="gentext.element" />
</xsl:template>

<xsl:template match="synopfragment" mode="synopsis.mode">
  <xsl:call-template name="gentext.element" />
</xsl:template>

<xsl:template match="gt:element[@name='synopfragmentref']/gt:number" 
              mode="gentext.mode">
  <xsl:param name="node" />
  <xsl:variable name="target" select="id($node/@linkend)" />
  <xsl:for-each select="$target">
    <xsl:number format="1" />
  </xsl:for-each>
</xsl:template>

<xsl:template match="gt:element[@name='synopfragment']/gt:number" 
              mode="gentext.mode">
  <xsl:param name="node" />
  <xsl:for-each select="$node">
    <xsl:number format="1" />
  </xsl:for-each>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="funcsynopsis">
  <quotation>
    <xsl:apply-templates />
  </quotation>
</xsl:template>

<xsl:template match="funcsynopsisinfo">
  <!-- format rather than example is used to avoid indenting the
       funcsynopsisinfo block -->
  <format><t>
    <xsl:apply-templates mode="synopsis.mode" />
  </t></format>
</xsl:template>

<xsl:template match="funcprototype">
  <para>
    <t>  
      <xsl:apply-templates />
    </t>

    <xsl:if test="$funcsynopsis.style='kr'">
      <sp n="1" />
      <xsl:apply-templates select="./paramdef" mode="kr-funcsynopsis-mode"/>
    </xsl:if>
  </para>
</xsl:template>

<xsl:template match="funcdef">
  <xsl:apply-templates mode="synopsis.mode" />
</xsl:template>

<xsl:template match="function" mode="synopsis.mode">
  <xsl:choose>
    <xsl:when test="$funcsynopsis.decoration">
      <b><xsl:apply-templates mode="synopsis.mode" /></b>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates mode="synopsis.mode" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="void">
  <xsl:choose>
    <xsl:when test="$funcsynopsis.style='ansi'">
      <xsl:text>(void);</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>();</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="varargs">
  <xsl:text>(...);</xsl:text>
</xsl:template>

<xsl:template match="paramdef">
  <xsl:choose>
    <xsl:when test="preceding-sibling::*[position()=1 and local-name() = 'paramdef']">
      <xsl:text>, </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>(</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
  
  <xsl:choose>
    <xsl:when test="$funcsynopsis.style='kr'">
      <xsl:apply-templates select="./parameter" mode="synopsis.mode" />
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates mode="synopsis.mode" />
    </xsl:otherwise>
  </xsl:choose>
  
  <xsl:if test="not(following-sibling::*[position()=1 and local-name() = 'paramdef'])">
    <xsl:text>);</xsl:text>
  </xsl:if>
</xsl:template>

<xsl:template match="paramdef/parameter" mode="synopsis.mode">
  <xsl:choose>
    <xsl:when test="$funcsynopsis.decoration">
      <i><xsl:apply-templates mode="synopsis.mode" /></i>
    </xsl:when>
    <xsl:otherwise>
      <xsl:apply-templates mode="synopsis.mode" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="paramdef" mode="kr-funcsynopsis-mode">
  <xsl:apply-templates mode="synopsis.mode" />
  <xsl:text>;</xsl:text>
</xsl:template>

<xsl:template match="funcparams">
  <xsl:text>(</xsl:text>
  <xsl:apply-templates/>
  <xsl:text>)</xsl:text>
</xsl:template>

<!-- ==================================================================== -->
<!-- FIXME: bring back classsynopsis stuff ! -->

</xsl:stylesheet>
