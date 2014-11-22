<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: index.xsl,v 1.7 2000/08/21 16:17:18 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="index|setindex">
  <!-- Norm says: 
       some implementations use completely empty index tags to indicate
       where an automatically generated index should be inserted. so
       if the index is completely empty, skip it.

       I say:
       Only if the index does not contain indexdiv or indexentry
       then the above should occur.  It's perfectly reasonable that
       someone would want to write introductory matter before
       the generated index is inserted.  However, where we can find
       out what type of index to generate is a bit ill-defined.
       For now, use the role attribute on the index/setindex itself.
  -->

  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />

  <xsl:apply-templates />

  <xsl:if test="count(./indexdiv | ./indexentry)=0">
    <printindex>
      <xsl:attribute name="class">
        <xsl:value-of select="@role" />
      </xsl:attribute>
    </printindex>
  </xsl:if>
      
</xsl:template>


<!-- ==================================================================== -->

<xsl:template match="indexdiv">
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="indexterm">
  <indexterm>
    <xsl:attribute name="class">
      <xsl:choose>
        <xsl:when test="@role"><!-- FIXME -->
          <xsl:value-of select="@role" />
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>c</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:attribute>
    <xsl:apply-templates />
  </indexterm>
</xsl:template>

<xsl:template match="primary">
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="secondary|tertiary">
  <xsl:call-template name="gentext.element" />
</xsl:template>

<xsl:template match="see|seealso">
<!-- FIXME -->
</xsl:template>

<xsl:template match="indexentry"></xsl:template>
<xsl:template match="primaryie|secondaryie|tertiaryie|seeie|seealsoie">
</xsl:template>

</xsl:stylesheet>
