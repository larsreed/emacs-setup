<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: table.xsl,v 1.6 2000/08/22 00:36:46 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<xsl:template match="tgroup">
  <multitable>
    <xsl:attribute name="columnfractions">
      <xsl:call-template name="calculate.columnfractions" />
    </xsl:attribute>

    <!-- order -->
    <xsl:apply-templates select="thead" />
    <xsl:apply-templates select="tbody" />
    <xsl:apply-templates select="tfoot" />

  </multitable>
</xsl:template>

<xsl:template match="colspec"></xsl:template>
<xsl:template match="spanspec"></xsl:template>

<xsl:template match="thead|tfoot|tbody">
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="row">
  <item />
  <!-- FIXME warn about unsupported atts -->

  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="entry[1]" priority="1">
  <xsl:apply-templates mode="force.inline.mode" />
</xsl:template>

<xsl:template match="entry">
  <tab /><!-- FIXME spanspec and all that stuff -->
  <xsl:apply-templates mode="force.inline.mode" />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="calculate.columnfractions">
  <xsl:param name="numcols" select="@cols" />
  <xsl:param name="curcol" select="1" />
  <xsl:param name="sum">
    <xsl:call-template name="calculate.columnfractions.sum" />
  </xsl:param>

  <xsl:if test="$curcol &lt;= $numcols">
    <xsl:variable name="width">
      <xsl:call-template name="get.proportional.columnwidth">
        <xsl:with-param name="wantcol" select="$curcol" />
      </xsl:call-template>
    </xsl:variable>

    <xsl:value-of select="$width div $sum" />
    <xsl:text> </xsl:text>
              
    <xsl:call-template name="calculate.columnfractions">
      <xsl:with-param name="numcols" select="$numcols" />
      <xsl:with-param name="curcol" select="$curcol + 1" />
      <xsl:with-param name="sum" select="$sum" />
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="calculate.columnfractions.sum">
  <xsl:param name="numcols" select="@cols" />
  <xsl:param name="curcol" select="1" />
  <xsl:param name="cursum" select="0" />

  <xsl:choose>
    <xsl:when test="$curcol &lt;= $numcols">
      <xsl:call-template name="calculate.columnfractions.sum">
        <xsl:with-param name="numcols" select="$numcols" />
        <xsl:with-param name="curcol" select="$curcol + 1" />
        <xsl:with-param name="cursum">
          <xsl:variable name="width">
            <xsl:call-template name="get.proportional.columnwidth">
              <xsl:with-param name="wantcol" select="$curcol" />
            </xsl:call-template>
          </xsl:variable>
          <xsl:value-of select="$cursum + $width" />
        </xsl:with-param>
      </xsl:call-template>
    </xsl:when>
    <xsl:otherwise>
      <xsl:value-of select="$cursum" />
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="get.colspec">
  <xsl:param name="wantcol" select="1" /><!-- column# to search for -->

  <xsl:variable name="colspecs" select="./colspec[@colnum=$wantcol]" />
  <xsl:variable name="colspec" select="$colspecs[1]" />

  <xsl:if test="count($colspecs)>1">
    <xsl:call-template name="user.message">
      <xsl:with-param name="arg" select="$wantcol" />
      <xsl:with-param name="key">Multiple colspecs for same column</xsl:with-param>
    </xsl:call-template>
  </xsl:if>

  <xsl:choose>
    <xsl:when test="$colspec">
      <xsl:copy-of select="$colspec" />
    </xsl:when>
    <xsl:otherwise>
      <xsl:call-template name="get.colspec.search">
        <xsl:with-param name="wantcol" select="$wantcol" />
      </xsl:call-template>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>
  
<!-- I really want to shoot whoever thought of the idea of making
     colspec/@colnum optional and making the stylesheet do a
     linear search to resolve it!
-->
<xsl:template name="get.colspec.search">
  <xsl:param name="colspecs" select="./colspec" />
  <xsl:param name="curcol" select="1" />
  <xsl:param name="wantcol" select="1" />

  <xsl:variable name="colspec" select="$colspecs[1]" />
  <xsl:variable name="nextcolspecs" select="$colspecs[position()>1]" />

  <!-- column number of this $colspec -->
  <xsl:variable name="colnum">
    <xsl:choose>
      <xsl:when test="not($colspec)">
      </xsl:when>
      <xsl:when test="$colspec/@colnum">
        <xsl:value-of select="$colspec/@colnum" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$curcol" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
    
  <xsl:if test="$colspec">
    <xsl:choose>
      <xsl:when test="$colnum = $wantcol">
        <xsl:copy-of select="$colspec" />
      </xsl:when>
      
      <xsl:otherwise>
        <xsl:call-template name="get.colspec.search">
          <xsl:with-param name="colspecs" select="$nextcolspecs" />
          <xsl:with-param name="curcol" select="$colnum+1" />
          <xsl:with-param name="wantcol" select="$wantcol" />
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:if>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template name="get.proportional.columnwidth">
  <xsl:param name="wantcol" select="1" />

  <xsl:variable name="colspec">
    <xsl:call-template name="get.colspec">
      <xsl:with-param name="wantcol" select="$wantcol" />
    </xsl:call-template>
  </xsl:variable>

  <xsl:choose>
    <xsl:when test="not($colspec)">
      <xsl:value-of select="1" />
    </xsl:when>
    
    <xsl:when test="$colspec/@colwidth">
      <xsl:variable name="colwidth" select="$colspec/@colwidth" />

      <xsl:choose>
        <xsl:when test="not(contains($colwidth,'*'))">
          <xsl:call-template name="user.message">
            <xsl:with-param name="node" select="$colspec" />
            <xsl:with-param name="key">Absolute column widths not supported</xsl:with-param>
          </xsl:call-template>
          <xsl:value-of select="1" />
        </xsl:when>

        <xsl:when test="$colwidth = '*'">
          <xsl:value-of select="1" />
        </xsl:when>

        <xsl:otherwise>
          <xsl:value-of select="substring-before($colwidth,'*')" />
        </xsl:otherwise>
      </xsl:choose>
    </xsl:when>

    <xsl:otherwise>
      <xsl:value-of select="1" />
    </xsl:otherwise>

  </xsl:choose>
</xsl:template>

</xsl:stylesheet>
