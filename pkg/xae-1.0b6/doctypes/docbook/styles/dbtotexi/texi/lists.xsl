<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: lists.xsl,v 1.12 2000/08/21 23:53:33 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="itemizedlist">
  <xsl:if test="title">
    <xsl:apply-templates select="title"/>
  </xsl:if>
  <xsl:call-template name="texinfo.anchor" />
  <itemize>
    <xsl:if test="@mark">
      <xsl:attribute name="mark">
        <xsl:value-of select="@mark" />
      </xsl:attribute>
    </xsl:if>
      
    <xsl:apply-templates select="listitem"/>
  </itemize>
</xsl:template>

<xsl:template match="itemizedlist/title">
  <xsl:call-template name="caption" />
</xsl:template>

<xsl:template name="orderedlist-starting-number">
  <xsl:param name="list" select="."/>
  <xsl:choose>
    <xsl:when test="$list/@continuation != 'continues'">1</xsl:when>
    <xsl:otherwise>
      <xsl:variable name="prevlist"
                    select="$list/preceding::orderedlist[1]"/>
      <xsl:choose>
        <xsl:when test="count($prevlist) = 0">2</xsl:when>
        <xsl:otherwise>
          <xsl:variable name="prevlength" select="count($prevlist/listitem)"/>
          <xsl:variable name="prevstart">
            <xsl:call-template name="orderedlist-starting-number">
              <xsl:with-param name="list" select="$prevlist"/>
            </xsl:call-template>
          </xsl:variable>
          <xsl:value-of select="$prevstart + $prevlength"/>
        </xsl:otherwise>
      </xsl:choose>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:template match="orderedlist">
  <xsl:variable name="start">
    <xsl:choose>
      <xsl:when test="@continuation='continues'">
        <xsl:call-template name="orderedlist-starting-number"/>
      </xsl:when>
      <xsl:otherwise>1</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <xsl:if test="title">
    <xsl:apply-templates select="title"/>
  </xsl:if>

  <xsl:call-template name="texinfo.anchor" />
 
  <enumerate>
    <xsl:if test="$start != '1'">
      <xsl:attribute name="start">
        <xsl:value-of select="$start"/>
      </xsl:attribute>
    </xsl:if>
    <xsl:apply-templates select="listitem"/>
  </enumerate>
    
</xsl:template>

<xsl:template match="orderedlist/title">
  <xsl:call-template name="caption" />
</xsl:template>

<xsl:template match="variablelist">
  <xsl:if test="title">
    <xsl:apply-templates select="title"/>
  </xsl:if>
  <xsl:call-template name="texinfo.anchor" />
  <table>
    <xsl:apply-templates select="varlistentry"/>
  </table>
</xsl:template>

<xsl:template match="variablelist/title">
  <xsl:call-template name="caption" />
</xsl:template>

<xsl:template match="listitem">
  <item />
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="listitem" mode="xref">
  <xsl:number format="1"/>
</xsl:template>

<xsl:template match="varlistentry">
  <xsl:apply-templates />
</xsl:template>

<xsl:template match="varlistentry/term">
  <itemx>
    <xsl:apply-templates />
  </itemx>
</xsl:template>

<xsl:template match="varlistentry/term[1]" priority="1">
  <item>
    <xsl:apply-templates />
  </item>
  <xsl:for-each select="..">
    <xsl:call-template name="texinfo.anchor" />
  </xsl:for-each>
</xsl:template>

<xsl:template match="varlistentry/listitem">
  <xsl:apply-templates/>
</xsl:template>


<!-- ==================================================================== -->

<xsl:template name="multitable.uniform.columnfractions">
  <xsl:param name="index">1</xsl:param>
  <xsl:param name="total">1</xsl:param>

  <xsl:value-of select="1 div $total" />
  <xsl:text> </xsl:text>

  <xsl:if test="$index &lt; $total">
    <xsl:call-template name="multitable.uniform.columnfractions">
      <xsl:with-param name="col" select="$index + 1" />
      <xsl:with-param name="total" select="$total" />
    </xsl:call-template>
  </xsl:if>
</xsl:template>


<xsl:template match="simplelist[@type='horiz']">
  <xsl:variable name="totalcolumns">
    <xsl:choose>
      <xsl:when test="@columns">
        <xsl:value-of select="@columns"/>
      </xsl:when>
    <xsl:otherwise>1</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <multitable>
    <!-- with simplelists we can correctly assume all items have the
         same width -->
    <xsl:attribute name="columnfractions">
      <xsl:call-template name="multitable.uniform.columnfractions">
        <xsl:with-param name="total" select="$totalcolumns" />
      </xsl:call-template>
    </xsl:attribute>
    
    <xsl:call-template name="simplelist.horiz">
      <xsl:with-param name="cols" select="$totalcolumns" />
    </xsl:call-template>

  </multitable>
</xsl:template>

<xsl:template match="simplelist[@type='vert']">
  <xsl:variable name="totalcolumns">
    <xsl:choose>
      <xsl:when test="@columns">
        <xsl:value-of select="@columns"/>
      </xsl:when>
    <xsl:otherwise>1</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <multitable>
    <!-- with simplelists we can correctly assume all items have the
         same width -->
    <xsl:attribute name="columnfractions">
      <xsl:call-template name="multitable.uniform.columnfractions">
        <xsl:with-param name="total" select="$totalcolumns" />
      </xsl:call-template>
    </xsl:attribute>
    
    <xsl:call-template name="simplelist.vert">
      <xsl:with-param name="cols" select="$totalcolumns" />
    </xsl:call-template>

  </multitable>
</xsl:template>


<xsl:template match="simplelist">
  <!-- with no type specified, the default is 'vert' -->
  <xsl:variable name="totalcolumns">
    <xsl:choose>
      <xsl:when test="@columns">
        <xsl:value-of select="@columns"/>
      </xsl:when>
    <xsl:otherwise>1</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <multitable>
    <!-- with simplelists we can correctly assume all items have the
         same width -->
    <xsl:attribute name="columnfractions">
      <xsl:call-template name="multitable.uniform.columnfractions">
        <xsl:with-param name="total" select="$totalcolumns" />
      </xsl:call-template>
    </xsl:attribute>
    
    <xsl:call-template name="simplelist.vert">
      <xsl:with-param name="cols" select="$totalcolumns" />
    </xsl:call-template>

  </multitable>
</xsl:template>

<xsl:template match="simplelist[@type='inline']">
  <xsl:apply-templates/>
</xsl:template>


<xsl:template name="simplelist.horiz">
  <xsl:param name="cols">1</xsl:param>
  <xsl:param name="cell">1</xsl:param>
  <xsl:param name="members" select="./member"/>

  <xsl:if test="$cell &lt;= count($members)">
    <xsl:call-template name="simplelist.horiz.row">
      <xsl:with-param name="cols" select="$cols"/>
      <xsl:with-param name="cell" select="$cell"/>
      <xsl:with-param name="members" select="$members"/>
    </xsl:call-template>
    
    <xsl:call-template name="simplelist.horiz">
      <xsl:with-param name="cols" select="$cols"/>
      <xsl:with-param name="cell" select="$cell + $cols"/>
      <xsl:with-param name="members" select="$members"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="simplelist.horiz.row">
  <xsl:param name="cols">1</xsl:param>
  <xsl:param name="cell">1</xsl:param>
  <xsl:param name="members" select="./member"/>
  <xsl:param name="curcol">1</xsl:param>

  <xsl:variable name="tag">
    <xsl:choose>
      <xsl:when test="$curcol = 1">item</xsl:when>
      <xsl:otherwise>tab</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
    
  <xsl:if test="$curcol &lt;= $cols">
    <xsl:element name="{$tag}" />
  
    <xsl:if test="$members[position()=$cell]">
      <xsl:apply-templates select="$members[position()=$cell]"/>
    </xsl:if>
    
    <xsl:call-template name="simplelist.horiz.row">
      <xsl:with-param name="cols" select="$cols"/>
      <xsl:with-param name="cell" select="$cell+1"/>
      <xsl:with-param name="members" select="$members"/>
      <xsl:with-param name="curcol" select="$curcol+1"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="simplelist.vert">
  <xsl:param name="cols">1</xsl:param>
  <xsl:param name="cell">1</xsl:param>
  <xsl:param name="members" select="./member"/>
  <xsl:param name="rows"
             select="floor((count($members)+$cols - 1) div $cols)"/>

  <xsl:if test="$cell &lt;= $rows">
    <xsl:call-template name="simplelist.vert.row">
      <xsl:with-param name="cols" select="$cols"/>
      <xsl:with-param name="rows" select="$rows"/>
      <xsl:with-param name="cell" select="$cell"/>
      <xsl:with-param name="members" select="$members"/>
    </xsl:call-template>
   
    <xsl:call-template name="simplelist.vert">
      <xsl:with-param name="cols" select="$cols"/>
      <xsl:with-param name="cell" select="$cell+1"/>
      <xsl:with-param name="members" select="$members"/>
      <xsl:with-param name="rows" select="$rows"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="simplelist.vert.row">
  <xsl:param name="cols">1</xsl:param>
  <xsl:param name="rows">1</xsl:param>
  <xsl:param name="cell">1</xsl:param>
  <xsl:param name="members" select="./member"/>
  <xsl:param name="curcol">1</xsl:param>

  <xsl:variable name="tag">
    <xsl:choose>
      <xsl:when test="$curcol = 1">item</xsl:when>
      <xsl:otherwise>tab</xsl:otherwise>
    </xsl:choose>
  </xsl:variable>
  
  <xsl:if test="$curcol &lt;= $cols">
    <xsl:element name="{$tag}" />
    
    <xsl:if test="$members[position()=$cell]">
      <xsl:apply-templates select="$members[position()=$cell]"/>
    </xsl:if>
    
    <xsl:call-template name="simplelist.vert.row">
      <xsl:with-param name="cols" select="$cols"/>
      <xsl:with-param name="rows" select="$rows"/>
      <xsl:with-param name="cell" select="$cell+$rows"/>
      <xsl:with-param name="members" select="$members"/>
      <xsl:with-param name="curcol" select="$curcol+1"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template match="member">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="simplelist[@type='inline']/member">
  <xsl:call-template name="gentext.char">
    <xsl:with-param name="key" select="'inline.separator'" />
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="simplelist[@type='inline']/member[1]" priority="1">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="procedure">
  <xsl:if test="title">
    <xsl:apply-templates select="title" mode="procedure.title.mode"/>
  </xsl:if>
  
  <enumerate>
    <xsl:apply-templates/>
  </enumerate>
</xsl:template>

<xsl:template match="procedure/title">
</xsl:template>

<xsl:template match="title" mode="procedure.title.mode">
  <xsl:call-template name="caption" />
</xsl:template>

<xsl:template match="substeps">
  <enumerate><xsl:apply-templates/></enumerate>
</xsl:template>

<xsl:template match="step">
  <item /><xsl:apply-templates/>
</xsl:template>

<xsl:template match="step/title">
  <xsl:apply-templates select="." mode="procedure.title.mode"/>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="segmentedlist">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="segmentedlist/title">
  <xsl:call-template name="caption" />
</xsl:template>

<xsl:template match="segtitle">
</xsl:template>

<xsl:template match="segtitle" mode="segtitle-in-seg">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="seglistitem">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="seg">
  <xsl:variable name="segnum" select="position()"/>
  <xsl:variable name="seglist" select="ancestor::segmentedlist"/>
  <xsl:variable name="segtitles" select="$seglist/segtitle"/>

  <!--
     Note: segtitle is only going to be the right thing in a well formed
     SegmentedList.  If there are too many Segs or too few SegTitles,
     you'll get something odd...maybe an error
  -->

  <para>
    <xsl:apply-templates select="$segtitles[$segnum=position()]"
                           mode="segtitle-in-seg"/>
    <xsl:text>: </xsl:text><!-- FIXME i18n -->
    <xsl:apply-templates/>
  </para>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="calloutlist">
  <xsl:if test="./title">
    <xsl:call-template name="caption">
      <xsl:with-param name="content">
        <xsl:apply-templates select="./title" mode="calloutlist.title.mode"/>
      </xsl:with-param>
    </xsl:call-template>
  </xsl:if>
  <itemize>
    <xsl:apply-templates/>
  </itemize>
</xsl:template>

<xsl:template match="calloutlist/title">
</xsl:template>

<xsl:template match="calloutlist/title" mode="calloutlist.title.mode">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="callout">
  <item />
  <xsl:call-template name="callout.arearefs">
    <xsl:with-param name="arearefs" select="@arearefs"/>
  </xsl:call-template>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template name="callout.arearefs">
  <xsl:param name="arearefs"></xsl:param>
  <xsl:if test="$arearefs!=''">
    <xsl:choose>
      <xsl:when test="substring-before($arearefs,' ')=''">
        <xsl:call-template name="callout.arearef">
          <xsl:with-param name="arearef" select="$arearefs"/>
        </xsl:call-template>
      </xsl:when>
      <xsl:otherwise>
        <xsl:call-template name="callout.arearef">
          <xsl:with-param name="arearef"
                          select="substring-before($arearefs,' ')"/>
        </xsl:call-template>
      </xsl:otherwise>
    </xsl:choose>
    <xsl:call-template name="callout.arearefs">
      <xsl:with-param name="arearefs"
                      select="substring-after($arearefs,' ')"/>
    </xsl:call-template>
  </xsl:if>
</xsl:template>

<xsl:template name="callout.arearef">
  <xsl:param name="arearef"></xsl:param>
  <xsl:variable name="targets" select="id($arearef)"/>
  <xsl:variable name="target" select="$targets[1]"/>

  <xsl:choose>
    <xsl:when test="count($target)=0">
      <xsl:value-of select="$arearef"/>
      <xsl:text>: ???</xsl:text>
    </xsl:when>
    <xsl:when test="$target/self::co">
      <a>
        <xsl:attribute name="href">
          <xsl:text>#</xsl:text>
          <xsl:value-of select="$arearef"/>
        </xsl:attribute>
        <xsl:apply-templates select="$target" mode="callout-bug"/>
      </a>
      <xsl:text> </xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <xsl:text>???</xsl:text>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>

