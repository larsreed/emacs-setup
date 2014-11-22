<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<!DOCTYPE xsl:stylesheet [
<!ENTITY mdash "&#x2014;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: block.xsl,v 1.7 2000/08/20 03:16:34 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template name="block.object">
  <para>
    <xsl:call-template name="texinfo.anchor" />
    <xsl:apply-templates/>
  </para>
</xsl:template>

<xsl:template name="indented.block.object">
  <quotation><!-- evil hack -->
    <xsl:call-template name="texinfo.anchor" />
    <xsl:apply-templates/>
  </quotation>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="para|simpara">
  <para>
    <xsl:call-template name="texinfo.anchor" />
    <xsl:apply-templates/>
  </para>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="formalpara">
  <para>
    <xsl:call-template name="texinfo.anchor" />
    <xsl:apply-templates/>
  </para>
</xsl:template>

<xsl:template match="formalpara/title">
  <!-- run-in head; FIXME i18n -->
  <strong><xsl:apply-templates/>.</strong>
  <xsl:text> </xsl:text>
</xsl:template>

<xsl:template match="formalpara/para">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="blockquote">
  <quotation>
    <xsl:call-template name="texinfo.anchor" />
    <xsl:apply-templates/>
    <xsl:apply-templates select="attribution" mode="blockquote.attribution.mode" />
  </quotation>
</xsl:template>

<xsl:template match="blockquote/title">
  <xsl:call-template name="caption" />
</xsl:template>

<xsl:template match="attribution"></xsl:template>
<xsl:template match="attribution" mode="blockquote.attribution.mode">
  <para>&mdash; <xsl:apply-templates /></para>
</xsl:template>

<xsl:template match="epigraph">
  <xsl:apply-templates />
  <xsl:apply-templates select="attribution" mode="blockquote.attribution.mode" />
</xsl:template>


<!-- ==================================================================== -->

<xsl:template match="sidebar">
  <xsl:call-template name="texinfo.anchor" />
  <xsl:call-template name="texinfo.section">
    <xsl:with-param name="level">
      <xsl:call-template name="texinfo.section.level">
        <xsl:with-param name="heading.class" select="'chapheading'" />
      </xsl:call-template>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates />
</xsl:template>


<!-- ==================================================================== -->

<xsl:template match="abstract">
  <xsl:call-template name="texinfo.anchor" />
  <xsl:call-template name="texinfo.section">
    <xsl:with-param name="level">
      <xsl:call-template name="texinfo.section.level">
        <xsl:with-param name="heading.class" select="'chapheading'" />
      </xsl:call-template>
    </xsl:with-param>
  </xsl:call-template>
  <xsl:apply-templates />
</xsl:template>


<!-- ==================================================================== -->

<xsl:template match="msgset">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="msgentry">
  <xsl:call-template name="block.object"/>
</xsl:template>

<xsl:template match="simplemsgentry">
  <xsl:call-template name="block.object"/>
</xsl:template>

<xsl:template match="msg">
  <xsl:call-template name="block.object"/>
</xsl:template>

<xsl:template match="msgmain">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="msgmain/title">
  <strong><xsl:apply-templates/></strong>
</xsl:template>

<xsl:template match="msgsub">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="msgsub/title">
  <strong><xsl:apply-templates/></strong>
</xsl:template>

<xsl:template match="msgrel">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="msgrel/title">
  <strong><xsl:apply-templates/></strong>
</xsl:template>

<xsl:template match="msgtext">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="msginfo">
  <xsl:call-template name="block.object"/>
</xsl:template>

<xsl:template match="msglevel|msgorig|msgaud">
  <xsl:call-template name="caption">
    <xsl:with-param name="content">
      <xsl:call-template name="gentext.element"/>
      <xsl:text>: </xsl:text><!-- FIXME -->
      <xsl:apply-templates/>
    </xsl:with-param>
  </xsl:call-template>
</xsl:template>

<xsl:template match="msgexplan">
  <xsl:call-template name="block.object"/>
</xsl:template>

<xsl:template match="msgexplan/title">
  <para><strong><xsl:apply-templates/></strong></para>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="revhistory">
  <multitable distribution=".33 .33 .33">
    <xsl:apply-templates />
  </multitable>
</xsl:template>
  
<xsl:template match="revhistory/revision">
  <xsl:variable name="revnumber" select=".//revnumber"/>
  <xsl:variable name="revdate"   select=".//date"/>
  <xsl:variable name="revauthor" select=".//authorinitials"/>
  <xsl:variable name="revremark" select=".//revremark|../revdescription"/>

  <item />
  <xsl:if test="$revnumber">
    <xsl:call-template name="gentext.element"/>
    <xsl:text> </xsl:text>
    <xsl:apply-templates select="$revnumber"/>
  </xsl:if>
  
  <tab />
  <xsl:apply-templates select="$revdate"/>

  <tab />
  <xsl:if test="count($revauthor)!=0">
    <xsl:apply-templates select="$revauthor"/>
  </xsl:if>

  <xsl:if test="$revremark">
    <item />
      <xsl:apply-templates select="$revremark"/>
  </xsl:if>
</xsl:template>

<xsl:template match="revision/revnumber">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="revision/date">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="revision/authorinitials">
  <xsl:text>, </xsl:text>
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="revision/authorinitials[1]">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="revision/revremark">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="revision/revdescription">
  <xsl:apply-templates/>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="ackno">
  <para>
    <xsl:apply-templates/>
  </para>
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
