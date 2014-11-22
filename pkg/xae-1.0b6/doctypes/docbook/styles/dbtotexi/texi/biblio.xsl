<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<!-- FIXME The original file used the dingbats templates, which I replaced
     with plain entities for readability and consistency.  On the other
     hand, for real i18n we will have to put the templates back
     (e.g. to select the language-dependent cite-quote character) ... -->
<!DOCTYPE xsl:stylesheet [
<!ENTITY copy  "&#x00A9;">
<!ENTITY ldquo "&#x201C;">
<!ENTITY rdquo "&#x201D;">
]>

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: biblio.xsl,v 1.6 2000/08/20 03:16:34 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<!-- ==================================================================== -->

<xsl:template match="bibliography">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>


<!-- ==================================================================== -->

<xsl:template match="bibliodiv">
  <xsl:call-template name="texinfo.node" />
  <xsl:call-template name="texinfo.section" />
  <xsl:apply-templates />
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="biblioentry">
  <xsl:call-template name="texinfo.anchor" />
  <para>
    <xsl:apply-templates mode="bibliography.mode"/>
  </para>
</xsl:template>

<xsl:template match="bibliomixed">
  <xsl:call-template name="texinfo.anchor" />
  <para>
    <xsl:apply-templates mode="bibliomixed.mode"/>
  </para>
</xsl:template>


<!-- ==================================================================== -->

<xsl:template match="*" mode="bibliography.mode">
  <xsl:apply-templates select="."/><!-- try the default mode -->
  <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="abbrev" mode="bibliography.mode">
    <xsl:text>[</xsl:text>
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:text>] </xsl:text>
</xsl:template>

<xsl:template match="abstract" mode="bibliography.mode">
  <!-- suppressed -->
</xsl:template>

<xsl:template match="address" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="affiliation" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="shortaffil" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="jobtitle" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="artheader" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="artpagenums" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="author" mode="bibliography.mode">
    <xsl:call-template name="person.name"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="authorblurb" mode="bibliography.mode">
  <!-- suppressed -->
</xsl:template>

<xsl:template match="authorgroup" mode="bibliography.mode">
    <xsl:call-template name="person.name.list"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="authorinitials" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="bibliomisc" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="bibliomset" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<!-- ================================================== -->

<xsl:template match="biblioset" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
</xsl:template>

<xsl:template match="biblioset/title|biblioset/citetitle" 
              mode="bibliography.mode">
  <xsl:variable name="relation" select="../@relation"/>
  <xsl:choose>
    <xsl:when test="$relation='article'">
      <xsl:text>&ldquo;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&rdquo;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <I><xsl:apply-templates/></I>
    </xsl:otherwise>
  </xsl:choose>
  <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<!-- ================================================== -->

<xsl:template match="bookbiblio" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="citetitle" mode="bibliography.mode">
    <I><xsl:apply-templates mode="bibliography.mode"/></I>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="collab" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="collabname" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="confgroup" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="confdates" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="conftitle" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="confnum" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="confsponsor" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="contractnum" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="contractsponsor" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="contrib" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<!-- ================================================== -->

<xsl:template match="copyright" mode="bibliography.mode">
  <xsl:text>&copy; </xsl:text>
  <xsl:apply-templates select="year" mode="bibliography.mode" />
  <xsl:text> </xsl:text>
  <xsl:apply-templates select="holder" mode="bibliography.mode"/>
  <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="year" mode="bibliography.mode">
  <xsl:apply-templates/><xsl:text>, </xsl:text>
</xsl:template>

<xsl:template match="year[position()=last()]" mode="bibliography.mode">
  <xsl:apply-templates/>
</xsl:template>

<xsl:template match="holder" mode="bibliography.mode">
  <xsl:apply-templates/>
</xsl:template>

<!-- ================================================== -->

<xsl:template match="corpauthor" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="corpname" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="date" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="edition" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="editor" mode="bibliography.mode">
    <xsl:call-template name="person.name"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="firstname" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="honorific" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="indexterm" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="invpartnumber" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="isbn" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="issn" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="issuenum" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="lineage" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="orgname" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="orgdiv" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="othercredit" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="othername" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="pagenums" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="printhistory" mode="bibliography.mode">
  <!-- suppressed -->
</xsl:template>

<xsl:template match="productname" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="productnumber" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="pubdate" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="publisher" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
</xsl:template>

<xsl:template match="publishername" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="pubsnumber" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="releaseinfo" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="revhistory" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="seriesinfo" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
</xsl:template>

<xsl:template match="seriesvolnums" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="subtitle" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="surname" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="title" mode="bibliography.mode">
    <i><xsl:apply-templates mode="bibliography.mode"/></i>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="titleabbrev" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<xsl:template match="volumenum" mode="bibliography.mode">
    <xsl:apply-templates mode="bibliography.mode"/>
    <xsl:value-of select="$biblioentry.item.separator"/>
</xsl:template>

<!-- ==================================================================== -->

<xsl:template match="*" mode="bibliomixed.mode">
  <xsl:apply-templates select="."/><!-- try the default mode -->
</xsl:template>

<xsl:template match="abbrev" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="abstract" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="address" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="affiliation" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="artpagenums" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="author" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="authorblurb" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="authorgroup" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="authorinitials" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="bibliomisc" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<!-- ================================================== -->

<xsl:template match="bibliomset" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="bibliomset/title|bibliomset/citetitle" 
              mode="bibliomixed.mode">
  <xsl:variable name="relation" select="../@relation"/>
  <xsl:choose>
    <xsl:when test="$relation='article'">
      <xsl:text>&ldquo;</xsl:text>
      <xsl:apply-templates/>
      <xsl:text>&rdquo;</xsl:text>
    </xsl:when>
    <xsl:otherwise>
      <I><xsl:apply-templates/></I>
    </xsl:otherwise>
  </xsl:choose>
</xsl:template>

<!-- ================================================== -->

<xsl:template match="biblioset" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="citetitle" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="collab" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="confgroup" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="contractnum" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="contractsponsor" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="contrib" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="copyright" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="corpauthor" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="corpname" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="date" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="edition" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="editor" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="firstname" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="honorific" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="indexterm" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="invpartnumber" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="isbn" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="issn" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="issuenum" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="lineage" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="orgname" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="othercredit" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="othername" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="pagenums" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="printhistory" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="productname" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="productnumber" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="pubdate" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="publisher" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="publishername" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="pubsnumber" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="releaseinfo" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="revhistory" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="seriesvolnums" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="subtitle" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="surname" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="title" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="titleabbrev" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<xsl:template match="volumenum" mode="bibliomixed.mode">
    <xsl:apply-templates mode="bibliomixed.mode"/>
</xsl:template>

<!-- ==================================================================== -->

</xsl:stylesheet>
