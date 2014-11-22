<?xml version='1.0'?>
<!-- vim: sw=2 sta et
-->

<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'>

<!-- ********************************************************************
     $Id: autotoc.xsl,v 1.5 2000/08/22 01:23:58 stevecheng Exp $
     ********************************************************************

     &copy; 2000 Steve Cheng <steve@ggi-project.org>

     This file is part of the docbook2X XSLT stylesheets for
     converting DocBook to Texinfo.

     Derived from files in Norman Walsh's XSL DocBook Stylesheet
     Distribution and Mark Burton's dbtotexi stylesheets.

     ******************************************************************** -->

<xsl:template name="texinfo.menu">
  <menu>
    
    <!-- Although using apply-templates is probably more efficient,
         we use for-each and do an explicit texinfo.isnode.mode test
         to save grief when more types of nodes are added. -->
    <xsl:for-each select="*">
      <xsl:variable name="isnode">
        <xsl:apply-templates select="." mode="texinfo.isnode.mode" />
      </xsl:variable>

      <xsl:choose>

        <!-- part special case: display the part/title before
             the entries for the components that are enclosed in it. -->
        <xsl:when test="self::part">
          <sp n="1" />
          <xsl:apply-templates select="." mode="title.content" />
        
          <xsl:for-each select="*">
            <xsl:variable name="isnode1"><!-- Saxon heresy! 
                                              Doesn't allow nested bindings like the spec says! -->
              <xsl:apply-templates select="." mode="texinfo.isnode.mode" />
            </xsl:variable>

            <xsl:if test="$isnode1 = '1'">
              <menuitem>
                <xsl:attribute name="node">
                  <xsl:call-template name="texinfo.nodename" />
                </xsl:attribute>
                <xsl:apply-templates select="." mode="title.content" />
              </menuitem>
              <xsl:apply-templates select="." mode="menu.description.mode" />
            </xsl:if>
          </xsl:for-each>

          <sp n="1" />
        </xsl:when>

        <xsl:when test="$isnode = '1'">
          <menuitem>
            <xsl:attribute name="node">
              <xsl:call-template name="texinfo.nodename" />
            </xsl:attribute>
            <xsl:apply-templates select="." mode="title.content" />
          </menuitem>
          <xsl:apply-templates select="." mode="menu.description.mode" />
        </xsl:when>
     
      </xsl:choose>
    </xsl:for-each>

  </menu>
</xsl:template>

</xsl:stylesheet>

