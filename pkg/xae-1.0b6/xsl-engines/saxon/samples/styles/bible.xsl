<xsl:stylesheet
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0"
 xmlns:saxon="http://icl.com/saxon"
 saxon:trace="no"
 extension-element-prefixes="saxon"
>

<saxon:compile class="Bible"/>
<xsl:strip-space elements="*"/>

<!-- parameter "dir" must be set from the command line: it represents the output directory -->
<xsl:param name="dir">bible</xsl:param>

<xsl:variable name="filesep" select="system-property('file.separator')" />

<xsl:template match="tstmt">
    <saxon:output file="{$dir}{$filesep}index.html" method="html">
        <html><frameset rows="15%,*">
            <noframes>You need a browser that allows frames</noframes>
            <frame src="titlepage.html" />
            <frame src="index2.html" />
        </frameset></html>
    </saxon:output>
    <saxon:output file="{$dir}{$filesep}index2.html" method="html">
        <html><frameset cols="16%,14%,*">
            <noframes>You need a browser that allows frames</noframes>
            <frame src="books.html" />
            <frame src="book1.html" name="chapters" />
            <frame src="chap1.1.html" name="content" />
        </frameset></html>
    </saxon:output>
    <saxon:output file="{$dir}{$filesep}titlepage.html" method="html">
        <html><body bgcolor="#008080" text="#ffffff" link="#00ffff" vlink="#00cccc">
        <font face="sans-serif">
            <xsl:apply-templates/>
        </font>
        <xsl:if test="@preface">
            <a href="preface.html" target="_blank">Preface</a>
        </xsl:if>
        <div align="right"><a href="coverpage.html" target="_blank">Source</a></div>
        </body></html>
    </saxon:output>
</xsl:template>

<xsl:template match="coverpg">
    <saxon:output file="{$dir}{$filesep}coverpage.html" method="html">
        <html><body bgcolor="#00eeee"><center>
            <xsl:apply-templates/>
        <hr/>This HTML Rendition produced from John Bosak's XML source by
             <a href="mailto:Michael.Kay@icl.com">Michael Kay</a>
        <hr/></center>
        </body></html>
    </saxon:output>
</xsl:template>

<xsl:template match="titlepg">
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="title">
    <center><font size="5"><b>
        <xsl:apply-templates/>
    </b></font></center>
</xsl:template>

<xsl:template match="title2">
    <center><font size="2">
        <xsl:apply-templates/>
    <br/></font></center>
</xsl:template>

<xsl:template match="subtitle">
    <xsl:apply-templates/>
</xsl:template>

<xsl:template match="preface">
    <saxon:output file="{$dir}{$filesep}preface.html" method="html">
        <html><body bgcolor="#00eeee"><center>
            <xsl:apply-templates/>
        </center><hr/></body></html>
    </saxon:output>
    <xsl:for-each select="ancestor::tstmt">
        <saxon:set-attribute name="preface">yes</saxon:set-attribute>
    </xsl:for-each>
</xsl:template>

<xsl:template match="ptitle">
    <h2>
        <xsl:apply-templates/>
    </h2>
</xsl:template>

<xsl:template match="ptitle0">
    <p><font size="7" color="red">
        <xsl:apply-templates/>
    </font></p>
</xsl:template>

<xsl:template match="bookcoll">
    <saxon:output file="{$dir}{$filesep}books.html" method="html">
        <html><body bgcolor="#00c0c0">
        <font face="sans-serif" size="2">
        <script language="JavaScript">
        <xsl:comment>
            function bk(n) {
                parent.frames['content'].location="chap" + n + ".1.html";
            }
            //</xsl:comment>
        </script>
        <xsl:apply-templates/>
        </font>
        </body></html>
    </saxon:output>
</xsl:template>

<xsl:template match="book">
    <xsl:variable name="booknr"><xsl:number count="book"/></xsl:variable>
    <saxon:output file="{$dir}{$filesep}book{$booknr}.html" method="html">
        <html><body bgcolor="#00FFFF"><font face="sans-serif" size="2">
            <xsl:apply-templates>
                <xsl:with-param name="booknr" select="$booknr"/>
            </xsl:apply-templates>
        </font></body></html>
    </saxon:output>
    <a href="book{$booknr}.html" target="chapters" onClick="bk({$booknr})">
        <xsl:value-of select="@bktshort"/>
    </a><br/>
</xsl:template>

<xsl:template match="bktlong">
    <xsl:variable name="bktlong">
	    <xsl:apply-templates/>
    </xsl:variable>
    <xsl:for-each select="ancestor::book">
        <saxon:set-attribute name="bktlong" select="$bktlong"/>
    </xsl:for-each>
</xsl:template>

<xsl:template match="bktshort">
    <xsl:variable name="bktshort">
	    <xsl:apply-templates/>
    </xsl:variable>
    <xsl:for-each select="ancestor::book">
        <saxon:set-attribute name="bktshort" select="$bktshort"/>
    </xsl:for-each>
	<h2><xsl:value-of select="$bktshort"/></h2>
</xsl:template>

<xsl:template match="chapter">
    <xsl:param name="booknr"/>
    <xsl:variable name="chapnr"><xsl:number count="chapter"/></xsl:variable>
    <xsl:variable name="chapfile" select="concat('chap', $booknr, '.', $chapnr, '.html')"/> 
    <saxon:output file="{$dir}{$filesep}{$chapfile}" method="html">
        <html><head><title>
        <xsl:value-of select="ancestor::book/@bktshort"/>
        </title></head>

        <body text="#000080"><H2><font face="sans-serif">
        <xsl:value-of select="ancestor::book/@bktlong"/>
        </font></H2>
        <table>
            <xsl:apply-templates/>        
        </table><hr/>
        </body></html>
    </saxon:output>
    <a href="{$chapfile}" target="content">
        <xsl:value-of select="chtitle"/>
    </a><br/>
</xsl:template>

<xsl:template match="chtitle">
    <h2>
	    <xsl:apply-templates/>
	</h2>
</xsl:template>

<xsl:template match="v">
    <xsl:variable name="pos"><xsl:number/></xsl:variable>
    <tr><td valign="top">
    <xsl:choose>
    <xsl:when test="$pos=1">
        <b><xsl:apply-templates/></b>
    </xsl:when>
    <xsl:otherwise>
        <xsl:apply-templates/>
    </xsl:otherwise>
    </xsl:choose>
    </td>
    <td width="10"></td>
    <td valign="top"><font color="#808080">
        <xsl:copy-of select="$pos"/>
    </font></td>
    </tr>
</xsl:template>

<xsl:template match="div">
	<xsl:apply-templates/>
</xsl:template>

<xsl:template match="divtitle">
    <tr><td valign="TOP"><font color="green">
	<xsl:apply-templates/>
    </font></td></tr>
</xsl:template>

</xsl:stylesheet>	
