<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
 xmlns:my="mine.own.uri"
>

<xsl:param name="my:title">A list of books</xsl:param>

<xsl:variable name="header">
    <tr><td>Author</td><td>Title</td></tr>
</xsl:variable>

<xsl:template match="/">
    <html>
    <head><title><xsl:value-of select="$my:title"/></title></head>
    <body>
        <table>
        <xsl:copy-of select="$header"/>
        <xsl:apply-templates select="BOOKLIST/BOOKS/ITEM">
        <xsl:sort select="AUTHOR"/>
        </xsl:apply-templates>
        </table>
    </body>
    </html>
</xsl:template>

<xsl:template match="ITEM">
    <tr>
    <td><xsl:value-of select="AUTHOR"/></td>
    <td><xsl:value-of select="TITLE"/></td>
    </tr>
</xsl:template>

</xsl:transform>	
