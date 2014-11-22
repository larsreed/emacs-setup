<xsl:transform
 xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
 version="1.0"
>

<xsl:variable name="my:title" select="'Books with Prices'" xmlns:my="mine.own.uri"/>

<xsl:variable name="header">
    <tr><td>Author</td><td>Title</td><td>Price</td></tr>
</xsl:variable>

<xsl:template match="ITEM">
    <tr>
    <td><xsl:value-of select="AUTHOR"/></td>
    <td><xsl:value-of select="TITLE"/></td>
    <td><xsl:value-of select="PRICE"/></td>
    </tr>
</xsl:template>

</xsl:transform>	
