<?xml version="1.0" encoding="utf-8" ?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

   <xsl:output method="xml" indent="yes"/>

   
   <xsl:template match="/">
   <xsl:message>--- filter1 ---&#xa;</xsl:message>
   <filter1>
        <xsl:copy-of select="/"/>
   </filter1>
   </xsl:template>

   
</xsl:stylesheet>
