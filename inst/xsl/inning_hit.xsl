<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" encoding="iso-8859-1"/>
<xsl:strip-space elements="*" />


<xsl:template match="//hitchart">
	<xsl:text>inning|type|batterId|pitcherId|x|y|event</xsl:text>
  <xsl:text>&#xA;</xsl:text>
	<xsl:for-each select="//hip">
	  <xsl:value-of select="@inning"/><xsl:text>|</xsl:text>
	  <xsl:value-of select="@type"/><xsl:text>|</xsl:text>
	  <xsl:value-of select="@batter"/><xsl:text>|</xsl:text>
	  <xsl:value-of select="@pitcher"/><xsl:text>|</xsl:text>
	  <xsl:value-of select="@x"/><xsl:text>|</xsl:text>
	  <xsl:value-of select="@y"/><xsl:text>|</xsl:text>
	  <xsl:value-of select="@des"/>
    <xsl:text>&#xA;</xsl:text>
	</xsl:for-each>
</xsl:template>


<!-- etc -->
</xsl:stylesheet>
