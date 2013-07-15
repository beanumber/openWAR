<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" encoding="iso-8859-1"/>
<xsl:strip-space elements="*" />

<xsl:template match="//game">
	<xsl:text>game_type|home_team|home_teamId|home_lg|away_team|away_teamId|away_lg|venueId|stadium</xsl:text>
  <xsl:text>&#xA;</xsl:text>
	<xsl:value-of select="@type"/><xsl:text>|</xsl:text>
  <xsl:for-each select="team">
	 	<xsl:value-of select="@code"/><xsl:text>|</xsl:text>
    <xsl:value-of select="@id"/><xsl:text>|</xsl:text>
    <xsl:value-of select="@league"/><xsl:text>|</xsl:text>
	</xsl:for-each>
	<xsl:for-each select="stadium">
	  <xsl:value-of select="@id"/><xsl:text>|</xsl:text>
    <xsl:value-of select="@name"/><xsl:text></xsl:text>
	</xsl:for-each>
  <xsl:text>&#xA;</xsl:text>
</xsl:template>

<!-- etc -->
</xsl:stylesheet>
