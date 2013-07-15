<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" encoding="iso-8859-1"/>
<xsl:strip-space elements="*" />

<xsl:template match="//boxscore">
  <xsl:variable name="home_teamId"><xsl:value-of select="@home_id"/></xsl:variable>
	<xsl:variable name="away_teamId"><xsl:value-of select="@away_id"/></xsl:variable>
	<xsl:text>teamId|playerId|playerName|pos|bo</xsl:text>
  <xsl:text>&#xA;</xsl:text>
  <xsl:for-each select="batting">
		<xsl:variable name="team_flag"><xsl:value-of select="@team_flag"/></xsl:variable>
    <xsl:for-each select="batter">
      <xsl:choose>
        <xsl:when test="$team_flag='home'">
          <xsl:value-of select="$home_teamId"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$away_teamId"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>|</xsl:text>
      <xsl:value-of select="@id"/><xsl:text>|</xsl:text>
      <xsl:value-of select="@name"/><xsl:text>|</xsl:text>
      <xsl:value-of select="@pos"/><xsl:text>|</xsl:text>
      <xsl:choose>
        <xsl:when test="@bo &gt; 0">
          <xsl:value-of select="@bo"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:text>NA</xsl:text>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
	</xsl:for-each>
  <xsl:for-each select="pitching">
		<xsl:variable name="team_flag"><xsl:value-of select="@team_flag"/></xsl:variable>
    <xsl:for-each select="pitcher">
      <xsl:choose>
        <xsl:when test="$team_flag='home'">
          <xsl:value-of select="$home_teamId"/>
        </xsl:when>
        <xsl:otherwise>
          <xsl:value-of select="$away_teamId"/>
        </xsl:otherwise>
      </xsl:choose>
      <xsl:text>|</xsl:text>
      <xsl:value-of select="@id"/><xsl:text>|</xsl:text>
      <xsl:value-of select="@name"/><xsl:text>|</xsl:text>
      <xsl:value-of select="@pos"/><xsl:text>|NA|</xsl:text>
      <xsl:text>&#xA;</xsl:text>
    </xsl:for-each>
	</xsl:for-each>
</xsl:template>

<!-- etc -->
</xsl:stylesheet>
