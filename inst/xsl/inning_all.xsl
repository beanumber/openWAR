<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" encoding="iso-8859-1"/>
<xsl:strip-space elements="*" />

<xsl:template match="//game">
	<xsl:text>inning|ab_num|timestamp|batterId|stand|pitcherId|throws|event|runnerMovement|</xsl:text>
  <xsl:text>&#xA;</xsl:text>
	<xsl:for-each select="inning">
		<xsl:variable name="inning_num"><xsl:value-of select="@num"/></xsl:variable>
		<xsl:for-each select="top">
      <xsl:for-each select="atbat">
	   	  <xsl:copy-of select="$inning_num" /><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@num"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@start_tfs_zulu"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@batter"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@stand"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@pitcher"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@p_throws"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@event"/><xsl:text>|</xsl:text>
		 	  <xsl:for-each select="runner">
		 	   <xsl:text>[</xsl:text>
		 	  	<xsl:value-of select="@id"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@start"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@end"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@score"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@event"/>
		 	  	<xsl:text>]</xsl:text>
		 	  </xsl:for-each>
		 	  <xsl:text>|&#xA;</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
		<xsl:for-each select="bottom">
      <xsl:for-each select="atbat">
	   	  <xsl:copy-of select="$inning_num" /><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@num"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@start_tfs_zulu"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@batter"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@stand"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@pitcher"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@p_throws"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@event"/><xsl:text>|</xsl:text>
		 	  <xsl:for-each select="runner">
		 	   <xsl:text>[</xsl:text>
		 	  	<xsl:value-of select="@id"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@start"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@end"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@score"/><xsl:text>:</xsl:text>
		 	  	<xsl:value-of select="@event"/>
		 	  	<xsl:text>]</xsl:text>
		 	  </xsl:for-each>
		 	  <xsl:text>|&#xA;</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:for-each>
</xsl:template>

<!-- etc -->
</xsl:stylesheet>
