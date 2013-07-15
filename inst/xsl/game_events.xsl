<xsl:stylesheet version="1.0"
xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="text" encoding="iso-8859-1"/>
<xsl:strip-space elements="*" />

<xsl:template match="//game">
	<xsl:text>inning|half|ab_num|balls|strikes|endOuts|timestamp|event|actionId|description</xsl:text>
   <xsl:text>&#xA;</xsl:text>
	<xsl:for-each select="inning">
		<xsl:variable name="inning_num"><xsl:value-of select="@num"/></xsl:variable>
		<xsl:for-each select="top">
      <xsl:for-each select="atbat">
	   	  <xsl:copy-of select="$inning_num" /><xsl:text>|</xsl:text>
        <xsl:text>top|</xsl:text>
  	 	  <xsl:value-of select="@num"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@b"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@s"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@o"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@start_tfs_zulu"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@event"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@player"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@des"/><xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
      <xsl:for-each select="action">
	   	  <xsl:copy-of select="$inning_num" /><xsl:text>|</xsl:text>
        <xsl:text>top|</xsl:text>
     	  <xsl:value-of select="@num"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@b"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@s"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@o"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@tfs_zulu"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@event"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@player"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@des"/><xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
		<xsl:for-each select="bottom">
      <xsl:for-each select="atbat">
	   	  <xsl:copy-of select="$inning_num" /><xsl:text>|</xsl:text>
        <xsl:text>bottom|</xsl:text>
     	  <xsl:value-of select="@num"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@b"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@s"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@o"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@start_tfs_zulu"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@event"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@player"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@des"/><xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
      <xsl:for-each select="action">
	   	  <xsl:copy-of select="$inning_num" /><xsl:text>|</xsl:text>
        <xsl:text>bottom|</xsl:text>
     	  <xsl:value-of select="@num"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@b"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@s"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@o"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@tfs_zulu"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@event"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@player"/><xsl:text>|</xsl:text>
		 	  <xsl:value-of select="@des"/><xsl:text>&#xA;</xsl:text>
			</xsl:for-each>
		</xsl:for-each>
	</xsl:for-each>
</xsl:template>

<!-- etc -->
</xsl:stylesheet>
