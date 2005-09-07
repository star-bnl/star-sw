<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet
    version='1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
>

<xsl:template match='Pythia'>
    <xsl:call-template name='pythiaHeader'/>
    <xsl:apply-templates/>
</xsl:template>

<!-- Someday we need to send appropriate code to file. -->
<xsl:template match='pythiaMacro'>
    <xsl:apply-templates/>
</xsl:template>

<xsl:template name='pythiaHeader'>
# ********************************************
# *********** Pythia Macro File **************
# ** Nothing here yet. Probably should be  ***
# ********************************************
</xsl:template>

<!-- Want to ignore most nodes. -->
<xsl:template match='text()'>
</xsl:template>

</xsl:stylesheet>
