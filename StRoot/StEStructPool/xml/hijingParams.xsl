<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet
    version='1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
>

<!-- Print out the header information, then for each parameter
     node print the Comment then the parameter values.
     Need to add enclosing apostrophes for the comments which
     allows me a way of adding carriage returns (relying on
     the funny indenting of xsl:text blocks.) -->
<xsl:template match="hijingParams">
    <xsl:call-template name='hijingHeader'/>
    <xsl:for-each select='*'>
        <xsl:text>
'</xsl:text>
        <xsl:value-of select='@Comment'/>
        <xsl:text>' </xsl:text>
        <xsl:for-each select='*'>
            <xsl:value-of select="."/>
        </xsl:for-each>
    </xsl:for-each>
</xsl:template>

<!-- Note that I omit the last carriage return on purpose. -->
<xsl:template name='hijingHeader'>
'  ====================================================== '
'  =====         Hijing Control file                ===== '
'  ====================================================== '
' Events                          '   1</xsl:template>

<!-- Want to ignore most nodes. -->
<xsl:template match='text()'>
</xsl:template>

</xsl:stylesheet>
