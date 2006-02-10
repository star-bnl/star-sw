<?xml version='1.0' encoding='ISO-8859-1'?>
<xsl:stylesheet
    version='1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
>

<xsl:output method="xml" encoding="UTF-8" indent="no"/>
<xsl:strip-space elements="variable"/>

<!-- variable elements should be substituted by value stored in
     macro element with the same name -->
<!-- I want insert commas between elements of type list or
     count number of elements in the list.
     Other elements I don't want to modify the white space. -->

<!-- If I use default template to avoid writing most text nodes
     I have problems writing 'main' with the variables resolved.
     Instead I match nodes I don't want to output. This is rather
     a pain. Seems to work though. -->
<xsl:template match='pythiaInit|jobControl|starSubmit|eventCuts|trackCuts|pairCuts|hijingParams'/>
<xsl:template match='*[@id]'/>

<!-- The rest of this macro is to resolve refersTo, listOf and sumOf -->
<xsl:template match='variable'>
    <xsl:variable name='reference' select='@refersTo'/>
    <xsl:for-each select='//*[@id=$reference]'><xsl:value-of select='.'/></xsl:for-each>
    <xsl:variable name='intInLine' select='@intInLine'/>
    <xsl:for-each select='//*[@id=$intInLine]'><xsl:value-of select='.'/>;</xsl:for-each>
    <xsl:variable name='boolInLine' select='@boolInLine'/>
    <xsl:for-each select='//*[@id=$boolInLine]'><xsl:value-of select='.'/>;</xsl:for-each>
    <xsl:variable name='stringInLine' select='@stringInLine'/>
    <xsl:for-each select='//*[@id=$stringInLine]'>"<xsl:value-of select='.'/>";</xsl:for-each>
    <xsl:variable name='numList' select='@listOf'/>
    <xsl:for-each select='//*[@id=$numList]'>{<xsl:call-template name='commaList'/>};</xsl:for-each>
    <xsl:variable name='summand' select='@sumOf'/>
    <xsl:for-each select='//*[@id=$summand]'><xsl:call-template name='listLength'/>;</xsl:for-each>
</xsl:template>

<xsl:template name='commaList'>
    <xsl:call-template name='addCommas'>
        <xsl:with-param name='list' select='.'/>
    </xsl:call-template>
</xsl:template>
<xsl:template name='addCommas'>
    <xsl:param name='list'/>
    <xsl:choose>
        <xsl:when test='contains($list," ")'>
            <xsl:value-of select='substring-before($list," ")'/>
            <xsl:text>, </xsl:text>
            <xsl:call-template name='addCommas'>
                <xsl:with-param name='list' select='substring-after($list," ")'/>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select='$list'/>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

<!-- I really should be able to do the addition in xslt, but I can't
     find a reference I can understand, so I make CINT (or c++ or something)
     do the addition for me. -->
<xsl:template name='listLength'>
    <xsl:call-template name='countLength'>
        <xsl:with-param name='list' select='.'/>
    </xsl:call-template>
</xsl:template>
<xsl:template name='countLength'>
    <xsl:param name='list'/>
    <xsl:choose>
        <xsl:when test='contains($list," ")'>
            <xsl:text>1+</xsl:text>
            <xsl:call-template name='countLength'>
                <xsl:with-param name='list'
                                select='substring-after($list," ")'/>
            </xsl:call-template>
        </xsl:when>
        <xsl:otherwise>
            <xsl:text>1</xsl:text>
        </xsl:otherwise>
    </xsl:choose>
</xsl:template>

</xsl:stylesheet>
