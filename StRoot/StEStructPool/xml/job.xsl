<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet 
    version="1.0"
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
>

<!-- Obviously there should be a simpler code to do this
     transformation. Maybe someday I will learn about writing xslt code. -->
<!-- This template ignores everything that is not in the starSubmit node.
     It replaces that node with one named job, copying all attributes of
     starSubmit to job.
     Then it outputs every element within starSubmit including all
     attributes. If the attribute is named toURL we insert the
     output directory (which is part of the jobControl element) -->


<xsl:template match='Pythia|jobControl|eventCuts|trackCuts|pairCuts|hijingParams|therminatorParams|doEStructMacro'/>

<xsl:template match='starSubmit'>
    <xsl:element name='job'>
        <xsl:for-each select='@*'>
            <xsl:attribute name='{name()}'>
                <xsl:value-of select='.'/>
            </xsl:attribute>
        </xsl:for-each>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>

<xsl:template match='command'>
    <xsl:element name='{name()}'>
        <xsl:variable name='localDir' select='//localDir'/>
        <xsl:apply-templates/>
    </xsl:element>
</xsl:template>

<xsl:template match='refer[@refersTo]'>
    <xsl:variable name='reference' select='@refersTo'/>
    <xsl:for-each select='//*[@id=$reference]'><xsl:value-of select="."/></xsl:for-each>
</xsl:template>

<xsl:template match='stdout|stderr|stdin|input|output'>
    <xsl:element name='{name()}'>
        <xsl:value-of select='.'/>
        <xsl:apply-templates select='@*'/>
    </xsl:element>
</xsl:template>

<xsl:template match='@singleCopy|@preferStorage|@nFiles'>
    <xsl:copy/>
</xsl:template>

<xsl:template match='@URL'>
    <xsl:copy/>
</xsl:template>

<xsl:template match='@toURL|@URL'>
    <xsl:variable name='outputDir' select='//outputDir'/>
    <xsl:attribute name='{name()}'>
        <xsl:variable name='att' select='.'/>
        <xsl:if test='contains($att,"file:OUTPUTDIR")'>
            <xsl:value-of select='substring-before($att,"file:")'/>
            <xsl:value-of select='string("file:")'/>
            <xsl:value-of select='$outputDir'/>
            <xsl:value-of select='substring-after($att,"file:OUTPUTDIR")'/>
        </xsl:if>
        <xsl:if test='contains($att,"file:")'>
            <xsl:if test='not(contains($att,"file:OUTPUTDIR"))'>
                <xsl:value-of select='$att'/>
            </xsl:if>
        </xsl:if>
        <xsl:if test='contains($att,"filelist:")'>
            <xsl:value-of select='substring-before($att,"filelist:")'/>
            <xsl:value-of select='string("filelist:")'/>
            <xsl:value-of select='$outputDir'/>
            <xsl:value-of select='string("/")'/>
            <xsl:value-of select='substring-after($att,"filelist:")'/>
        </xsl:if>
        <xsl:if test='not(contains($att,"file:"))'>
            <xsl:if test='not(contains($att,"filelist:"))'>
                <xsl:value-of select='$att'/>
            </xsl:if>
        </xsl:if>
    </xsl:attribute>
</xsl:template>

<xsl:template match='@fromScratch'>
    <xsl:variable name='jobName' select='//jobName'/>
    <xsl:variable name='att' select='.'/>
    <xsl:attribute name='{name()}'>
        <xsl:value-of select='$jobName'/>
        <xsl:value-of select='string("/")'/>
        <xsl:value-of select='$att'/>
    </xsl:attribute>
</xsl:template>


</xsl:stylesheet>
