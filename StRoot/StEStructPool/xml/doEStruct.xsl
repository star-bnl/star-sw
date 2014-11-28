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
<xsl:template match='jobControl|starSubmit|eventCuts|trackCuts|pairCuts|hijingParams|therminatorParams'/>
<xsl:template match='*[@id]'/>

<!-- The rest of this macro is to resolve refersTo, listOf and sumOf etc.-->
<!-- Note that when I figured out I could include attributes, such as typedef, in the output
     I should be able to get rid of most of these InLine things. Leave that for a later cleanup. -->
<xsl:template match='variable'>
    <xsl:if test='@refersTo'>
        <xsl:variable name='reference' select='@refersTo'/>
        <xsl:for-each select='//*[@id=$reference]'><xsl:value-of select='@typedef'/><xsl:value-of select='.'/></xsl:for-each>
    </xsl:if>
    <xsl:if test='@inLine'>
        <xsl:value-of select='@typedef'/>
        <xsl:value-of select='.'/>
        <xsl:variable name='inLine' select='@inLine'/>
        <xsl:for-each select='//*[@id=$inLine]'><xsl:value-of select='@typedef'/><xsl:value-of select='.'/>;
    </xsl:for-each>
    </xsl:if>
    <xsl:if test='@stringInLine'>
        <xsl:variable name='stringInLine' select='@stringInLine'/>
        <xsl:for-each select='//*[@id=$stringInLine]'><xsl:value-of select='@typedef'/>"<xsl:value-of select='.'/>";
    </xsl:for-each>
    </xsl:if>
    <xsl:if test='@listOf'>
        <xsl:variable name='numList' select='@listOf'/>
        <xsl:for-each select='//*[@id=$numList]'><xsl:value-of select='@typedef'/>{<xsl:call-template name='commaList'/>};
    </xsl:for-each>
    </xsl:if>
    <xsl:if test='@sumOf'>
        <xsl:variable name='summand' select='@sumOf'/>
        <xsl:for-each select='//*[@id=$summand]'><xsl:value-of select='@typedef2'/><xsl:call-template name='listLength'/>;
    </xsl:for-each>
    </xsl:if>
    </xsl:template>

<!--Adds commas to the numbered lists in the final macro by calling the template
    'addcommas'. -->
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
<!-- XSLT does it now! [MR] -->
<xsl:template name='listLength'>
    <xsl:call-template name='countLength'>
        <xsl:with-param name='list' select='.'/>
	<xsl:with-param name='length' select='0'/>
    </xsl:call-template>
</xsl:template>
<!-- This template actually sums the whitespace in the list recursively and stores it in the
     variable recursive_result which is then printed in the final macro. [MR]-->
  <xsl:template name='countLength'>
    <xsl:param name='list' select='normalize-space(.)'/>
    <xsl:param name='length'/>
    <xsl:choose>
        <xsl:when test='contains($list," ")'>            
	    <!-- The trick here is that by encapsulating the recursive part with an xsl:variable
	         tag, the xsl:value-of stuff is stored in the variable, rather than being printed. -->           
            <xsl:variable name='recursive_result'>
		<xsl:call-template name='countLength'>
                    <xsl:with-param name='list' select='substring-after($list," ")'/>
		    <xsl:with-param name='length' select='number($length+1)'/>
                </xsl:call-template>
	    </xsl:variable>
            <xsl:value-of select='number(1 + $recursive_result)'/>
        </xsl:when>
        <xsl:otherwise>      
            <xsl:value-of select='1'/>                                
        </xsl:otherwise>
    </xsl:choose> 
  </xsl:template>  

</xsl:stylesheet>
