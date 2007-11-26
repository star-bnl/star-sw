<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet
    version='1.0'
    xmlns:xsl='http://www.w3.org/1999/XSL/Transform'
>

<!-- Print out some header information, then for each parameter
     node print a Comment line, the parameter name = value. -->
<!-- Don't have "variable = " part in yet. -->
<xsl:template match="therminatorParams">
    <xsl:call-template name='therminatorHeader'/>
    <xsl:text>
# Number of events to generate (this is not used within StEStruct framework.)
  NumberOfEvents = 1
 </xsl:text>
    <xsl:for-each select='*'>
        <xsl:text>
#  </xsl:text>
        <xsl:value-of select='@Comment'/>
        <xsl:text>.
  </xsl:text>
        <xsl:value-of select='@typedef'/>
        <xsl:text> = </xsl:text>
            <xsl:value-of select="."/>
    </xsl:for-each>
        <xsl:text>
# InputDirSHARE is derived from jobControl/outputDir
  InputDirSHARE = </xsl:text>
        <xsl:value-of select='//outputDir'/>
        <xsl:text>/StRoot/StEStructPool/Therminator/share
 </xsl:text>
</xsl:template>

<xsl:template name='therminatorHeader'>
#******************************************************************************
#*                      T H E R M I N A T O R                                 *
#*                   THERMal heavy-IoN generATOR                              *
#*                           version 1.0                                      *
#*                                                                            *
#* Authors of the model: Wojciech Broniowski, Wojciech.Broniowski@ifj.edu.pl, *
#*                       Wojciech Florkowski, Wojciech.Florkowski@ifj.edu.pl  *
#* Authors of the code:  Adam Kisiel, kisiel@if.pw.edu.pl                     *
#*                       Tomasz Taluc, ttaluc@if.pw.edu.pl                    *
#* Code designers: Adam Kisiel, Tomasz Taluc, Wojciech Broniowski,            *
#*                 Wojciech Florkowski                                        *
#*                                                                            *
#* For the detailed description of the program and furhter references         * 
#* to the description of the model plesase refer to: nucl-th/0504047,         *
#* accessibile at: http://www.arxiv.org/nucl-th/0504047                       *
#*                                                                            *
#* Homepage: http://hirg.if.pw.edu.pl/en/therminator/                         *
#*                                                                            *
#* This code can be freely used and redistributed. However if you decide to   *
#* make modifications to the code, please contact the authors, especially     *
#* if you plan to publish the results obtained with such modified code.       *
#* Any publication of results obtained using this code must include the       *
#* reference to nucl-th/0504047 and the published version of it, when         *
#* available.                                                                 *
#*                                                                            *
#******************************************************************************
</xsl:template>

<!-- Want to ignore most nodes. -->
<xsl:template match='text()'>
</xsl:template>

</xsl:stylesheet>
