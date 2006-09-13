
 package require Tk
 package require tdom
 package require BWidget

namespace eval ::jobCreate:: {
    variable jobFilesCurrent false
    variable jobCreatePath
    variable schemaFileName
    variable schemaInfo
    variable jobInfo
    variable interfaceWindow
    variable jobXmlFile
    variable jobLisFile
    variable jobSelectComboBox
    variable jobSelectButton
    variable jobAnalysisType
    variable showingXML
    variable showingMain
    variable undone
    variable lastNode
    variable numberOfInstances
    variable slaveInterpCount 0
    variable findString ""
}

################################################################################
# Some utility routines.
################################################################################
proc ::jobCreate::applyXsl {xslFile as} {
    set f [open [file join $::jobCreate::jobCreatePath $xslFile]]
    set xsl [read $f]
    close $f
    set domDoc [dom parse $xsl]

    set jR $::jobCreate::jobInfo
    set xslt [$jR xslt $domDoc]
    $domDoc delete
    return [K [$xslt $as] [$xslt delete]]
}
# This lremove was extracted from tkcon because I got used to using the command
# and want it available when not using tkcon.
proc ::jobCreate::lremove {args} {
    array set opts {-all 0 pattern -exact}
    while {[string match -* [lindex $args 0]]} {
	switch -glob -- [lindex $args 0] {
	    -a*	{ set opts(-all) 1 }
	    -g*	{ set opts(pattern) -glob }
	    -r*	{ set opts(pattern) -regexp }
	    --	{ set args [lreplace $args 0 0]; break }
	    default {return -code error "unknown option \"[lindex $args 0]\""}
	}
	set args [lreplace $args 0 0]
    }
    set l [lindex $args 0]
    foreach i [join [lreplace $args 0 0]] {
	if {[set ix [lsearch $opts(pattern) $l $i]] == -1} continue
	set l [lreplace $l $ix $ix]
	if {$opts(-all)} {
	    while {[set ix [lsearch $opts(pattern) $l $i]] != -1} {
		set l [lreplace $l $ix $ix]
	    }
	}
    }
    return $l
}

################################################################################
# Read and parse schema file.
# Will use schema to check validity of xml and also as all possible
# values displayed in the gui.
# An xml file will be read containing the actual values to be used for a job.
################################################################################
proc ::jobCreate::getSchemaInfo {fileName} {
    if {[info exists ::jobCreate::schemaInfo]} {
        $::jobCreate::schemaInfo delete
    }
    set f [open $fileName]
    set scheme [dom parse [read $f]]
    close $f

    set ::jobCreate::schemaInfo [$scheme documentElement]
    set ::jobCreate::schemaFileName $fileName
}

################################################################################
# Read and parse job specific xml file.
# The code in this file directly manipulates the dom.
# Much of the complication of this routine is to give possibility of
# merging separate xml files into one document.
################################################################################
proc ::jobCreate::parseJobInfo {fileName} {
    if {[info exists domInfo]} {
        $domInfo delete 
    }
    if {[file extension $fileName] eq ".xml"} {
        set fh [open $fileName]
        set domInfo [dom parse [read $fh]]
        close $fh
        set ::jobCreate::jobInfo [$domInfo documentElement]
    } elseif {[file extension $fileName] eq ".lis"} {
        set fh [open $fileName]
        set dir [file dir $fileName]
        set fDom [open [file join $dir [gets $fh]]]
        set domInfo [dom parse [read $fDom]]
        close $fDom
        set ::jobCreate::jobInfo [$domInfo documentElement]
        while {[gets $fh mFile] >= 0} {
            if {$mFile eq "StEStructCorrelation.xml"} {
                set ::jobCreate::jobAnalysisType "StEStructCorrelation"
            } elseif {$mFile eq "StEStructFluctuation.xml"} {
                set ::jobCreate::jobAnalysisType "StEStructFluctuation"
            } elseif {$mFile eq "StEStructEmpty.xml"} {
                set ::jobCreate::jobAnalysisType "StEStructEmpty"
            }
            set mDom [open [file join $dir $mFile]]
            set mDomInfo [dom parse [read $mDom]]
            close $mDom
            set doc2 [$mDomInfo documentElement]
            ::jobCreate::merge $doc2 $::jobCreate::jobInfo
        }
        # Need to validate the merged jobInfo
        set doc [$::jobCreate::jobInfo asList]
        foreach {name atts -} $doc break
        readSchema $::jobCreate::schemaFileName
        is-a $name $doc
    }
    set jc [$::jobCreate::jobInfo selectNodes //jobControl]
    resolveEnv $jc
}
################################################################################
# Wanted the merge to be done using xslt.
# I can't figure out how (or more accurately I just can't get it to work.)
#
# If node n2 does not exist in doc1 we append it to the corresponding
# parent of doc1. The problem with this is that the schema may require
# nodes in a particular order
#
# If n2 does exist in doc1 we check if it is of type eventCut or trackCut.
# If not we over-write the doc1 version. Attributes not given in n2 but
# present in doc1 are unchanged.
# If n2 is an eventCut or trackCut we look for a node in doc1 with the
# same eventCutName (trackCutName). If this is not found append n2 to doc1.
# If it is found then replace the doc1 version.
#
# Ignore comment nodes in n2. (With our current logic it is likely
# that comments will end up in locations unrelated to what they are
# comments on.)
################################################################################
proc ::jobCreate::merge {n2 doc1} {
    if {[$n2 nodeType] eq "COMMENT_NODE"} {
        return
    }
    set p  [$n2 toXPath]
    set n1 [$doc1 selectNodes $p]
    if {$n1 eq ""} {
        # n2 not found in doc1. The asXML command will include all children
        # so we do not need to recurse for this node.
        set par2 [$n2 parentNode]
        set p [$par2 toXPath]
        set n1 [$doc1 selectNodes $p]
        set x [$n2 asXML]
        $n1 appendXML $x
        return
    }
    if {[$n2 nodeName] eq "eventCut" ||
        [$n2 nodeName] eq "trackCut"} {
        # There can be multiple cut nodes. Need to check cutName to
        # see if they are the same or different.
        mergeCuts $n2 $doc1
        return
    } else {
        # Copy n2 nodeValue and attributes to doc1.
        # Need to recurse to merge children of n2.
        if {[$n2 nodeValue] ne ""} {
            $n1 nodeValue "[$n2 nodeValue]"
        }
        foreach att [$n2 attributes] {
            $n1 setAttribute $att [$n2 getAttribute $att]
        }
        foreach c [$n2 childNodes] {
            merge $c $doc1
        }
    }
}
proc ::jobCreate::mergeCuts {n2 doc1} {
    set nc  [$n2 selectNodes ./cutName]
    set cut [$nc text]
    set n1  [$doc1 selectNodes //*\[cutName='$cut'\]]
    if {$n1 eq ""} {
        set par2 [$n2 parentNode]
        set p    [$par2 toXPath]
        set n1   [$doc1 selectNodes $p]
        set x    [$n2 asXML]
        $n1 appendXML $x
    } else {
        foreach v1 [$n1 selectNodes ./Value] v2 [$n2 selectNodes ./Value] {
            $v1 nodeValue [$v2 nodeValue]
        }
    }
}
################################################################################
# I find it convenient to use environment variables in the jobControl tree.
# This routine does a subst on each nodeValue in jobControl and stores
# the result back in the tree.
################################################################################
proc ::jobCreate::resolveEnv {n} {
    global env

    set v [$n nodeValue]
    if {$v ne ""} {
        if {![catch {subst $v} v2]} {
            $n nodeValue $v2
        }
    }
    foreach c [$n childNodes] {
        resolveEnv $c
    }
}

################################################################################
# Not sure it really makes sense to switch to a new schema.
# At the moment I leak variables, but other than that I think this is
# essentially a restart.
# The right way to do this is to encapsulate all variables (and probably
# procs) in a namespace wich can be destroyed and re-created.
################################################################################
proc ::jobCreate::getNewSchema { {fileName ""} } {
    if {$fileName eq ""} {
        set fileName [tk_getOpenFile -filetypes {{schema {.xsd}} {any {.*}}}]
    }
    if {$fileName ne ""} {
        getSchemaInfo $fileName
        if {[info exists ::jobCreate::interfaceWindow]} {
            if {[winfo exists $::jobCreate::interfaceWindow.all]} {
                destroy $::jobCreate::interfaceWindow.all
            }
        }
        createWindow
    }
}

################################################################################
# Creation of main window
################################################################################
proc ::jobCreate::createWindow {} {
    wm withdraw .
    if {[info exists ::jobCreate::interfaceWindow]} {
        destroy $::jobCreate::interfaceWindow
    }
    set  ::jobCreate::interfaceWindow [toplevel .interfaceWindow -width 600 -height 600]
    wm title $::jobCreate::interfaceWindow "A Dynamic Dispatch System"

    set m [menu $::jobCreate::interfaceWindow.menu]
    $::jobCreate::interfaceWindow configure -menu $m
    set file [menu $m.file]

    set sw  [ScrolledWindow $::jobCreate::interfaceWindow.sw -relief sunken -borderwidth 2]
    set sff [ScrollableFrame $::jobCreate::interfaceWindow.sw.f]
    $sw setwidget $sff
    set sf  [$sff getframe]
    set ::jobCreate::scrollableFrame $sf
    pack $sw -fill both -expand true

    $m add cascade -label File -menu $file
    $file add command -label "New Schema File..."      -command [namespace code getNewSchema]
    $file add command -label "Read Job Description..." -command [namespace code [list getJobXmlFile]]
    $file add command -label "Create Job Files"        -command [namespace code createJobFiles]
    $file add command -label "Submit Job"              -command [namespace code submitJob]
    $file add command -label "Start jobMonitor"        -command [namespace code startJobMonitor]
    $file add command -label Exit -command [namespace code exit]
    set edit [menu $m.edit]
    $m add cascade -label Edit -menu $edit
    $edit add command -label PlaceHolder
    set post [menu $m.post -postcommand [namespace code [list checkAnalysisType $m.post]]]
    $m add cascade -label Post -menu $post
    $post add command -label "Add histograms" -command [namespace code addHistograms]
    $post add command -label "Run selectAll"  -command [namespace code runSelectAll]
    set help [menu $m.help]
    $m add cascade -label Help -menu $help
    $help add command -label Help -command [namespace code [list displayHelp ""]]


    +JobDescriptionFrame $sf
}
################################################################################
# Creates buttons for choices offered in jobDescription
################################################################################
proc ::jobCreate::+JobDescriptionFrame {t} {
    set f [frame $t.jobDescription]
    pack $f -fill x
    foreach jNode [$::jobCreate::schemaInfo selectNodes //*\[@name='jobType'\]//xs:element\[@name\]] {
        set type [$jNode getAttribute name]
        button $f.b$type -text $type -command [namespace code [list +listDefaultXml $type $t $t.f.cSelectJobXml $t.f.bSelectJobXml]]
        set cNode [$jNode selectNodes ./*/*\[@Comment\]]
        set comm [$cNode getAttribute Comment]
        DynamicHelp::register $f.b$type balloon $comm
        pack $f.b$type -side left -anchor nw
    }

    set f [frame $t.f]
    pack $f -fill x
    set ::jobCreate::jobLisFile ""
    set ::jobCreate::jobSelectComboBox $f.cSelectJobXml
    ComboBox::create $f.cSelectJobXml \
            -editable  false          \
            -state     disabled       \
            -textvariable ::jobCreate::jobLisFile  \
            -width     30             \
            -modifycmd "$f.bSelectJobXml configure -state normal"
    set ::jobCreate::jobSelectButton $f.bSelectJobXml
    button $f.bSelectJobXml -pady 1 \
            -text "Use selection"   \
            -state disabled         \
            -command [namespace code "+Type $t \[file join $::jobCreate::jobCreatePath jobFiles \[$f.cSelectJobXml get\]\]"]
    pack $f.cSelectJobXml $f.bSelectJobXml -side left -anchor nw
}
################################################################################
# Extract list of default xml job description files for selected type of job.
################################################################################
proc ::jobCreate::+listDefaultXml {type t c b} {
    set vals [list]
    switch $type {
        Data {
            set vals [list dataPP200_MinBias.lis \
                           data_ppProductionMinBias_year5.lis \
                           dataAuAu19_2001_MinBias.lis \
                           dataAuAu62_2004_MinBias.lis \
                           dataAuAu130_2000_MinBias.lis \
                           dataAuAu200_2001_MinBiasVertex.lis \
                           dataAuAu200_2001_ProductionMinBias.lis \
                           dataAuAu200_2004_MinBias.lis \
                           dataCuCu62_2005_ProductionMinBias.lis \
                           dataCuCu200_2005_ProductionMinBias.lis]
        }
        GEANT {
            set vals [list GEANTAuAu200_b_0_3.lis \
                           GEANTPP200_minbias.lis \
                           dataCuCu62_2005_ProductionMinBias_nfc.lis \
                           dataCuCu200_2005_ProductionMinBias_nfc.lis]
        }
        Hijing {
            set vals [list hijing19GeVAuAuQuenchOn.lis   \
                           hijing19GeVAuAuQuenchOff.lis  \
                           hijing19GeVAuAuJetOff.lis     \
                           hijing62GeVAuAuQuenchOn.lis   \
                           hijing62GeVAuAuQuenchOff.lis  \
                           hijing62GeVAuAuJetOff.lis     \
                           hijing200GeVAuAuQuenchOn.lis  \
                           hijing200GeVAuAuQuenchOff.lis \
                           hijing200GeVAuAuJetOff.lis]
        }
        Pythia {
            set vals [list flat.lis \
                           pythia200GeVPP.lis \
                           pythia2.lis]
        }
    }
    set ::jobCreate::jobLisFile ""
    $b configure -state disabled
    $c configure -values $vals
    $c configure -state normal -text ">>Make, then Use Selection<<"
    if {[winfo exists $t.job]} {
        destroy $t.job
    }
}
################################################################################
# Create and fill frames for all elements in type of element chosen
# from jobDescription.
################################################################################
proc ::jobCreate::+Type {t file} {
    parseJobInfo $file
    if {[winfo exists $t.job]} {
        destroy $t.job
    }
    set type [[[$::jobCreate::jobInfo selectNode //jobType] childNodes] nodeName]
    set node [$::jobCreate::schemaInfo selectNodes //*\[@name='$type'\]]
    set nList [$node selectNodes .//*\[@ref\]]
    set node [$::jobCreate::schemaInfo selectNodes //*\[@name='standardElements'\]]
    lappend nList [$node selectNodes .//*\[@ref\]]

    set j [frame $t.job]
    pack $j -fill x

    foreach eNode [join $nList] {
        set el [$eNode getAttribute ref]
        labelframe $j.$el -text $el
        pack $j.$el -fill x
        ArrowButton $j.$el.ab -dir right -command [namespace code "toggleDisplay $j.$el"]
        pack $j.$el.ab -side left -fill x -anchor nw
        frame $j.$el.f
        +$el $j.$el.f
    }
}
################################################################################
# Get and validate job xml file.
################################################################################
proc ::jobCreate::getJobXmlFile {{fileName ""}} {
    if {$fileName eq ""} {
        set fileName [tk_getOpenFile -filetypes {{schema {.xml}} {any {.*}}}]
    }
    if {$fileName ne ""} {
        if {[catch {validateFile $fileName $::jobCreate::schemaFileName}]} {
            set message "The file, $fileName, did not pass schema validation. \
                         You really should fix something, \
                         but should we continue for now anyway?"
            set cont [tk_messageBox -message $message -type yesno \
                    -icon question -title "Bad Job XML file"]
            if {!$cont} {
                return
            }
        }
        $::jobCreate::jobSelectComboBox configure -values "" -state disabled
        set ::jobCreate::jobXmlFile $fileName
        set ::jobCreate::jobLisFile ""
        +Type $::jobCreate::scrollableFrame $fileName
    }
}
################################################################################
# Open and close frames (actually pack and forget)
################################################################################
proc ::jobCreate::toggleDisplay {f} {
    switch [$f.ab cget -dir] {
        right  {$f.ab configure -dir bottom
                pack $f.f -in $f
                }
        bottom {$f.ab configure -dir right
                pack forget $f.f
               }
    }
}

################################################################################
# Next eight +XXX procs actually fill widgets for elements from jobDescription
# into the appropriate frame. Most of these
################################################################################
# 1 ############################################################################
proc ::jobCreate::+jobControl {f} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName jobControl]
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='jobControl'\]]
    foreach sN [$sNode getElementsByTagName xs:element] {
        set name [$sN getAttribute name]
        set cNode [$sN selectNodes .//*\[@Comment\]]
        set comment [$cNode getAttribute Comment]
        set node [$jNode getElementsByTagName $name]
        if {$name ne "jobPurpose"} {
            label $f.$name -text $name
            variable code${f}$name {}
            set code${f}$name [$node text]
            entry $f.${name}Code                \
                    -textvariable ::jobCreate::code${f}$name \
                    -width 40                   \
                    -validate all               \
                    -vcmd [namespace code "modifyJobControlNode $node %P %W %V"]
        } else {
            label  $f.$name -text $name
            text $f.${name}Code -height 6 -width 60
            $f.${name}Code insert 1.0 [$node text]
            bind $f.${name}Code <KeyPress> [namespace code "modifyJobPurpose $node %W %V"]
        }
        ::DynamicHelp::register $f.$name balloon $comment
        grid $f.$name $f.${name}Code -sticky nw
    }
}
# 2 ############################################################################
proc ::jobCreate::+starSubmit {f} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName starSubmit]
    button $f.jobCmd -text "Show <job>" \
            -command [namespace code "displayCode $jNode {}
                      wm deiconify .fileText
                      raise .fileText"]
    pack $f.jobCmd -anchor w

    set jobAttFrame [labelframe $f.job -text "job Attributes"]
    pack $jobAttFrame
    set atts [$jNode attributes]
    m+XmlAtts $jobAttFrame starSubmit $jNode label
    foreach att $atts {
        set val [$jNode getAttribute $att]
        insertAttributeRow $jobAttFrame starSubmit $att $val $jNode
    }

    set jobElFrame [labelframe $f.el -text "job Elements"]
    pack $jobElFrame
    m+XmlElements $jobElFrame
    foreach n [$jNode childNodes] {
        set el [$n nodeName]
        if {$el eq "command"} {
            continue
        }
        insertElementRow $jobElFrame $el $n
    }
}
# 3 ############################################################################
proc ::jobCreate::+eventCuts {f} {
    i+Row $f eventCut
}
# 4 ############################################################################
proc ::jobCreate::+trackCuts {f} {
    i+Row $f trackCut
}
# 5 ############################################################################
proc ::jobCreate::+pairCuts {f} {
    i+Row $f pairCut
}
# 6 ############################################################################
proc ::jobCreate::+doEStructMacro {f} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName doEStructMacro]

    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='doEStructMacro'\]]
    foreach sN [$sNode getElementsByTagName xs:element] {
        set name [$sN getAttribute ref]
        if {[lsearch [$sN attributes] minOccurs] >= 0} {
            set min  [$sN getAttribute minOccurs]
        } else {
            set min 1
        }
        set sN [$::jobCreate::schemaInfo selectNodes //*\[@name='$name'\]]
        set cNode [$sN selectNodes .//*\[@name='Comment'\]]
        set comment [$cNode text]
        set wNode [$sN selectNodes .//*\[@name='widget'\]]

        set node [$jNode getElementsByTagName $name]
        if {$node eq "" && $min == 0} {
            continue
        }
        if {[lsearch [$node attributes] widget] >= 0} {
            set wType [$node getAttribute widget]
        }
        if {$wType eq "combobox" && $name ne "analysisType"} {
            m+MacroCombo $f $name $node
        } elseif {$wType eq "combobox" && $::jobCreate::jobLisFile ne ""} {
            # Only allow analysis type choice when encountering combobox in doEStruct?????
            m+AnalysisType $f $name [$node text]
        } elseif {$wType eq "entry"} {
            label $f.$name -text $name
            variable code${f}$name [$node text]
            entry $f.${name}Code                \
                    -textvariable ::jobCreate::code${f}$name \
                    -width 40                   \
                    -validate all               \
                    -vcmd [namespace code "modifyMacroNode $node %P %W %V"]
            grid $f.$name $f.${name}Code -sticky w
        } elseif {$wType eq "text"} {
            label  $f.$name -text $name
            if {$name eq "main"} {
                set bl "show"
            } else {
                set bl "edit"
            }
            button $f.${name}Code -text $bl -pady 1 \
                    -command [namespace code "displayCode $node {Part of doEStruct.C}
                              wm deiconify .fileText
                              raise .fileText"]
            grid $f.$name $f.${name}Code -sticky w
        } else {
            continue
        }
        ::DynamicHelp::register $f.$name balloon $comment
    }
}
# 7 ############################################################################
# See comment for modifyHijingParam to remember why string trim.
proc ::jobCreate::+hijingParams {f} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName hijingParams]
    foreach node [$jNode childNodes] {
        set cut [$node nodeName]
        label $f.name$cut -text "'[$node getAttribute Comment] '"
        set widgets [list $f.name$cut]
        set i 0
        foreach vNode [$node childNodes] {
            entry $f.e${i}$cut -width 10 \
                    -validate all -vcmd [namespace code "modifyHijingParam $vNode %P %W %V"]
            $f.e${i}$cut insert 0 [string trim [$vNode text]]
            lappend widgets $f.e${i}$cut
            incr i
        }
        eval grid $widgets -sticky w
    }
}
# 8 ############################################################################
proc ::jobCreate::+pythiaInit {f} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName pythiaInit]

    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='pythiaInit'\]]
    foreach sN [$sNode getElementsByTagName xs:element] {
        set name [$sN getAttribute ref]
        set sN [$::jobCreate::schemaInfo selectNodes //*\[@name='$name'\]]
        set cNode [$sN selectNodes .//*\[@name='Comment'\]]
        set comment [$cNode text]
        set wNode [$sN selectNodes .//*\[@name='widget'\]]

        set node [$jNode getElementsByTagName $name]
        if {[lsearch [$node attributes] widget] >= 0} {
            set wType [$node getAttribute widget]
        }
        if {$wType eq "combobox"} {
            m+PythiaParameter $f $name $node
        } elseif {$wType eq "entry"} {
            label $f.$name -text $name
            variable code${f}$name [$node text]
            entry $f.${name}Code                \
                    -textvariable ::jobCreate::code${f}$name \
                    -width 40                   \
                    -validate all               \
                    -vcmd [namespace code "modifyPythiaParameter $node %P %W %V"]
            grid $f.$name $f.${name}Code -sticky w
        } else {
            continue
        }
        ::DynamicHelp::register $f.$name balloon $comment
    }
}
################################################################################
# Adapted from m+AnalysisType.
# Give user choice of any of the enumerated types.
################################################################################
proc ::jobCreate::m+PythiaParameter {f el node} {
    set val [$node text]
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$el'\]]
    set sR    [$sNode getElementsByTagName xs:restriction]
    set type  [$sR getAttribute base]
    set sR2   [$::jobCreate::schemaInfo selectNodes //*\[@name='$type'\]]
    set vals [list]
    foreach sN [$sR2 getElementsByTagName xs:enumeration] {
        lappend vals [$sN getAttribute value]
    }

    label $f.l$el -text $el
    variable cVal${f}$el $val
    ComboBox::create $f.combo$el \
            -editable     false  \
            -values       $vals  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   [namespace code "modifyPythiaParameter $node \[$f.combo$el get\] $f.combo$el %V"]
    grid $f.l$el $f.combo$el -sticky w
}

################################################################################
# Adapted from m+AnalysisType.
# Give user choice of any of the enumerated types in doEStructMacro section.
################################################################################
proc ::jobCreate::m+MacroCombo {f el node} {
    set val [$node text]
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$el'\]]
    set sR    [$sNode getElementsByTagName xs:restriction]
    set type  [$sR getAttribute base]
    set sR2   [$::jobCreate::schemaInfo selectNodes //*\[@name='$type'\]]
    set vals [list]
    foreach sN [$sR2 getElementsByTagName xs:enumeration] {
        lappend vals [$sN getAttribute value]
    }

    label $f.l$el -text $el
    variable cVal${f}$el $val
    ComboBox::create $f.combo$el \
            -editable     false  \
            -values       $vals  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   [namespace code "modifyMacroNode $node \[$f.combo$el get\] $f.combo$el %V"]
    grid $f.l$el $f.combo$el -sticky w
}
################################################################################
# Adapted from m+XmlAtts.
# Want to give user the choice between analysis types (allowable are defined
# in the schema) with the effect that a file for that analysis type is read,
# merged with current dom tree and ::jobCreate::+doEStructMacro is
# invoked again.
################################################################################
proc ::jobCreate::m+AnalysisType {f el val} {
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$el'\]]
    set sR    [$sNode getElementsByTagName xs:restriction]
    set type  [$sR getAttribute base]
    set sR2   [$::jobCreate::schemaInfo selectNodes //*\[@name='$type'\]]
    set vals [list]
    foreach sN [$sR2 getElementsByTagName xs:enumeration] {
        lappend vals [$sN getAttribute value]
    }

    label $f.l$el -text $el
    variable cVal${f}$el $val
    ComboBox::create $f.combo$el \
            -editable     false  \
            -values       $vals  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   [namespace code "setAnalysisType $f \[$f.combo$el get\]"]
    grid $f.l$el $f.combo$el -sticky w
}
################################################################################
# Fluctuations and Correlations have some independent elements.
# For now we only allow switching between fluctuations, correlations and empty if we
# have defined the job through a .lis file. Strategy is to delete current doEStruct
# node and recreate it by merging from files in .lis file, only replacing
# StEStructCorrelation.xml, StEStructFluctuation.xml or StEStructEmpty.xml
# with $type.xml
# Not really a satisfying way of doing it.
################################################################################
proc ::jobCreate::setAnalysisType {f type} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName doEStructMacro]
    $jNode delete
    set ::jobCreate::jobAnalysisType $type
    set lisFile [file join $::jobCreate::jobCreatePath jobFiles $::jobCreate::jobLisFile]
    set lfh [open $lisFile]
    while {[gets $lfh xmlFile] >= 0} {
        if {$xmlFile eq "StEStructCorrelation.xml" ||
            $xmlFile eq "StEStructFluctuation.xml" ||
            $xmlFile eq "StEStructEmpty.xml"} {
            set xmlFile $::jobCreate::jobAnalysisType.xml
        }
        set mDom [open [file join $::jobCreate::jobCreatePath jobFiles $xmlFile]]
        set mDomInfo [dom parse [read $mDom]]
        close $mDom
        set doc2 [$mDomInfo getElementsByTagName doEStructMacro]
        if {$doc2 ne ""} {
            ::jobCreate::merge $doc2 $::jobCreate::jobInfo
        }
    }
    # Need to validate the merged jobInfo
    set doc [$::jobCreate::jobInfo asList]
    foreach {name atts -} $doc break
    readSchema $::jobCreate::schemaFileName
    is-a $name $doc

    after 100 [namespace code "recreateDoEStrcutMacro $f"]
}
################################################################################
proc ::jobCreate::recreateDoEStrcutMacro {f} {
    # Re-create all the doEStructMacro widgets.
    foreach c [winfo children $f] {
        destroy $c
    }
    +doEStructMacro $f
}
################################################################################
# When entry is modified we want to update the dom.
# This routine checks that the new value is valid.
################################################################################
proc ::jobCreate::modifyNode {node val e cond} {
    # Sometimes we are passed a non-existant node??
    # Probably a problem somewhere else but for now ignore it.
    if {[info command $node] ne $node} {
        return true
    }
    set nChild [$node childNodes]
    set curr [$nChild nodeValue]
    $nChild nodeValue $val
    if {[catch {is-a [$node nodeName] [$node asList]} err]} {
        $nChild nodeValue $curr
        $e configure -foreground red
        return true
    }
    $e configure -foreground black
    if {$cond ne "focusin" && $cond ne "focusout"} {
        set ::jobCreate::jobFilesCurrent false
    }
    return true
}
################################################################################
# modifyJobPurpose is similar to modifyNode.
# Extract text from a text widget and store into node.
# Any text is valid, so we don't check with schema.
# This is done on every key press, so we don't want to do too much.
################################################################################
proc ::jobCreate::modifyJobPurpose {node e cond} {
    set val [$e get 1.0 end-1c]
    set nChild [$node childNodes]
    $nChild nodeValue $val

    if {$cond ne "focusin" && $cond ne "focusout"} {
        set ::jobCreate::jobFilesCurrent false
    }
    return true
}

################################################################################
# i+Row is used to add the combobox for trackCuts eventCuts and pairCuts.
#  This adds all possible values found in the schema.
#  Some of these are then removed when we create rows for cuts
#  found in the job xml file.
################################################################################
proc ::jobCreate::i+Row {f type} {
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='${type}Names'\]]
    set vals [list]
    if {$type eq "eventCut"} {
        set vals [list triggerTag]
    }
    foreach sN [$sNode getElementsByTagName xs:enumeration] {
        lappend vals [$sN getAttribute value]
    }
        
    set jNode [$::jobCreate::jobInfo getElementsByTagName ${type}s]

    # Create combobox to hold all possible cuts.
    label $f.addLabel -text "Add Cut"
    variable cVal$f {}
    ComboBox::create $f.combo    \
            -editable     false  \
            -textvariable ::jobCreate::cVal$f \
            -values       $vals  \
            -modifycmd   "$f.addButton configure -state normal"
    button $f.addButton -text "Add New Cut" \
            -pady 0 -padx 0 -bd 2           \
            -state disabled                 \
            -command [namespace code "[list iRow $f $type]
                      $f.addButton configure -state disabled
                      set cVal$f {}"]
    grid $f.addLabel - $f.combo - $f.addButton - -sticky w

    # Now create editing rows for each cut in the jobXml file.
    if {$jNode eq ""} {
        return
    }
    set node [$jNode getElementsByTagName triggerTag]
    if {$node ne ""} {
        +Row $f $type triggerTag
    }
    foreach node [$jNode getElementsByTagName cutName] {
        +Row $f $type [$node text]
    }
}
################################################################################
# iRow is only invoked by a button when combobox has
# a valid choice selected. Need to use schema information to create
# appropriate node in dom. Use +Row to display.
# This proc is only used for eventCuts and trackCuts.
################################################################################
proc ::jobCreate::iRow {f type} {
    set cut [$f.combo get]
    if {$cut eq "triggerTag"} {
        i+TriggerRow $f
        return
    }

    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='${type}Names'\]]
    set cNode [$sNode selectNodes .//*\[@value='$cut'\]/xs:Comment]
    set xText "
        <$type Comment = '[$cNode text]'>
            <cutName>$cut</cutName>
            <Value widget = 'entry'>0</Value>
            <Value widget = 'entry'>0</Value>
        </$type>
    "

    set jNode [$::jobCreate::jobInfo getElementsByTagName ${type}s]
    if {$jNode eq ""} {
        set sText "
            <${type}s>
            </${type}s>
        "
        set sNode [$::jobCreate::jobInfo getElementsByTagName standardElements]
        $sNode appendXML $sText
        set jNode [$::jobCreate::jobInfo getElementsByTagName ${type}s]
    }
    $jNode appendXML $xText
    +Row $f $type $cut
}
################################################################################
# +Row is used for event and track cuts.
# Remove option from combobox, create row with widgets to display cut values.
################################################################################
proc ::jobCreate::+Row {f type cut} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName ${type}s]
    set node [$jNode selectNodes .//*\[cutName='$cut'\]]

    set possible [$f.combo cget -values]
    set ind [lsearch $possible $cut]
    if {$ind < 0} {
        error "Tried adding cut $cut. Not in list of possible cuts."
    }
    set i1 [expr $ind-1]
    set i2 [expr $ind+1]
    set new "[lrange $possible 0 $i1] [lrange $possible $i2 end]"
    $f.combo configure -values $new
    if {$cut eq "triggerTag"} {
        +TriggerRow $f [$jNode getElementsByTagName $cut]
        return
    }

    button $f.del$cut -text "x" -fg red -pady 0 -padx 0 -bd 1 \
            -command [namespace code "-Row $f $cut $node"]
    label $f.name$cut -text $cut
    set i 1
    foreach n [$node getElementsByTagName Value] {
        entry $f.e$i$cut -width 10 -validate all \
                -vcmd [namespace code "modifyNode $n %P %W %V"]
        $f.e$i$cut insert 0 [$n text]
        incr i
    }
    label $f.comment$cut -text "# [$node getAttribute Comment]"
    grid $f.del$cut $f.name$cut $f.e1$cut $f.e2$cut $f.comment$cut -sticky w
}
################################################################################
# -Row is used to remove an eventCut or a trackCut.
# Removes the row of widgets and also removes the node from the dom
################################################################################
proc ::jobCreate::-Row {f cut node} {
    set current [$f.combo cget -values]
    if {[lsearch $current $cut] >= 0} {
        error "Tried removing cut $cut. Already listed as removed"
    }
    if {$cut eq "triggerTag"} {
        lappend current $cut
        $f.combo configure -values $current
        destroy $f.del$cut $f.name$cut $f.combo$cut $f.comment$cut
    } else {
        set comment [$f.comment$cut cget -text]
        lappend current $cut
        $f.combo configure -values $current
        destroy $f.del$cut $f.name$cut $f.e1$cut $f.e2$cut \
                $f.hash$cut $f.comment$cut
    }
    $node delete
}
################################################################################
# i+TriggerRow inserts a triggerTag into the dom
################################################################################
proc ::jobCreate::i+TriggerRow {f} {
    set cut triggerTag
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$cut'\]]
    set vNode [lindex [$sNode selectNodes .//*\[@value\]] 0]
    set trig [$vNode getAttribute value]

    set jNode [$::jobCreate::jobInfo getElementsByTagName eventCuts]
    set xText "<triggerTag Comment='trigger period selection'>$trig</triggerTag>"
    $jNode appendXML $xText
    +Row $f eventCut $cut
}
################################################################################
# +TriggerRow inserts a trigger tag into the dom and 
################################################################################
proc ::jobCreate::+TriggerRow {f node} {
    set cut triggerTag
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='triggerTag'\]]
    set opts [list]
    foreach tN [$sNode selectNodes .//*\[@value\]] {
        lappend opts [$tN getAttribute value]
    }

    button $f.del$cut -text "x" -fg red -pady 0 -padx 0 -bd 1 \
            -command [namespace code "-Row $f $cut $node"]
    label $f.name$cut -text "triggerWord"
    ComboBox::create $f.combo$cut \
            -editable false       \
            -text    [$node text] \
            -values   $opts       \
            -modifycmd [namespace code "modifyNode $node \[$f.combo$cut get\] $f.combo$cut %V"]
    label $f.comment$cut -text "# [$node getAttribute Comment]"
    grid $f.del$cut $f.name$cut $f.combo$cut - $f.comment$cut -sticky w
}
################################################################################
# Wrapper around modifyNode for actions specific to changes in Hijing
# parameters. Specifically, since I can't figure out how to add spaces
# in xslt, I ensure the value has a space before and after.
################################################################################
proc ::jobCreate::modifyHijingParam {node val e cond} {
    set new [string trim $val]
    modifyNode $node " $new " $e $cond
    return true
}
################################################################################
# Wrapper around modifyNode for actions specific to changes in Pythia
# parameters. Mostly want to update visual doEStruct.C
################################################################################
proc ::jobCreate::modifyPythiaParameter {node val e cond} {
    set new [string trim $val]
    if {![modifyNode $node $new $e $cond]} {
        return true
    }
    if {[info exists ::jobCreate::showingMain] && $::jobCreate::showingMain} {
        displayCode [$::jobCreate::jobInfo getElementsByTagName main] {}
    }
    return true
}
################################################################################
# Wrapper around modifyNode for actions specific to changes in jobControl
# information. Currently we make sure dispay of <job> or <main> is updated.
################################################################################
proc ::jobCreate::modifyJobControlNode {node val e cond} {
    if {![modifyNode $node $val $e $cond]} {
        return true
    }
    if {[info exists ::jobCreate::showingXML] && $::jobCreate::showingXML} {
        displayCode [$::jobCreate::jobInfo getElementsByTagName starSubmit] {}
    }
    if {[info exists ::jobCreate::showingMain] && $::jobCreate::showingMain} {
        displayCode [$::jobCreate::jobInfo getElementsByTagName main] {}
    }
    return true
}
################################################################################
# Wrapper around modifyNode for actions specific to changes in doEStruct
# information. Currently we make sure dispay of <main> is updated.
################################################################################
proc ::jobCreate::modifyMacroNode {node val e cond} {
    if {![modifyNode $node $val $e $cond]} {
        return false
    }
    if {[info exists ::jobCreate::showingMain] && $::jobCreate::showingMain} {
        set doE [$::jobCreate::jobInfo getElementsByTagName doEStructMacro]
        displayCode [$doE getElementsByTagName main] {}
    }
    return true
}
################################################################################
# displayCode is used for display of all text (as opposed to entry) nodes.
# For special cases of nodeName = main or starSubmit disable editing.
# When this proc is called check to see if we are already displaying
# a node and, if modified, ask user to save it.
################################################################################
proc ::jobCreate::displayCode {node title} {
    if {![winfo exists .fileText]} {
        toplevel .fileText
        set m [menu .fileText.menu]
        .fileText configure -menu $m
        set file [menu $m.file -postcommand [namespace code "checkFile $m.file .fileText.t"]]
        $m add cascade -label File -menu $file
        $file add command -label Save
        $file add command -label Revert
        $file add command -label Close -command [namespace code "wm iconify .fileText"]
        set edit [menu $m.edit -postcommand [namespace code "checkUndo $m.edit .fileText.t"]]
        $m add cascade -label Edit -menu $edit
        $edit add command -label Undo -command [namespace code "undo $m.edit .fileText.t"]
        $edit add command -label Redo -command [namespace code "redo $m.edit .fileText.t"]
        $edit add command -label Find -command [namespace code findDialog]

        text .fileText.t -yscrollcommand {.fileText.y set} \
                         -xscrollcommand {.fileText.x set} -wrap none \
                         -undo true
        scrollbar .fileText.y -command {.fileText.t yview}
        scrollbar .fileText.x -command {.fileText.t xview} -orient horizontal
        grid .fileText.t .fileText.y
        grid .fileText.x x
        grid .fileText.t -sticky news
        grid .fileText.y -sticky ns
        grid .fileText.x -sticky we
        grid columnconfigure .fileText 0 -weight 1
        grid rowconfigure    .fileText 0 -weight 1

        bind .fileText <Control-w> {destroy .fileText}
    }
    if {[.fileText.t edit modified] && [info exists ::jobCreate::lastNode]} {
        set title "Save edits for [$::jobCreate::lastNode nodeName]"
        set msg   "Do you want to save edits for [$::jobCreate::lastNode nodeName]?" 
        set save [tk_messageBox -message $msg -type yesno \
                -icon question -title $title]
        if {$save} {
            save .fileText.menu.file $::jobCreate::lastNode .fileText.t
        }
    }
    set ::jobCreate::showingMain false
    set ::jobCreate::showingXML  false
    .fileText.t configure -state normal
    if {[$node nodeName] eq "main"} {
        # Try to keep current position visible in file when minor changes
        # are made. This seems to assume that all other things we might
        # view in this window are small enough users won't scroll them.
        set yPos [.fileText.t yview]
        wm title .fileText "doEStruct.C: After current substitutions."
        .fileText.t delete 0.0 end
        .fileText.t insert end [applyXsl doEStruct.xsl asText]
        set ::jobCreate::showingMain true
        .fileText.t configure -state disabled
        .fileText.t yview moveto [lindex $yPos 0]
    } elseif {[$node nodeName] eq "starSubmit"} {
        wm title .fileText "xml for star-submit: After current substitutions."
        .fileText.t delete 0.0 end
        .fileText.t insert end [applyXsl job.xsl asXML]
        set ::jobCreate::showingXML true
        .fileText.t configure -state disabled
    } else {
        wm title .fileText "[$node nodeName]: $title"
        .fileText.t delete 0.0 end
        .fileText.t insert end [$node asText ]
    }
    .fileText.t edit reset
    .fileText.t edit modified false
    .fileText.menu.file entryconfigure Save   -state disabled \
            -command [namespace code "save   .fileText.menu.file $node .fileText.t"]
    .fileText.menu.file entryconfigure Revert -state disabled \
            -command [namespace code "revert .fileText.menu.file $node .fileText.t"]
    .fileText.menu.edit entryconfigure Undo -state disabled
    .fileText.menu.edit entryconfigure Redo -state disabled
    set ::jobCreate::undone 0
    set ::jobCreate::lastNode $node
    ::jobCreate::highlightString $::jobCreate::findString
}
################################################################################
# popupSearch will pop up a box allowing user to enter a search string and
# search text currently displayed in .fileText.t
################################################################################
proc ::jobCreate::findDialog {} {
    if {[winfo exists .findDialog]} {
        set w .findDialog
        raise $w
    } else {
        set w [toplevel .findDialog]
        wm resizable $w 0 0
        wm title $w "Find"
        entry  $w.e -textvar ::jobCreate::findString -bg white
        bind $w.e <KeyRelease> "::jobCreate::highlightString \$::jobCreate::findString"
        button $w.ok     -text Find   -command "::jobCreate::highlightString \$::jobCreate::findString"
        button $w.cancel -text Cancel -command "::jobCreate::highlightString {}
                                                destroy $w"
        grid $w.e  -        -sticky news
        grid $w.ok $w.cancel
    }
    focus $w.e
    $w.e icursor end
}
################################################################################
# highlightString is used by search to make string obvious to users.
################################################################################
proc ::jobCreate::highlightString {string} {
    if {![winfo exists .fileText]} {
        return
    }
    if {$string eq ""} {
        return
    }
    set t .fileText.t
    $t tag delete found
    $t tag configure found -foreground green -background blue
    set posM 0.0
    set pat mbBins
    while {[set pos [$t search -count length -regexp -- $string $posM end]] > 0} {
        $t tag add found $pos $pos+${length}c
        set posM $pos+1c
    }
}
################################################################################
# checkFile is invoked when Edit menu is posted.
# It checks if the text has been modified and enables/disables
# undo/redo menus to reflect current state.
################################################################################
proc ::jobCreate::checkFile {m t} {
    if {$::jobCreate::showingMain        ||
        $::jobCreate::showingXML} {
        return
    }
    if {[$t edit modified]} {
        $m entryconfigure Save   -state normal
        $m entryconfigure Revert -state normal
    } else {
        $m entryconfigure Save   -state disabled
        $m entryconfigure Revert -state disabled
    }
}
################################################################################
# Next five procs are for undo/redo of text widget.
# 1############################################################################
proc ::jobCreate::save {m n t} {
    foreach cn [$n childNodes] {
        if {[$cn nodeName] eq "#text"} {
            $cn nodeValue [$t get 1.0 end-1c]
        }
    }
    revert $m $n $t
}
# 2 ############################################################################
proc ::jobCreate::revert {m n t} {
    set title [wm title .fileText]
    regsub {[a-zA-Z0-9]*: } $title {} title
    $t edit modified false
    displayCode $n $title
}
# 3 ############################################################################
proc ::jobCreate::checkUndo {m t} {
    if {$::jobCreate::showingMain        ||
        $::jobCreate::showingXML} {
        return
    }
    if {[$t edit modified]} {
        $m entryconfigure Undo -state normal
    } else {
        $m entryconfigure Undo -state disabled
    }
    if {0 == $::jobCreate::undone} {
        $m entryconfigure Redo -state disabled
    } else {
        $m entryconfigure Redo -state normal
    }
}
# 4 ############################################################################
proc ::jobCreate::undo {m t} {
    if {[catch {$t edit undo}]} {
        $t edit modified false
    } else {
        incr ::jobCreate::undone
        $m entryconfigure Redo -state normal
    }
}
# 5 ############################################################################
proc ::jobCreate::redo {m t} {
    if {[catch {$t edit redo}]} {
        set ::jobCreate::undone 0
        $m entryconfigure Redo -state disabled
    } else {
        incr ::jobCreate::undone -1
    }
}

################################################################################
# m+XmlAtts creates a row with a combobox allowing choice of
# attributes to include in frame f.
# If label == {} we include a label displaying el to the left of the combobox.
# Attributes are added to node and included in display via makeAttributeRow
################################################################################
proc ::jobCreate::m+XmlAtts {f el node {label ""}} {
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$el'\]]
    set atts [list]
    foreach tN [$sNode selectNodes ./*/xs:attribute] {
        lappend atts [$tN getAttribute name]
    }

    if {$label eq ""} {
        set bf [button $f.del$el -text "x" -fg red -pady 0 -padx 0 -bd 1 \
                -command [namespace code "-ElementRow $f $el $node"]]
        set lf [label $f.addLabel$el -text "$el"]
    }
    variable cVal${f}$el {}
    ComboBox::create $f.combo$el \
            -editable     false  \
            -values       $atts  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   "$f.addAttribute$el configure -state normal"
    set af [button $f.addAttribute$el \
            -text "Include attribute" -pady 0 -padx 0 -bd 2 \
            -state disabled \
            -command [namespace code "[list makeAttributeRow $f $el $node]
                      $f.addAttribute$el configure -state disabled
                      set cVal${f}$el {}"]]
    if {$label eq ""} {
        grid $bf $lf $f.combo$el $af -sticky w
    } else {
        grid $f.combo$el - $af -sticky w
    }
    return $f.addLabel
}
################################################################################
# m+XmlElements creates a row with a combobox allowing choice of
# elements to include in job.
# The element command is dealt with separately.
# Element is inserted into job node and added to display via makeElementRow
################################################################################
proc ::jobCreate::m+XmlElements {f} {
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='starSubmit'\]]
    set els [list]
    foreach tN [$sNode selectNodes .//xs:element] {
        lappend els [$tN getAttribute name]
    }
    
    variable cVal$f {}
    ComboBox::create $f.combo    \
            -editable     false   \
            -values       $els    \
            -textvariable ::jobCreate::cVal$f \
            -modifycmd   "$f.addElement configure -state normal"
    set bf [button $f.addElement \
            -text "Include element" -pady 0 -padx 0 -bd 2 \
            -state disabled \
            -command [namespace code "[list makeElementRow $f]
                      $f.addElement configure -state disabled
                      set cVal$f {}"]]
    grid $f.combo $bf -sticky w
}
################################################################################
# Following command is only invoked by a button when combobox has
# a valid attribute selected. Add attribute to node and invoke
# insertAttributeRow to include in display.
################################################################################
proc ::jobCreate::makeAttributeRow {f el node} {
    set att [$f.combo$el get]
    $node setAttribute $att ""
    insertAttributeRow $f $el $att "" $node
}
################################################################################
# Following command is only invoked by a button when combobox has
# a valid element selected. Append element to job and invoke
# insertElementRow to include in display.
################################################################################
proc ::jobCreate::makeElementRow {f} {
    set el [$f.combo get]
    set xText "<$el/>"
    set eNode [$::jobCreate::jobInfo getElementsByTagName starSubmit]
    # appendXML should return the new node (according to my way
    # of thinking) but seems not to. Look for new child instead.
    set clBefore [$eNode childNodes]
    $eNode appendXML $xText
    set clAfter [$eNode childNodes]
    set new [lremove $clAfter $clBefore]
    insertElementRow $f $el $new
}

################################################################################
# Check that att was listed in combobox.
# If so remove from combobox and create row allowing editing of
# attribute value.
################################################################################
proc ::jobCreate::insertAttributeRow {f el att val node} {
    set possible [$f.combo$el cget -values]
    set ind [lsearch $possible $att]
    if {$ind < 0} {
        error "Tried adding attribute $att. Not in list of possible attributes."
    }
    set i1 [expr $ind-1]
    set i2 [expr $ind+1]
    set new "[lrange $possible 0 $i1] [lrange $possible $i2 end]"
    $f.combo$el configure -values $new

    button $f.del$att -text "x" -fg red -pady 0 -padx 0 -bd 1 \
            -command [namespace code "-AttributeRow $f $el $att $node"]
    label $f.name$att -text $att
    entry $f.e$att -width 40 -validate all \
            -vcmd [namespace code "modifyAttribute $node $att %P %W %V"]
    $f.e$att insert 0 $val
    if {$el eq "starSubmit"} {
        grid $f.del$att $f.name$att $f.e$att -sticky w
    } else {
        grid x $f.del$att $f.name$att $f.e$att -sticky w
    }

    set new [$::jobCreate::schemaInfo selectNode //*\[@name='$el'\]]
    if {[$new nodeName] eq "xs:element"} {
        set new [$::jobCreate::schemaInfo selectNode //*\[@name='$el'\]//*\[@name='$att'\]]
    }
    set comment [$new getAttribute Comment]
    ::DynamicHelp::register $f.name$att balloon $comment
}
################################################################################
# Check that el was listed in combobox.
# Keep count of how many nodes of this element are included in job.
# If we reach maxOccurs we remove it from list in combobox.
# Need to tag element with an index so the visual interface knows
# which one it is dealing with.
################################################################################
proc ::jobCreate::insertElementRow {f el node} {
    set possible [$f.combo cget -values]
    if {[lsearch $possible $el] < 0} {
        error "Tried adding element $el. Not in list of possible elements."
    }
    if {![info exists ::jobCreate::numberOfInstances($el)]} {
        set ::jobCreate::numberOfInstances($el) 1
    } else {
        incr ::jobCreate::numberOfInstances($el)
    }

    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='starSubmit'\]]
    set els [list]
    foreach tN [$sNode selectNodes .//xs:element] {
        lappend els [$tN getAttribute name]
    }
    set eNode [$sNode selectNodes .//*\[@name='$el'\]]
    set max [$eNode getAttribute maxOccurs]
    set ind [lsearch [lremove $els starSubmit command] $el]
    if {$max == $::jobCreate::numberOfInstances($el)} {
        set i1 [expr $ind-1]
        set i2 [expr $ind+1]
        set new "[lrange $possible 0 $i1] [lrange $possible $i2 end]"
        $f.combo configure -values $new
    }

    set fE [frame $f.${el}$::jobCreate::numberOfInstances($el)]
    grid $fE -columnspan 99
    m+XmlAtts $fE $el $node

    foreach att [$node attributes] {
        set val [$node getAttribute $att]
        insertAttributeRow $fE $el $att $val $node
    }
}
################################################################################
# modifyAttribute is like modifyNode. In all cases so far we need
# to update <job> if it is being displayed.
# Note that if attribute is not valid (as decided by is-a) we revert
# the value stored in the node and change the color in the entry widget
# to indicate it is not being used.
################################################################################
proc ::jobCreate::modifyAttribute {node att val e cond} {
    set curr [$node getAttribute $att]
    $node setAttribute $att $val
    if {[catch {is-a [$node nodeName] [$node asList]}]} {
        $node setAttribute $att $curr
        $e configure -foreground red
        return true
    }
    $e configure -foreground black
    if {[info exists ::jobCreate::showingXML] && $::jobCreate::showingXML} {
        set node [$::jobCreate::jobInfo getElementsByTagName starSubmit]
        displayCode $node {}
    }

    set ::jobCreate::jobFilesCurrent false
    return true
}
################################################################################
# -AttributeRow removes attribute from the dom and removes the widgets
# representing it in the gui.
# Update <job> if it is being displayed.
################################################################################
proc ::jobCreate::-AttributeRow {f el att node} {
    set possible [$f.combo$el cget -values]
    if {[lsearch $possible $att] < 0} {
        lappend possible $att
        $f.combo$el configure -values $possible
    }

    $node removeAttribute $att
    destroy $f.del$att $f.name$att $f.e$att

    if {[info exists ::jobCreate::showingXML] && $::jobCreate::showingXML} {
        set node [$::jobCreate::jobInfo getElementsByTagName starSubmit]
        displayCode $node {}
    }
}
################################################################################
# -ElementRow removes element from the dom and removes the widgets
# representing it in the gui.
# Update <job> if it is being displayed.
################################################################################
proc ::jobCreate::-ElementRow {f el node} {
    set possible [$f.combo$el cget -values]
    if {[lsearch $possible $el] < 0} {
        lappend possible $el
        $f.combo$el configure -values $possible
    }

    $node delete
    destroy $f

    if {[info exists ::jobCreate::showingXML] && $::jobCreate::showingXML} {
        set node [$::jobCreate::jobInfo getElementsByTagName starSubmit]
        displayCode $node {}
    }
}

################################################################################
# createJobFiles checks for directory tree. If it exists warn user.
# If it doesn't exist create it, then write files derived from
# xslt transformations of dom into scripts directory.
################################################################################
proc ::jobCreate::createJobFiles {} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]

    # If directory exists warn user about overwriting
    # Allow abort or destroy/recreate.
    if {[file exists $path]} {
        if {[file isdir $path]} {
            # Check that we have all the directories we expect and only
            # those directories.
            set dirs [list]
            foreach dir [glob $path/*/] {
                lappend dirs [file tail $dir]
            }
            set normal [list QA logs cuts data scripts txt stats StRoot]
            set extra [lremove $dirs $normal]
            set missing [lremove $normal $dirs]
            if {[llength $extra] == 0 && [llength $missing] == 0} {
                set title "Directory $path already exists"
                set msg   "The directory, $path, already exists. \
                         It has the right directories to have been \
                         previously created by me. Do you want to \
                         brutally destroy it and all its contents and \
                         replace it with a fresh set of job files?"
                set cont [tk_messageBox -message $msg -type yesno \
                        -icon warning -title $title -default no]
                if {!$cont} {
                    return false
                }
                file delete -force $path
            } else {
                set title "Directory $path already exists"
                set msg   "The directory, $path, already exists. \
                         It has the wrong directories to have been \
                         previously created by me. If you really want \
                         to use this directory name please delete it from \
                         outside this program and try again."
                set cont [tk_messageBox -message $msg -type ok -title $title]
                return false
            }
        }
        if {[file isfile $path]} {
            set title "A file $path exists"
            set msg   "A file, $path, exists. \
                     You may want to cancel this operation to \
                     consider if this file is important to you. \
                     If you proceed we will brutally destroy this \
                     file and replace it with a directory structure?"
            set cont [tk_messageBox -message $msg -type yesno \
                    -icon warning -title $title -default no]
            if {!$cont} {
                return false
            }
            file delete $path
        }
    }

    # Check if we have an un-saved change from the currently edited node.
    if {[winfo exists .fileText] && [.fileText.t edit modified]} {
        set title "Save edits for [$::jobCreate::lastNode nodeName]"
        set msg   "Do you want to save edits for [$::jobCreate::lastNode nodeName] before submitting job?" 
        set save [tk_messageBox -message $msg -type yesno \
                -icon question -title $title]
        if {$save} {
            save .fileText.menu.file $::jobCreate::lastNode .fileText.t
        }
    }
    
    # Make directory structure for job files and output.
    set d [list /]
    foreach dir [split $path /] {
        set d [file join $d $dir]
        if {![file exists $d]} {
            file mkdir $d
        }
    }
    if {![file exists $path/QA]} {
        file mkdir $path/QA
    }
    if {![file exists $path/logs]} {
        file mkdir $path/logs
    }
    if {![file exists $path/cuts]} {
        file mkdir $path/cuts
    }
    if {![file exists $path/data]} {
        file mkdir $path/data
    }
    if {![file exists $path/scripts]} {
        file mkdir $path/scripts
    }
    if {![file exists $path/txt]} {
        file mkdir $path/txt
    }
    if {![file exists $path/stats]} {
        file mkdir $path/stats
    }
    if {![file exists $path/StRoot]} {
        file mkdir $path/StRoot
    }

    # Create CutsFile with event and track cuts.
    set f [open $path/scripts/CutsFile.txt w]
    puts $f [applyXsl eventCuts.xsl asText]
    puts $f [applyXsl trackCuts.xsl asText]
    puts $f [applyXsl pairCuts.xsl asText]
    close $f
    # Create doEStructMacro
    set f [open $path/scripts/doEStruct.C w]
    puts $f [applyXsl doEStruct.xsl asText]
    close $f
    # Create xml file for star-submit
    # First line of xml file should be <?xml ... ?> which I don't get from asXML
    set f [open $path/scripts/job.xml w]
    puts $f "<?xml version='1.0' encoding='utf-8' ?>"
    set jobDesc [applyXsl job.xsl asXML]
    puts $f $jobDesc
    close $f

    # Might have Hijing Parameters
    set hNode [$::jobCreate::jobInfo getElementsByTagName hijingParams]
    if {$hNode ne ""} {
        set f [open $path/scripts/hijev.inp w]
        puts $f [applyXsl hijingParams.xsl asText]
        close $f
    }

    # Save the dom tree as an xml file
    # (use value of jobName for the file name.)
    # We get linefeeds in the <command> section which cause problems when
    # reading this file back in. Strip out all line feeds between
    # "root.exe" and the first ")" following it.
    # Might be using root4star instead of root.exe.)
    set node [$::jobCreate::jobInfo selectNodes //jobName]
    set f [open $path/scripts/[$node text].xml w]
    puts $f "<?xml version='1.0' encoding='utf-8' ?>"
    set jobInfo [$::jobCreate::jobInfo asXML]
    set start [string first root.exe $jobInfo]
    if {$start < 1} {
        set start [string first root4star $jobInfo]
    }
    if {$start >= 0} {
        set stop  [string first ) $jobInfo $start]
        set cmdOld [string range $jobInfo $start $stop]
        set cmdNew [string map {\n ""} $cmdOld]
        set subInfo [string replace $jobInfo $start $stop $cmdNew]
    }
    puts $f $subInfo
    close $f

    # Make copy of source code. This also picks up object files.
    # If this is a problem we should modify how copy is done.
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    set srcDir [$node text]
    if {[catch {file copy $srcDir/StRoot/StEStructPool $path/StRoot}]} {
        tk_messageBox -message "Problem copying StRoot subdir in $srcDir" \
                -type ok \
                -icon question -title "Skipping copying source code"
    }

    # Also write the jobPurpose into a README file fur user convenience.
    set node [$::jobCreate::jobInfo selectNodes //jobPurpose]
    set f [open $path/README w]
    puts $f [$node text]
    close $f

    set ::jobCreate::jobFilesCurrent true
    return true
}
################################################################################
# submitJob will invoke createJobFiles if dom has been modified since
#  its last invocation, then it will invoke star-submit.
################################################################################
proc ::jobCreate::submitJob {} {
    if {!$::jobCreate::jobFilesCurrent} {
        if {![createJobFiles]} {
            return
        }
    }

    checkProgressWindow

    set pwd [pwd]
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    cd $path/scripts

    set fLog [open $path/logs/submitLog w]
    puts $fLog ">>>>>Information printed by star-submit<<<<<"
    set fid [open "|star-submit job.xml"]
    fconfigure $fid -blocking false -buffering line
    fileevent $fid readable [namespace code "isReadable $fid $fLog"]
    set currCurs [.star-submit.t cget -cursor]
    .star-submit.t configure -cursor watch

    # Wait for the fileevents (i.e. scheduler) to finish.
    vwait ::DONE

    # Close the pipe
    close $fid

    # Check if user should start jobMonitor to complete job submission
    set sNode [$::jobCreate::jobInfo getElementsByTagName starSubmit]
    if {[$sNode hasAttribute simulateSubmission]} {
        if {[$sNode getAttribute simulateSubmission]} {
            puts $fLog ">>>>>You had simulateSubmission=true in job node for star-submit<<<<<"
            set message "You had the simulateSubmission set to true. \
                         To submit  these jobs you may consider starting the jobMonitor."
            set cont [tk_messageBox -message $message -type ok \
                    -icon question -title "scheduler done with simulate submission"]
        }
    }

    close $fLog
    .star-submit.t conf -cursor $currCurs
    .star-submit.b.action configure -text "finished submitting jobs"

    cd $pwd
}
proc ::jobCreate::checkProgressWindow {} {
    if {![winfo exists .star-submit]} {
        toplevel .star-submit
        grid [label  .star-submit.l -text "Output of star-submit command"]
        text .star-submit.t -yscrollcommand {.star-submit.y set} \
                         -xscrollcommand {.star-submit.x set} -wrap char \
                         -undo true
        scrollbar .star-submit.y -command {.star-submit.t yview}
        scrollbar .star-submit.x -command {.star-submit.t xview} -orient horizontal
        grid .star-submit.t .star-submit.y
        grid .star-submit.x ^
        grid .star-submit.t -sticky news
        grid .star-submit.y -sticky ns
        grid .star-submit.x -sticky we
        grid [frame  .star-submit.b]
        grid [button .star-submit.b.action -text Cancel \
                -command "destroy .star-submit"]
        grid columnconfigure .star-submit 0 -weight 1
        grid rowconfigure    .star-submit 1 -weight 1

        bind .star-submit <Control-w> {destroy .star-submit}

    }
}
################################################################################
# isReadable waits for output from the star-submit command and
#  puts it into a text widget.
# If it doesn't exist create it, then write files derived from
# xslt transformations of dom into scripts directory.
################################################################################
proc ::jobCreate::isReadable { fid fLog } {
    # The channel is readable; try to read it.
    set status [catch { gets $fid line } result]
    if { $status != 0 } {
        # Error on the channel
        puts $fLog ">>>>>error reading $fid: $result<<<<<"
        .star-submit.t insert end "error reading $fid: $result\n"
        .star-submit.t see end
        .star-submit.b.action configure -text Error -fg red
        set ::DONE 2
    } elseif { [eof $fid] } {
        puts $fLog ">>>>>star-submit command done<<<<<"
        .star-submit.b.action configure -text "star-submit done"
        set ::DONE 1
    } elseif { $result >= 0 } {
        puts $fLog $line
        .star-submit.t insert end "$line\n"
        .star-submit.t see end
    } elseif { [fblocked $fid] } {
        # Read blocked.  Just return
    } else {
        # Something else
        puts $fLog ">>>>>Something impossible happened while reading output from star-submit<<<<<"
        .star-submit.t insert end "the impossible happened while reading $fid: $result\n"
        .star-submit.t see end
        .star-submit.b.action configure -text Huh? -fg red
        set ::DONE 3
    }
}
################################################################################
# startJobMonitor sources and start a program that monitors running and
# output of batch jobs. We assume it is in the same directory as jobCreate.
################################################################################
#>>>>> Can we put jobMonitors into slave interpreters?
#      Would like to have more than one, and they coul
proc ::jobCreate::startJobMonitor {} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]

#    if {![info exists ::jobCreate::slaveInterpCount]} {
#        set ::jobCreate::slaveInterpCount 1
#    } else {
#        incr ::jobCreate::slaveInterpCount
#    }
    incr ::jobCreate::slaveInterpCount
    interp create slave$::jobCreate::slaveInterpCount
    interp eval slave$::jobCreate::slaveInterpCount "
        source [file join $::jobCreate::jobCreatePath jobMonitor.tcl]
        ::jobMonitor::createWindow $path
        wm withdraw .
    "
}
################################################################################
# exit deletes the jobCreate main window and destroys the namespace.
################################################################################
proc ::jobCreate::exit {} {
    destroy $::jobCreate::interfaceWindow
    namespace delete ::jobCreate::
    ::exit
}
################################################################################
# checkAnalysisType checks to see if analysisType is StEStructCorrelation.
# If so we enable menu for running selectAll. Otherwise we disable that menu
################################################################################
proc ::jobCreate::checkAnalysisType {menu} {
    if {![info exists ::jobCreate::jobInfo]} {
        return
    }
    set node [$::jobCreate::jobInfo getElementsByTagName analysisType]
    set aType [$node text]
    if {$aType eq "StEStructCorrelation"} {
       $menu entryconfigure 2 -state normal
    } else {
       $menu entryconfigure 2 -state disabled
    }
}
################################################################################
# addHistograms parses output directory for histograms and adds them
# in hopefully appropriate ways. Need to open status window for
# user feedback since addition of data files can take a long time.
################################################################################
proc ::jobCreate::addHistograms {} {
    if {![winfo exists .addHistograms]} {
        toplevel .addHistograms
        wm title .addHistograms "Adding histograms from job output"
        text .addHistograms.t -yscrollcommand {.addHistograms.y set} \
                              -xscrollcommand {.addHistograms.x set} -wrap word
        scrollbar .addHistograms.y -command {.addHistograms.t yview}
        scrollbar .addHistograms.x -command {.addHistograms.t xview} -orient horizontal
        grid .addHistograms.t .addHistograms.y
        grid .addHistograms.x ^
        grid .addHistograms.t -sticky news
        grid .addHistograms.y -sticky ns
        grid .addHistograms.x -sticky we
        grid [frame  .addHistograms.b]
        grid [button .addHistograms.b.action -text Cancel \
                -command {set ::jobCreate::stopAddHistograms true}]
        grid columnconfigure .addHistograms 0 -weight 1
        grid rowconfigure    .addHistograms 0 -weight 1
        .addHistograms.t tag configure input -foreground black
        .addHistograms.t tag configure normalOutput -foreground blue
        .addHistograms.t tag configure errorOutput -foreground red

        bind .addHistograms <Control-w> {destroy .addHistograms}
    } else {
        .addHistograms.b.action configure -text Cancel \
                -command {set ::jobCreate::stopAddHistograms true}
        raise .addHistograms
    }
    set ::jobCreate::stopAddHistograms false
    set currCurs [.addHistograms.t cget -cursor]
    .addHistograms.t configure -cursor watch

    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName analysisType]
    set aType [$node text]

    # Add QA files
    set fileList [lsort -dictionary [glob [file join $path QA QA_*.root]]]
    set QALogFile [file join $path QA addedQALog]
    if {[file exists $QALogFile]} {
        set fh [open $QALogFile a]
    } else  {
        set fh [open $QALogFile w]
    }
    set sum [file join $path QA QA.root]
    .addHistograms.t insert end "hadd $sum $fileList\n" input
    .addHistograms.t see end
    update
    puts $fh "hadd -f $sum $fileList"
    if {[catch {eval exec hadd -f $sum $fileList} mess]} {
        .addHistograms.t insert end $mess errorOutput
    } else {
        .addHistograms.t insert end $mess normalOutput
    }
    .addHistograms.t see end
    close $fh

    # Add cuts files
    set fileList [lsort -dictionary [glob [file join $path cuts cutHists_*.root]]]
    set cutsLogFile [file join $path cuts addedCutsLog]
    if {[file exists $cutsLogFile]} {
        set fh [open $cutsLogFile a]
    } else  {
        set fh [open $cutsLogFile w]
    }
    set sum [file join $path cuts Cuts.root]
    .addHistograms.t insert end "hadd $sum $fileList\n" input
    .addHistograms.t see end
    update
    puts $fh "hadd -f $sum $fileList"
    if {[catch {eval exec hadd -f $sum $fileList} mess]} {
        .addHistograms.t insert end $mess errorOutput
    } else {
        .addHistograms.t insert end $mess normalOutput
    }
    .addHistograms.t see end
    close $fh

    # Now for data histograms there is a difference between fluctuation
    # and correlation. For fluctuations all centralities are contained in
    # each file. For correlations we need to loop over centralities.
    set fileList [glob [file join $path data dataHists_*.root]]
    set top [lindex [lsort -dictionary $fileList] end]
    if {$aType eq "StEStructCorrelation"} {
        if {![regexp {([0-9]+)_([0-9]+)} $top m nCents nJobs]} {
            .addHistograms.t insert end "Did not find a file that looked like \
                a correlation histogram file in directory \
                [file join $path data ]  \n" errorOutput
            .addHistograms.t see end
            return
        }
        # Loop over centralities. For each centrality add groups of 15
        # files together.
        set tmpFile [list]
        for {set i 0} {$i <= $nCents} {incr i} {
            if {$::jobCreate::stopAddHistograms} {
                break
            }
            set fl [glob [file join $path data dataHists_M${i}_*.root]]
            set fileList [lsort -dictionary $fl]

            # Look to see if we have a log file from previous addition of files
            # If found parse it for files that have already been added and
            # remove them from the list.
            set reAdd false
            set logFile [file join $path data addedFileNames_${i}_Log]
            if {[file exists $logFile]} {
                .addHistograms.t insert end "Found a previous logfile. Will try appending new histograms to current sum." input
                .addHistograms.t see end
                set alreadyAdded [list]
                set fh [open $logFile]
                while {[gets $fh line] >= 0} {
                    set ll [split $line]
                    if {[lindex $line 0] eq "hadd"} {
                        set alreadyAdded [concat $alreadyAdded [lrange $ll 2 end]]
                    }
                }
                if {[llength $alreadyAdded] > 0} {
                    set reAdd true
                    set fileList [lremove $fileList $alreadyAdded]
                }
                close $fh
            }

            set nFiles [llength $fileList]
            set nSets [expr $nFiles/15]
            for {set j 0} {$j < $nSets} {incr j} {
                if {$::jobCreate::stopAddHistograms} {
                    break
                }
                set logFile [file join $path data addedFileNames_${i}_Log]
                if {[file exists $logFile]} {
                    set fh [open $logFile a]
                } else  {
                    set fh [open $logFile w]
                }
                set start [expr $j*15]
                set end   [expr $start+14]
                set sumFile [file join $path data Data_${i}_${j}.root]
                .addHistograms.t insert end "hadd $sumFile [lrange $fileList $start $end]" input
                .addHistograms.t see end
                update
                puts $fh "hadd $sumFile [lrange $fileList $start $end]"
#                set ::jobCreate::done false;
#                eval exec hadd $sumFile [lrange $fileList $start $end] ;# set ::jobCreate::done true
                if {[catch {eval exec hadd $sumFile [lrange $fileList $start $end]} mess]} {
                    .addHistograms.t insert end $mess errorOutput
                } else {
                    .addHistograms.t insert end $mess normalOutput
                }
                .addHistograms.t see end
#                vwait ::jobCreate::done
                lappend tmpFiles $sumFile
                close $fh
            }
            if {($nFiles%15 != 0) && !$::jobCreate::stopAddHistograms} {
                set logFile [file join $path data addedFileNames_${i}_Log]
                if {[file exists $logFile]} {
                    set fh [open $logFile a]
                } else  {
                    set fh [open $logFile w]
                }
                set start [expr $nSets*15]
                set sumFile [file join $path data Data_${i}_${j}.root]
                .addHistograms.t insert end "hadd $sumFile [lrange $fileList $start end]" input
                .addHistograms.t see end
                update
                puts $fh "hadd $sumFile [lrange $fileList $start end]"
#                set ::jobCreate::done false;
#                eval exec hadd $sumFile [lrange $fileList $start end] ;# set ::jobCreate::done true
                if {[catch {eval exec hadd $sumFile [lrange $fileList $start end]} mess]} {
                    .addHistograms.t insert end $mess errorOutput
                } else {
                    .addHistograms.t insert end $mess normalOutput
                }
                .addHistograms.t see end
#                vwait ::jobCreate::done
                lappend tmpFiles $sumFile
                close $fh
            }
        }

        # Now add all of those groups together (for each centrality)
        for {set i 0} {$i <= $nCents} {incr i} {
            if {$::jobCreate::stopAddHistograms} {
                break
            }
            set logFile [file join $path data addedFileNames_${i}_Log]
            set fh [open $logFile a]
            set fileList [lsort -dictionary [glob [file join $path data Data_${i}_*.root]]]
            set sumFile [file join $path data Data${i}.root]

            if {$reAdd} {
                set tmpName [file join $path data Data${i}Tmp.root]
                if {![file exists $sumFile]} {
                    error "I expected a previous sum for centrality $i in file $sumFile. File not found."
                }
                file rename $sumFile $tmpName
                lappend fileList $tmpName
                lappend tmpFiles $tmpName
            }
            if {[llength $fileList] > 1} {
                .addHistograms.t insert end "hadd $sumFile $fileList" input
                .addHistograms.t see end
                update
                puts $fh "hadd $sumFile $fileList"
#                set ::jobCreate::done false
#                eval exec hadd $sumFile $fileList ;# set ::jobCreate::done true
                if {[catch {eval exec hadd $sumFile $fileList} mess]} {
                    .addHistograms.t insert end $mess errorOutput
                } else {
                    .addHistograms.t insert end $mess normalOutput
                }
                .addHistograms.t see end
#                vwait ::jobCreate::done
            } else {
                .addHistograms.t insert end "file rename $fileList $sumFile" input
                .addHistograms.t see end
                update
                puts $fh "file rename $fileList $sumFile"
                file rename $fileList $sumFile
            }
            close $fh
        }
        foreach tmp $tmpFiles {
            catch {file delete $tmp}
        }
    } elseif {$aType eq "StEStructFluctuation"} {
        if {![regexp {_([0-9]+).} $top m nJobs]} {
            .addHistograms.t insert end "Did not find a file that looked like \
                a fluctuation histogram file in directory \
                [file join $path data ]  \n" errorOutput
            .addHistograms.t see end
            return
        }
        # Add files similarly to how we do correlations above. No need for
        # loop over centrality.
        # Should actually combine these two sections somehow.
        set fl [glob [file join $path data dataHists_*.root]]
        set fileList [lsort -dictionary $fl]

        # Look to see if we have a log file from previous addition of files
        # If found parse it for files that have already been added and
        # remove them from the list.
        set reAdd false
        set logFile [file join $path data addedFileNamesLog]
        if {[file exists $logFile]} {
            .addHistograms.t insert end "Found a previous logfile. Will try appending new histograms to current sum." input
            .addHistograms.t see end
            set alreadyAdded [list]
            set fh [open $logFile]
            while {[gets $fh line] >= 0} {
                set ll [split $line]
                if {[lindex $line 0] eq "hadd"} {
                    set alreadyAdded [concat $alreadyAdded [lrange $ll 2 end]]
                }
            }
            if {[llength $alreadyAdded] > 0} {
                set reAdd true
                set fileList [lremove $fileList $alreadyAdded]
            }
            close $fh
        }

        set nFiles [llength $fileList]
        set nSets [expr $nFiles/15]
        set tmpFile [list]
        for {set j 0} {$j < $nSets} {incr j} {
            if {$::jobCreate::stopAddHistograms} {
                break
            }
            set logFile [file join $path data addedFileNamesLog]
            if {[file exists $logFile]} {
                set fh [open $logFile a]
            } else  {
                set fh [open $logFile w]
            }
            set start [expr $j*15]
            set end   [expr $start+14]
            set sumFile [file join $path data Data_${j}.root]
            .addHistograms.t insert end "hadd $sumFile [lrange $fileList $start $end]" input
            .addHistograms.t see end
            update
            puts $fh "hadd $sumFile [lrange $fileList $start $end]"
#            set ::jobCreate::done false
#            eval exec hadd $sumFile [lrange $fileList $start $end] ;# set ::jobCreate::done true
            if {[catch {eval exec hadd $sumFile [lrange $fileList $start $end]} mess]} {
                .addHistograms.t insert end $mess errorOutput
            } else {
                .addHistograms.t insert end $mess normalOutput
            }
            .addHistograms.t see end
#            vwait ::jobCreate::done
            lappend tmpFiles $sumFile
            close $fh
        }
        if {($nFiles%15 != 0) && !$::jobCreate::stopAddHistograms} {
            set logFile [file join $path data addedFileNamesLog]
            if {[file exists $logFile]} {
                set fh [open $logFile a]
            } else  {
                set fh [open $logFile w]
            }
            set start [expr $nSets*15]
            set sumFile [file join $path data Data_${j}.root]
            .addHistograms.t insert end "hadd $sumFile [lrange $fileList $start end]" input
            .addHistograms.t see end
            update
            puts $fh "hadd $sumFile [lrange $fileList $start end]"
#            set ::jobCreate::done false
#            eval exec hadd $sumFile [lrange $fileList $start end] ;# set ::jobCreate::done true
            if {[catch {eval exec hadd $sumFile [lrange $fileList $start end]} mess]} {
                .addHistograms.t insert end $mess errorOutput
            } else {
                .addHistograms.t insert end $mess normalOutput
            }
            .addHistograms.t see end
#            vwait ::jobCreate::done
            lappend tmpFiles $sumFile
            close $fh
        }
        # Now add all of those groups together
        if {!$::jobCreate::stopAddHistograms} {
            set logFile [file join $path data addedFileNamesLog]
            set fh [open $logFile a]
            set fileList [lsort -dictionary [glob [file join $path data Data_*.root]]]
            set sumFile [file join $path data Data.root]

            if {$reAdd} {
                set tmpName [file join $path data DataTmp.root]
                if {![file exists $sumFile]} {
                    error "I expected a previous sum in file $sumFile. File not found."
                }
                file rename $sumFile $tmpName
                lappend fileList $tmpName
                lappend tmpFiles $tmpName
            }
            if {[llength $fileList] > 1} {
                .addHistograms.t insert end "hadd $sumFile $fileList" input
                .addHistograms.t see end
                update
                puts $fh "hadd $sumFile $fileList"
#                set ::jobCreate::done false
#                eval exec hadd $sumFile $fileList ;# set ::jobCreate::done true
                if {[catch {eval exec hadd $sumFile $fileList} mess]} {
                    .addHistograms.t insert end $mess errorOutput
                } else {
                    .addHistograms.t insert end $mess normalOutput
                }
                .addHistograms.t see end
#                vwait ::jobCreate::done
                close $fh
            } else {
                .addHistograms.t insert end "file rename $fileList $sumFile" input
                .addHistograms.t see end
                update
                puts $fh "file rename $fileList $sumFile"
                file rename $fileList $sumFile
            }
            if {[info exists tmpName]} {
                file delete $tmpName
                unset tmpName
            }
        }
        foreach tmp $tmpFiles {
            catch {file delete $tmp}
        }
    }
    .addHistograms.t configure -cursor $currCurs
    .addHistograms.b.action configure -text Done -command "destroy .addHistograms"
}
################################################################################
# runSelectAll executes the appropriate version of selectAll for the
# mode. Only available if analysisTyps is StEStructCorrelation.
################################################################################
proc ::jobCreate::runSelectAll {} {
    if {![winfo exists .addHistograms]} {
        toplevel .addHistograms
        wm title .addHistograms "Adding histograms from job output"
        text .addHistograms.t -yscrollcommand {.addHistograms.y set} \
                              -xscrollcommand {.addHistograms.x set} -wrap word
        scrollbar .addHistograms.y -command {.addHistograms.t yview}
        scrollbar .addHistograms.x -command {.addHistograms.t xview} -orient horizontal
        grid .addHistograms.t .addHistograms.y
        grid .addHistograms.x ^
        grid .addHistograms.t -sticky news
        grid .addHistograms.y -sticky ns
        grid .addHistograms.x -sticky we
        grid [frame  .addHistograms.b]
        grid [button .addHistograms.b.action -text Cancel \
                -command {set ::jobCreate::stopRunSelectAll true}]
        grid columnconfigure .addHistograms 0 -weight 1
        grid rowconfigure    .addHistograms 0 -weight 1
        .addHistograms.t tag configure input -foreground black
        .addHistograms.t tag configure normalOutput -foreground blue
        .addHistograms.t tag configure errorOutput -foreground red

        bind .addHistograms <Control-w> {destroy .addHistograms}
    } else {
        .addHistograms.b.action configure -text Cancel \
                -command {set ::jobCreate::stopRunSelectAll true}
        raise .addHistograms
    }
    set ::jobCreate::stopRunSelectAll false
    set currCurs [.addHistograms.t cget -cursor]
    .addHistograms.t configure -cursor watch

    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    set ldir [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]

    set currDir [pwd]
    cd $ldir

    # Assume we have less than 101 centralities.
    set fileList [glob [file join $path data Data\[0-9\].root]]
    lappend fileList [glob -nocomplain [file join $path data Data\[0-9\]\[0-9\].root]]
    set top [lindex [lsort -dictionary $fileList] end]
    if {![regexp {Data([0-9]+).root} $top m nCents]} {
        .addHistograms.t insert end "Did not find a file that looked like \
            a sum of the correlation histogram files in directory \
            [file join $path data ]  \n" errorOutput
        .addHistograms.t see end
        return
    }
    for {set i 0} {$i <= $nCents} {incr i} {
        if {$::jobCreate::stopRunSelectAll} {
            break
        }
        set logFile [file join $path data selectAllLog]
        if {[file exists $logFile]} {
            set fh [open $logFile a]
        } else  {
            set fh [open $logFile w]
        }
        set data [file join $path data]
        .addHistograms.t insert end "root4star -q -b selectAllM${mode}.C\(\"$data\",\"Data${i}\"\);" input
        .addHistograms.t see end
        puts $fh "root4star -q -b selectAllM${mode}.C\(\"$data\",\"Data${i}\"\);"
        # Hope this after command allows the gui to update.
        update
        if {[catch {exec root4star -q -b selectAllM${mode}.C\(\"$data\",\"Data${i}\"\);} mess]} {
            .addHistograms.t insert end "$mess" errorOutput
        } else {
            .addHistograms.t insert end "$mess" normalOutput
        }
        .addHistograms.t see end
        puts $fh "$mess"
        close $fh
    }
    .addHistograms.t configure -cursor $currCurs
    .addHistograms.b.action configure -text Done -command "destroy .addHistograms"
    cd $currDir
}
 
 
 # displayHelp --
#    Invoked via menu
#
# Arguments:
#    Widget to display help in. If empty create a toplevel named .jobCreateHelp
#    containing a scrolled text widget.
# Result:
#    Display help text in a toplevel widget.
#    If toplevel exists already reuse it. Else create a new toplevel.
# Side effects:
#    May create a new toplevel widget.
#
proc ::jobCreate::displayHelp {w} {
    if {[string equal $w ""]} {
        catch {destroy .jobCreateHelp}
        toplevel .jobCreateHelp
        wm transient .jobCreateHelp $::jobCreate::interfaceWindow
        wm title .jobCreateHelp "jobCreate Help"
        if {[regexp {(\+[0-9]+)(\+[0-9]+)$} [wm geom $::jobCreate::interfaceWindow] => wx wy]} {
            wm geom .jobCreateHelp "+[expr {$wx+35}]+[expr {$wy+35}]"
        }
        set w .jobCreateHelp.t
        text $w -wrap word -width 70 -height 28 -pady 10 \
                -yscrollcommand {.jobCreateHelp.s set}
        scrollbar .jobCreateHelp.s -command {.jobCreateHelp.t yview}
        grid $w .jobCreateHelp.s
        grid $w -sticky news
        grid .jobCreateHelp.s -sticky ns
        button .jobCreateHelp.quit -text Dismiss -command {catch {destroy .jobCreateHelp}}
        grid .jobCreateHelp.quit -
        grid columnconfigure .jobCreateHelp 0 -weight 1
        grid rowconfigure .jobCreateHelp 0 -weight 1
    }

    $w tag config header -justify center -font bold -foreground red
    $w tag config header2  -justify center -font bold
    set margin [font measure [$w cget -font] " o "]
    set margin2 [font measure [$w cget -font] " - "]
    $w tag config bullet -lmargin2 $margin
    $w tag config bullet -font "[font actual [$w cget -font]] -weight bold"
    $w tag config n -lmargin1 $margin -lmargin2 $margin2

    $w insert end "A Dynamic Dispatch System" header "\nby Duncan Prindle\n\n" header2


    $w insert end " o What does this do?\n" bullet

    $w insert end "- The purpose of this program is to create the batch jobs " n
    $w insert end "necessary for running EStruct analysis on all data sets. " n
    $w insert end "The data sets include data that has been processed to the " n
    $w insert end "MuDest level, GEANT that has been processed to the MuDst " n
    $w insert end "level and Hijing, Pythia and GeVSim taken directly from " n
    $w insert end "the event generators. The output from these batch jobs is " n
    $w insert end "typically combined and then run through another analysis " n
    $w insert end "step that is much less CPU intensive. This second processing " n
    $w insert end "step is now incorporated within this analysis framework. " n
    $w insert end "See the description of menus under Post for more details.\n " n
    $w insert end "step is now incorporated within this analysis framework. " n

    $w insert end "- A typical batch job requires a number of files including " n
    $w insert end "a cuts file, a doEStruct.C macro and an xml file to be " n
    $w insert end "passed to the scheduler. The scheduler creates an additional " n
    $w insert end "set of script and filelist files. Hijing and GeVSim also " n
    $w insert end "have files to contol their behaviour.\n" n

    $w insert end "- This interface allows the user to select a standard " n
    $w insert end "EStruct fluctuation or correlation analysis job " n
    $w insert end "and modify everything while doing its best to keep it all " n
    $w insert end "consistent.\n" n

    $w insert end " o New: You can give the name of an xml file (such as one produced " n
    $w insert end "by this program) on the command line.\n\n\n" n


    $w insert end "Menus\n\n" header

    $w insert end " o File\n" bullet
    $w insert end "- New Schema File...\n" n
    $w insert end "Primarily for debugging/developing.\n" n
    $w insert end "- Read Job Description...\n" n
    $w insert end "Read in previously created xml description of a job. " n
    $w insert end "This file may have previously been created by this interface. " n
    $w insert end "Can also give this file name as argument when starting jobCreate.\n" n
    $w insert end "- Create Job Files\n" n
    $w insert end "Create output directory hierarchy and populate with files " n
    $w insert end "created by this interface. (Does not invoke the scheduler.)\n" n
    $w insert end "- Submit Job\n" n
    $w insert end "Create output directory hierarchy, populate it with files " n
    $w insert end "created by this interface, then invoke the scheduler.\n" n
    $w insert end "- Start jobMonitor\n" n
    $w insert end "Start a companion program, jobMonitor, which allows " n
    $w insert end "monitoring, killing, fixing and re-submitting batch jobs. " n
    $w insert end "(See that program for more detailed help on it.)\n" n
    $w insert end "- Exit (or ^C)\n" n
    $w insert end "Exit from jobCreate. If jobMonitor has been started from " n
    $w insert end "jobCreate we kill it too.\n" n

    $w insert end " o Edit\n" bullet
    $w insert end "- Just a placeholder for now until I think of a use for it\n" n

    $w insert end " o Post\n" bullet
    $w insert end "- Add Histograms\n" n
    $w insert end "  For use after all jobs are finished. " n
    $w insert end "Add all cuts histograms to Cuts.root, all QA histograms " n
    $w insert end "to QA.root and all dataHistograms. For StEStructCorrelation " n
    $w insert end "we want to end up with one DataN.root histograms for each " n
    $w insert end "centrality N. For StEStructFluctuations we end up with one " n
    $w insert end "file, Data.root, containing all centralities internally. " n
    $w insert end "We add to intermediate files in groups of 15, then " n
    $w insert end "sum all these groups together, cleaning up the intermediate " n
    $w insert end "files at the end.\n " n
    $w insert end "  This can take a while.\n" n
    $w insert end " We always re-add the QA and cuts files, but for the data " n
    $w insert end "we check the addedFiles logs and add only the files that " n
    $w insert end "have not been included in the sum yet. Useful if you want " n
    $w insert end "to look at results before all jobs have finished.\n" n
    $w insert end "- Run selectAll\n" n
    $w insert end "  For StEStructCorrelation only. " n
    $w insert end "Uses the cutMode to decide which selectAll macro to run," n
    $w insert end "then process all Datan.root files\n\n" n


    $w insert end "Analysis Type\n\n" header

    $w insert end " o Data:\n" bullet
    $w insert end "- This allows the selection of a job for any of the " n
    $w insert end "largs STAR datasets.\n" n

    $w insert end " o GEANT:\n" bullet
    $w insert end "- This allows the selection of from a set of 'standard' " n
    $w insert end "GEANT jobs.\n" n

    $w insert end " o Hijing:\n" bullet
    $w insert end "- This allows the selection of from a set of 'standard' " n
    $w insert end "Hijing jobs. The user will be able modify all of the Hijing " n
    $w insert end "parameters (that are accessible via hijev.inp.)\n" n

    $w insert end " o Pythia:\n" bullet
    $w insert end "- This allows the selection of from a set of 'standard' " n
    $w insert end "Pythia jobs. The user will be able to modify the parameters " n
    $w insert end "that are used in the pythia initialization call.\n\n " n

    $w insert end "After selecting one of the analysis types, select one " n
    $w insert end "of the specific jobs via the drop-down box and hitting " n
    $w insert end "'Use selection' (or bypass this part by using the 'Read Job Description' menu)) " n
    $w insert end "you will will see a set of frames each of which can be expanded/contracted. " n
    $w insert end "The actual frames depend on the type of analysis job.\n\n\n" n


    $w insert end "jobControl\n\n" header

    $w insert end "Most of these parameters are used by more than one of the other " n
    $w insert end "blocks.\n\n" n

    $w insert end " o jobName:\n" bullet
    $w insert end "- A scratch directory with this name is created in /tmp/\$USER " n
    $w insert end "where the output root files are written.\n" n

    $w insert end " o outputDir:\n" bullet
    $w insert end "- Output root files are copied to this directory at end of job. " n
    $w insert end "In many of the default xml files I use an environment variable " n
    $w insert end "MYDATA as the first part of the path. This allows me to use " n
    $w insert end "the same code on rcf and pdsf.\n" n

    $w insert end " o starLibVersion:\n" bullet
    $w insert end "- Taken from an environment variable by default.\n" n

    $w insert end " o localDir:\n" bullet
    $w insert end "- This is the directory from which you have run cons " n
    $w insert end "to create the shared libraries you will use for this job.\n" n

    $w insert end " o eventsPerJob:\n" bullet
    $w insert end "- Passed into doEStruct.\n" n

    $w insert end " o jobPurpose:\n" bullet
    $w insert end "- Arbitrary text which will be written into a README file " n
    $w insert end "in the outputDir. Use this to remind yourself what the job was for " n
    $w insert end "sometime in the future when you have forgotten running it.\n\n" n


    $w insert end "starSubmit\n" header

    $w insert end " o show <job>\n" bullet
    $w insert end "- Pops up window containing xml file that will be used in " n
    $w insert end "star-submit command. " n
    $w insert end "For more information on the text display/edit window see the " n
    $w insert end "section titled \"Display/Edit window\".\n\n" n

    $w insert end "Everything else in this frame should be described on the STAR web page " n
    $w insert end "describing the scheduler. " n
    $w insert end "(See http://www.star.bnl.gov/STAR/comp/Grid/scheduler/manual.htm#3 perhaps.) " n
    $w insert end "To add an Attribute/Eelement that has not been included select it " n
    $w insert end "in the combobox and click 'include attribute/element' " n
    $w insert end "To remove an Attribute/Element click the button with the red x.\n\n" n

    $w insert end "- toURL and fromURL attributes will have OUTPUTDIR replaced by " n
    $w insert end "the outputDir specified in the jobControl frame if the path " n
    $w insert end "starts with file:OUTPUTDIR\n\n" n

    $w insert end "- fromScratch attributes will have the outputDir specified in the jobControl " n
    $w insert end "frame prepended.\n\n\n" n


    $w insert end "eventCuts, trackCuts, pairCuts\n\n" header

    $w insert end "- Select cut in combobox, then 'Add New Cut' to include a cut. " n
    $w insert end "Click on button with red x to exclude cut.\n\n" n


    $w insert end "doEStructMacro\n\n" header

    $w insert end "- The doEStruct macro is made up from parts that depend on what " n
    $w insert end "type of job is required. The actual widgets that show up here " n
    $w insert end "depending on whether you want to do a 2pt correlation or a fluctuation " n
    $w insert end "analysis.\n\n" n

    $w insert end " o analysisType\n" bullet
    $w insert end "- Choice of StEStructEmpty, StEStructFluctuation or StEStructCorrelation.\n\n" n

    $w insert end "Common in both analysisType choices are:\n\n" n

    $w insert end " o centralities\n" bullet
    $w insert end "- Space separated list of numbers. By default these numbers refer to the " n
    $w insert end "total multiplicity passing track cuts. In the case of Hijing this list " n
    $w insert end "can refer to impact parameter, depending on value of useImpactParameter flag. " n
    $w insert end "In principle this list could refer to any property of the event, as long as " n
    $w insert end "the reader supports that selection.\n\n" n

    $w insert end " o analysisMode\n" bullet
    $w insert end "- Bit pattern passed to constructor of analsis object.\n\n" n

    $w insert end " o declareAnalysis\n" bullet
    $w insert end "- Code to declare the analysis. Clicking on button will make this information editable. " n
    $w insert end "For more information on the text display/edit window see the " n
    $w insert end "section titled \"Display/Edit window\".\n\n" n

    $w insert end " o declareReader\n" bullet
    $w insert end "- Code to declare the reader. Clicking on button will make this information editable. " n
    $w insert end "For more information on the text display/edit window see the " n
    $w insert end "section titled \"Display/Edit window\".\n\n" n

    $w insert end " o allocateAnalysis\n" bullet
    $w insert end "- Code to allocate the analysis object. Clicking on button will make this information editable. " n
    $w insert end "For more information on the text display/edit window see the " n
    $w insert end "section titled \"Display/Edit window\".\n\n" n

    $w insert end " o weightsFile\n" bullet
    $w insert end "- For use in reaction plane analyses. \n\n" n

    $w insert end " o main\n" bullet
    $w insert end "- Complete doEStruct macro. Clicking on the button will bring up a " n
    $w insert end "window showing the text. It is not directly editable (but automatically reflects " n
    $w insert end "changes that modify it.) " n
    $w insert end "For more information on the text display/edit window see the " n
    $w insert end "section titled \"Display/Edit window\".\n\n" n

    $w insert end "Next options are only in StEStructFluctuation:\n\n" n

    $w insert end " o ptCuts\n" bullet
    $w insert end "- In addition to doing analysis for complete pt range specified " n
    $w insert end "by track cuts we do analysis in these bins. \n\n" n

    $w insert end " o ptCentralities\n" bullet
    $w insert end "- All pt bins have same centrality selection but this is " n
    $w insert end "different than the cuts for the entire pt range. \n\n" n


    $w insert end "Next options are only in StEStructCorrelation:\n\n" n

    $w insert end " o cutMode\n" bullet
    $w insert end "- Flag that chooses actual cut binning mode. \n\n" n


    $w insert end "Next options are only when we are using Hijing as input:\n\n" n

    $w insert end " o useImpactParameter\n" bullet
    $w insert end "- true or false. See 'centralities' for more information.\n\n" n



    $w insert end "hijingParams\n" header

    $w insert end "- This shows (and makes editable) all parameters from hijev.inp\n\n" n


    $w insert end "pythiaInit\n" header

    $w insert end " o pyFrame\n" bullet
    $w insert end "- Choice of CMS, FIXT.\n" n
    $w insert end " - Some of the options for pyFrame are not avaliable. In particular, " n
    $w insert end "in order for '3MOM', '4MOM', and '5MOM' to work properly values in a " n
    $w insert end "common block have to be set. We could add an interface to these in principle.\n" n

    $w insert end " o pyBeam\n" bullet
    $w insert end " o pyTarget\n" bullet
    $w insert end "- Choice of lots of possible beam and target species.\n" n
    $w insert end "- Not all values for pyBeam and pyTarget are available. " n
    $w insert end "In particular gammas from bremstrahlung of e, mu and tau " n
    $w insert end "require values in a common block to be set.\n\n" n

    $w insert end " o pyEnergy\n" bullet
    $w insert end "- Energy in GeV for beam or CMS, depending on pyFrame.\n\n" n

    $w insert end " o pyTune " bullet
    $w insert end " - Chooses a group of options used to tune Pythia for a particular physics case.\n\n" n


    $w insert end "Display/Edit Window\n" header

    $w insert end " o This window shows text fields that don't fit in a single line.\n " bullet
    $w insert end " - Some of these fields are editable and some not (when they include other " n
    $w insert end "fields that can be changed elswhere in this interface.) " n
    $w insert end "This window has it's own menus allowing you to undo/redo edits " n
    $w insert end "and search for text within the window.\n\n " n

    $w config -state disabled
}
