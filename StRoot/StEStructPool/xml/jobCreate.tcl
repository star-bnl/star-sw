
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
    variable numberOfInstances
    variable slaveInterpCount 0
    variable findString ""
    array set combinedJobs {
        1   ""
        2   ""
        sum ""
    }

    variable addQAHistograms 1
    variable addCutsHistograms 1
    variable tagColors
    array set tagColor {
        declareAnalysis  #d9f1d9
        declareReader    #9bf1d9
        allocateAnalysis #9cbcd1
        preLoop   #b1c494
        preEvent  #c8b486
        postEvent #d0a18e
        postLoop  #b1998e
    }
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
proc ::jobCreate::sumList {l} {
    set sum 0
    foreach e $l {
        set sum [expr $sum + [lindex $e 0]]
    }
    return $sum
}
proc ::jobCreate::reduceList {l i {j end}} {
    set r [list]
    foreach e $l {
        lappend r [lrange $e $i $j]
    }
    return $r
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
        #>>>>> Note to self: there may be an -keepEmpties switch when using
        #      asXML (also asText?). Maybe I can fix the annoying problem that spaces
        #      and new lines get eaten by XSLT? (also an -indent none switch.)
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

    # File menu.
    $m add cascade -label File -menu $file
    $file add command -label "New Schema File..."      -command [namespace code getNewSchema]
    $file add command -label "Read Job Description..." -command [namespace code [list getJobXmlFile]]
    $file add command -label "Create Job Files"        -command [namespace code createJobFiles]
    $file add command -label "Submit Job"              -command [namespace code submitJob] -accelerator "Alt-S"
    $file add command -label "Filter filelist"         -command [namespace code filterLists] -accelerator "Alt-F"
    $file add command -label "Start jobMonitor"        -command [namespace code startJobMonitor] -accelerator "Alt-M"
    $file add command -label Exit -command [namespace code exit] -accelerator "Ctrl-Q"
    # Accelerators don't seem to actually invoke code.
    bind $::jobCreate::interfaceWindow <Alt-S>     [namespace code submitJob]
    bind $::jobCreate::interfaceWindow <Alt-F>     [namespace code filterLists]
    bind $::jobCreate::interfaceWindow <Alt-M>     [namespace code startJobMonitor]
    bind $::jobCreate::interfaceWindow <Control-Q> [namespace code exit]

    # Edit menu. Just a place holder.
    #set edit [menu $m.edit]
    #$m add cascade -label Edit -menu $edit
    #$edit add command -label PlaceHolder

    # Post batch job menu.
    set post [menu $m.post -postcommand [namespace code [list checkAnalysisType $m.post]]]
    $m add cascade -label "Apr\u00e8s batch" -menu $post
    $post add command -label "Add histograms"       -command [namespace code addHistograms]       -accelerator "Alt-A"
    $post add command -label "Combine centralities" -command [namespace code combineCentralities] -accelerator "Alt-C"
    $post add command -label "selectAll macro"      -command [namespace code selectAll]           -accelerator "Alt-s"
    $post add command -label "pileup extrapolation" -command [namespace code pileupExtrapolation] -accelerator "Alt-e"
    $post add command -label "copy to HPSS"         -command [namespace code copyToHPSS]          -accelerator "Alt-c"
    #
    bind $::jobCreate::interfaceWindow <Alt-A> [namespace code addHistograms]
    bind $::jobCreate::interfaceWindow <Alt-C> [namespace code combineCentralities]
    bind $::jobCreate::interfaceWindow <Alt-s> [namespace code selectAll]
    bind $::jobCreate::interfaceWindow <Alt-e> [namespace code pileupExtrapolation]
    bind $::jobCreate::interfaceWindow <Alt-c> [namespace code copyToHPSS]

    # Help menu
    set help [menu $m.help]
    $m add cascade -label Help -menu $help
    $help add command -label Help -command [namespace code [list displayHelp ""]] -accelerator "F1"
    bind $::jobCreate::interfaceWindow <F1> [namespace code [list displayHelp ""]]


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
                           data_ppProductionMinBias_2005.lis \
                           data_pp400MinBias_2005.lis \
                           data_pp2006MinBias_2006.lis \
                           data_pp2pp_VPDMB_2009.lis \
                           data_ppProductionMB62_2006.lis \
                           production2009_200GeV_Single.lis \
                           dataAuAu7_2010_MinBias.lis \
                           dataAuAu11_2010_MinBias.lis \
                           dataAuAu19_2001_MinBias.lis \
                           dataAuAu19_2011_MinBias.lis \
                           dataAuAu27_2011_MinBias.lis \
                           dataAuAu39_2010_MinBias.lis \
                           dataAuAu62_2004_MinBias.lis \
                           dataAuAu62_2010_MinBias.lis \
                           dataAuAu130_2000_MinBias.lis \
                           dataAuAu200_2001_MinBiasVertex.lis \
                           dataAuAu200_2001_ProductionMinBias.lis \
                           dataAuAu200_2004_ProductionMinBias_FullField.lis \
                           dataAuAu200_2004_ProductionMinBias_ReversedFullField.lis \
                           dataAuAu200_2004A_ProductionMinBias.lis \
                           dataAuAu200_2004B_ProductionMinBias.lis \
                           dataAuAu200_2004_ProductionLow.lis \
                           dataAuAu200_2004_ProductionMid.lis \
                           dataAuAu200_2004_ProductionHigh.lis \
                           dataAuAu200_2007ProductionMinBias.lis \
                           dataAuAu200_2007Production2.lis \
                           dataAuAu200_2007_MinBias.lis \
                           dataAuAu200_2007_PMD.lis \
                           dataAuAu200_2010_MinBias.lis \
                           dataAuAu200_2011_MinBias.lis \
                           dataCuCu22_P05if_cuProductionMinBias.lis \
                           dataCuCu62_2005_ProductionMinBias.lis \
                           dataCuCu62_2007ic_cuProductionMinBias.lis \
                           dataCuCu200_2005_ProductionMinBias.lis \
                           dataCuCu200_2007ic_cuProductionMinBias.lis]
        }
        GEANT {
            set vals [list GEANTPP200_minbias.lis \
                           GEANTAuAu200_b_0_3.lis \
                           GEANTAuAu200_minbias_P08if.lis]
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
            set vals [list pythia200GeVPP.lis]
        }
        Therminator {
            set vals [list therminator200GeV.lis]
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
                pack $f.f -in $f -fill x
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
            -command [namespace code "displayCode $jNode
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

    grid columnconfigure $f 0 -weight 0
    grid columnconfigure $f 1 -weight 1
    set bFrame ""
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
            if {$bFrame eq ""} {
                set bFrame [frame $f.bFrame]
                grid $bFrame -sticky we -columnspan 2
            }
            if {$name eq "main"} {
                label  $f.$name -text doEStruct.C
                button $f.${name}Code -text edit -pady 1 \
                        -command [namespace code "displayCode $node
                                  wm deiconify .fileText
                                  raise .fileText"]
                grid $f.$name $f.${name}Code -sticky w
                ::DynamicHelp::register $f.$name balloon $comment
            } else {
                button $bFrame.$name -text $name -pady 1          \
                        -background $::jobCreate::tagColor($name) \
                        -command [namespace code "chooseTagColor $bFrame.$name $name"]
                pack $bFrame.$name -side left
            }
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
# 9? ############################################################################
proc ::jobCreate::+therminatorParams {f} {
    set jNode [$::jobCreate::jobInfo getElementsByTagName therminatorParams]

    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='therminatorParams'\]]
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
            m+TherminatorParameter $f $name $node
        } elseif {$wType eq "entry"} {
            label $f.$name -text $name
            variable code${f}$name [$node text]
            entry $f.${name}Code                \
                    -textvariable ::jobCreate::code${f}$name \
                    -width 40                   \
                    -validate all               \
                    -vcmd [namespace code "modifyTherminatorParam $node %P %W %V"]
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

    label $f.$el -text $el
    variable cVal${f}$el $val
    ComboBox::create $f.combo$el \
            -editable     true   \
            -values       $vals  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   [namespace code "modifyPythiaParameter $node \[$f.combo$el get\] $f.combo$el %V"]
    grid $f.$el $f.combo$el -sticky w
}
################################################################################
proc ::jobCreate::m+TherminatorParameter {f el node} {
    set val [$node text]
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$el'\]]
    set sR    [$sNode getElementsByTagName xs:restriction]
    set type  [$sR getAttribute base]
    set sR2   [$::jobCreate::schemaInfo selectNodes //*\[@name='$type'\]]
    set vals [list]
    foreach sN [$sR2 getElementsByTagName xs:enumeration] {
        lappend vals [$sN getAttribute value]
    }

    label $f.$el -text $el
    variable cVal${f}$el $val
    ComboBox::create $f.combo$el \
            -editable     false  \
            -values       $vals  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   [namespace code "modifyTherminatorParam $node \[$f.combo$el get\] $f.combo$el %V"]
    grid $f.$el $f.combo$el -sticky w
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

    label $f.$el -text $el
    variable cVal${f}$el $val
    ComboBox::create $f.combo$el \
            -editable     false  \
            -values       $vals  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   [namespace code "modifyMacroNode $node \[$f.combo$el get\] $f.combo$el %V"]
    grid $f.$el $f.combo$el -sticky w
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

    label $f.$el -text $el
    variable cVal${f}$el $val
    ComboBox::create $f.combo$el \
            -editable     false  \
            -values       $vals  \
            -textvariable ::jobCreate::cVal${f}$el \
            -modifycmd   [namespace code "setAnalysisType $f \[$f.combo$el get\]"]
    grid $f.$el $f.combo$el -sticky w
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
    if {$type eq "trackCut"} {
        lappend vals "hijingFragment"
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
    set node [$jNode getElementsByTagName hijingFragment]
    if {$node ne ""} {
        +Row $f $type hijingFragment
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
    if {$cut eq "hijingFragment"} {
        i+hijingFragmentRow $f
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
    if {$cut eq "hijingFragment"} {
        foreach jN [$jNode getElementsByTagName $cut] {
            +hijingFragmentRow $f $jN
        }
        set new "$new hijingFragment"
        $f.combo configure -values $new
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
    if {$cut eq "hijingFragment"} {
        destroy $f.del${cut}_${node} $f.name${cut}_${node} $f.combo${cut}_${node} $f.comment${cut}_${node}
    } else {
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
# i+hijingFragmentRow inserts a hijingFragment into the dom
################################################################################
proc ::jobCreate::i+hijingFragmentRow {f} {
    set cut hijingFragment
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$cut'\]]
    set vNode [lindex [$sNode selectNodes .//*\[@value\]] 0]
    set frag [$vNode getAttribute value]

    set jNode [$::jobCreate::jobInfo getElementsByTagName trackCuts]
    set xText "<hijingFragment Comment='type of fragmentation producing particle'>$frag</hijingFragment>"
    $jNode appendXML $xText
    +Row $f trackCut $cut
}
################################################################################
# +hijingFragmentRow inserts a Hijing fragmentation selection into the dom and 
################################################################################
proc ::jobCreate::+hijingFragmentRow {f node} {
    set cut hijingFragment
    if {[winfo exists $f.del${cut}_${node}]} {
        return
    }
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='hijingFragment'\]]
    set opts [list]
    foreach tN [$sNode selectNodes .//*\[@value\]] {
        lappend opts [$tN getAttribute value]
    }

    button $f.del${cut}_${node} -text "x" -fg red -pady 0 -padx 0 -bd 1 \
            -command [namespace code "-Row $f $cut $node"]
    label $f.name${cut}_${node} -text "hijingFragment"
    ComboBox::create $f.combo${cut}_${node} \
            -editable false       \
            -text    [$node text] \
            -values   $opts       \
            -modifycmd [namespace code "modifyNode $node \[$f.combo${cut}_${node} get\] $f.combo${cut}_${node} %V"]
    label $f.comment${cut}_${node} -text "# [$node getAttribute Comment]"
    grid $f.del${cut}_${node} $f.name${cut}_${node} $f.combo${cut}_${node} - $f.comment${cut}_${node} -sticky w
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
        displayCode [$::jobCreate::jobInfo getElementsByTagName main]
    }
    return true
}
################################################################################
# Wrapper around modifyNode for actions specific to changes in Therminator
# parameters. Specifically, since I can't figure out how to add spaces
# in xslt, I ensure the value has a space before.
################################################################################
proc ::jobCreate::modifyTherminatorParam {node val e cond} {
    set new [string trim $val]
    modifyNode $node " $new" $e $cond
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
        displayCode [$::jobCreate::jobInfo getElementsByTagName starSubmit]
    }
    if {[info exists ::jobCreate::showingMain] && $::jobCreate::showingMain} {
        displayCode [$::jobCreate::jobInfo getElementsByTagName main]
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
        displayCode [$doE getElementsByTagName main]
    }
    return true
}
###############################################################3
# Want to be able to show tagged regions and only edit those.

# Make text widgets readonly outside of specific tagged regions
# Note: This procedure is not in the jobCreate namespace because either WIDGET or
# WIDGET.internal kept ending up in that namespace. Both need to be in the global space
# and I couldn't figure out how to do that right off, other than putting the proc in
# the global namespace.
proc makeReadOnly {textwidget} {
    rename $textwidget $textwidget.internal
    proc $textwidget {args} [string map [list WIDGET $textwidget] {
        set cmd [lindex $args 0]
        if {$cmd eq "reallyDelete"} {
            return [eval WIDGET.internal delete [lrange $args 1 end]]
        } elseif {$cmd eq "reallyInsert"} {
            return [eval WIDGET.internal insert [lrange $args 1 end]]
        } elseif {$cmd eq "insert" || $cmd eq "delete"} {
            set tags  [list declareAnalysis declareReader allocateAnalysis preLoop preEvent postEvent postLoop]
            if {[WIDGET.internal tag ranges sel] ne ""} {
                foreach {sel1 sel2} [WIDGET.internal tag ranges sel] {break}
                set it1 [WIDGET.internal tag names $sel1]
                set it2 [WIDGET.internal tag names $sel2]
                if {[lsearch $it1 $it2] < 0 || [lsearch $tags $it2] < 0} {return}
            }
            set found false
            foreach t [WIDGET.internal tag names [WIDGET.internal index insert]] {
                if {[lsearch $tags $t] >= 0} {
                    set found true
                }
            }
            if {$found} {
                return [uplevel 1 WIDGET.internal $args]
            }
        } else {
            return [uplevel 1 WIDGET.internal $args]
        }
    }]
}

proc ::jobCreate::displayCode {node} {
    if {![winfo exists .fileText]} {
        toplevel .fileText
        set m [menu .fileText.menu]
        .fileText configure -menu $m
        set file [menu $m.file -postcommand [namespace code "checkFile $m.file .fileText.t"]]
        $m add cascade -label File -menu $file
        $file add command -label Save
        $file add command -label Revert
        $file add command -label Close -command [namespace code "wm iconify .fileText"]
        wm protocol .fileText WM_DELETE_WINDOW {wm iconify .fileText}
        set edit [menu $m.edit -postcommand [namespace code "checkUndo $m.edit .fileText.t"]]
        $m add cascade -label Edit -menu $edit
        $edit add command -label Undo      -command [namespace code "undo $m.edit .fileText.t"]
        $edit add command -label Redo      -command [namespace code "redo $m.edit .fileText.t"]
        $edit add command -label "Find..." -command [namespace code findDialog]

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
        bind .fileText.t <<Modified>> [namespace code "checkTextMod .fileText .fileText.t"]
        makeReadOnly .fileText.t
    }
    if {[.fileText.t edit modified]} {
        set title "Save edits for doEStruct.C?"
        set msg   "Do you want to save edits for doEStruct.C?" 
        set save [tk_messageBox -message $msg -type yesno \
                -icon question -title $title]
        if {$save} {
            save .fileText.t
        }
    }
    set ::jobCreate::showingMain false
    set ::jobCreate::showingXML  false
    if {[$node nodeName] eq "main"} {
        # Try to keep current position visible in file when minor changes
        # are made. This seems to assume that all other things we might
        # view in this window are small enough users won't scroll them.
        set yPos [.fileText.t yview]
        wm title .fileText "doEStruct.C - After substitutions."
        .fileText.t reallyDelete 0.0 end
        .fileText.t reallyInsert end [applyXsl doEStruct.xsl asText]
        set ::jobCreate::showingMain true
        .fileText.t yview moveto [lindex $yPos 0]

        # Tag the following nodes. Any edits made to these sections will be
        # stored back in the xml tree (if they are saved).
        .fileText.t configure -cursor crosshair
        set tagColors [list #bcd998 #fcae98 #7099b8 #1cd950 #d06348 #3c63c6]
        set nodeNames  [list declareAnalysis declareReader allocateAnalysis preLoop preEvent postEvent postLoop]
        set numLines [list 1 1 1 2 2 2 2]
        foreach name $nodeNames n $numLines {
            foreach {i1 i2} [getNodeTags .fileText.t $name] {break}
            .fileText.t tag configure $name -background $::jobCreate::tagColor($name)
            .fileText.t tag add $name $i1 $i2+${n}lines
            .fileText.t tag bind $name <Enter> {.fileText.t configure -cursor xterm}
            .fileText.t tag bind $name <Leave> {.fileText.t configure -cursor crosshair}
        }
    } elseif {[$node nodeName] eq "starSubmit"} {
        wm title .fileText "xml for star-submit - After substitutions."
        .fileText.t reallyDelete 0.0 end
        .fileText.t reallyInsert end [applyXsl job.xsl asXML]
        set ::jobCreate::showingXML true
    }
    .fileText.t edit reset
    .fileText.t edit modified false
    .fileText.menu.file entryconfigure Save   -state disabled \
            -command [namespace code "save   .fileText.t"]
    .fileText.menu.file entryconfigure Revert -state disabled \
            -command [namespace code "revert .fileText.t"]
    .fileText.menu.edit entryconfigure Undo -state disabled
    .fileText.menu.edit entryconfigure Redo -state disabled
    set ::jobCreate::undone 0
    ::jobCreate::highlightString $::jobCreate::findString
}
proc ::jobCreate::getNodeTags {w nodeName} {
    set n [$::jobCreate::jobInfo getElementsByTagName $nodeName]
    set iList [list]
    set start 1.0
    foreach line [split [$n asText] \n] {
        if {[string length [string trim $line]] > 0} {
            set start [$w search $line $start]
            lappend iList [$w search $line $start]
        }
    }
    set ind1 [lrange $iList 0 0]
    set ind2 [lrange $iList end end]
    return [list $ind1 $ind2]
}
proc ::jobCreate::chooseTagColor {w name} {
    set color [tk_chooseColor -initialcolor $::jobCreate::tagColor($name) -title "Color for $name in doEStruct.C"]
    if {$color ne ""} {
        set ::jobCreate::tagColor($name) $color
        $w configure -background $color
        .fileText.t tag configure $name -background $color
    }
}
proc  ::jobCreate::checkTextMod {w t} {
    if {[$t edit modified]} {
        wm title $w "doEStruct.C (modified) - After substitutions."
    } else {
        wm title $w "doEStruct.C - After substitutions."
    }
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
    if {$::jobCreate::showingXML} {
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
# This is now only possible for doEStruct.C macro display
# 1############################################################################
# This is now only called when doEStruct macro is edited.
# Six nodes can be edited via text widget. Save all of them.
proc ::jobCreate::save {t} {
    set nodeNames  [list declareAnalysis declareReader allocateAnalysis preLoop preEvent postEvent postLoop]
    foreach name $nodeNames {
        foreach {i1 i2} [$t tag ranges $name] {break}
        set txt [$t get $i1 $i2]
        set n [$::jobCreate::jobInfo getElementsByTagName $name]
        # I assume node has one TEXT_NODE child node
        # Need to rethink if this changes.
        foreach cn [$n childNodes] {
            if {[$cn nodeType] eq "TEXT_NODE"} {
                $cn nodeValue $txt
            }
        }
    }
    $t edit modified false
}
# 2 ############################################################################
proc ::jobCreate::revert {t} {
    set title [wm title .fileText]
    regsub {[a-zA-Z0-9]*: } $title {} title
    $t edit modified false
    displayCode [$::jobCreate::jobInfo getElementsByTagName main]
}
# 3 ############################################################################
proc ::jobCreate::checkUndo {m t} {
    if {$::jobCreate::showingXML} {
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

    set fE [frame $f.[string tolower $el 0 0]$::jobCreate::numberOfInstances($el)]
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
        displayCode $node
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
        displayCode $node
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
        displayCode $node
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
            foreach dir [glob -nocomplain $path/*/] {
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
        set title "Save edits for doEStruct.C?"
        set msg   "Do you want to save edits for [$::jobCreate::lastNode nodeName] before submitting job?" 
        set save [tk_messageBox -message $msg -type yesno \
                -icon question -title $title]
        if {$save} {
            save .fileText.t
        }
    }
    
    # Make directory structure for job files and output.
    set d [list]
    foreach dir [file split $path] {
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
    set hidden .$::env(STAR_HOST_SYS)
    if {![file exists $path/StRoot/$hidden]} {
        file mkdir $path/StRoot/$hidden
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
    # Might have Therminator Parameters
    set hNode [$::jobCreate::jobInfo getElementsByTagName therminatorParams]
    if {$hNode ne ""} {
        set f [open $path/scripts/therminator.in w]
        puts $f [applyXsl therminatorParams.xsl asText]
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

    # Make copy of source code.
    # This does not pick up object files.
    # If this is a problem we should modify how copy is done.
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    set srcDir [$node text]
    if {[catch {file copy $srcDir/StRoot/StEStructPool $path/StRoot}]} {
        tk_messageBox -message "Problem copying StRoot subdir in $srcDir" \
                -type ok \
                -icon question -title "Skipping copying source code"
    }
    # Want to copy compiled library. This lets users modify source code
    # while jobs that are waiting to run get the compiled code the way it was
    # at job submission.    
    if {[catch {file copy $srcDir/$hidden/lib $path/StRoot/$hidden}] ||
        [catch {file copy $srcDir/$hidden/obj $path/StRoot/$hidden}]} {
        tk_messageBox -message "Problem copying compiled code from $srcDir. We will try using $srcDir directly which means any changes you make could affect jobs waiting to run" \
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
    fileevent $fid readable [namespace code [list isReadable $fid $fLog]]
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
                         To submit  these jobs you may consider starting the jobMonitor. \
                         Also, don't forget to Filter filelist if you want uniform size jobs."
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
        grid .star-submit.x x
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
# filterLists will read the *.dataset file in the scripts directory and sort it
# chronologically, omitting runs and days with few events.
################################################################################
proc ::jobCreate::filterLists {} {
    set tf [checkListWindow]

    set pwd [pwd]
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    cd $path/scripts

    set tag [glob *.dataset]
    $tf.t insert end "About to filter $tag\n"

    set currCurs [$tf.t cget -cursor]
    $tf.t configure -cursor watch

    # Do the filtering, write some messages to $tf.t
    set dataset [open $tag]
    set nJobs -1
    set nFiles 0
    set nEvents 0
    set fileList [list]
    # Scan file for number of jobs as well as list of files.
    # Ignore files below a certain size.
    catch {unset run}
    catch {unset runDay}
    catch {unset runIndex}
    catch {unset fileLine}
    catch {unset fileNEvents}
    catch {unset rejectFile}
    while {[gets $dataset line] >= 0} {
        if {[string first :::::: $line] == 0} {
            incr nJobs
        } else {
            if {[regexp {([0-9]+):*$} $line m n] > 0} {
                if {$n < 50} {
                    lappend rejectFile $n
                    continue
                }
                set fileLine($nFiles) $line
                set fileNEvents($nFiles) $n
                lappend fileList "$n $nFiles"
                if {[regexp {_([0-9]+)_raw_([0-9]+)} $line m rn en]} {
                    # Assume the first number in the filename is yydddrrr
                    # where year can be one or two digits, day is three and run number is three
                    set d [string range $rn end-5 end-3]
                    lappend run($rn) "$n $nFiles"
                    if {[info exists runDay($rn)]} {
                        if {$runDay($rn) ne $d} {
                            %tf.t insert end "Found a run $rn that extends over multiple days: $d, $runDay($rn)"
                        }
                    }
                    set runDay($rn) $d
                } else {
                    $tf.t insert end "failed to parse $line"
                }
                incr nFiles
                incr nEvents $n
            }
        }
    }
    close $dataset

    # Create list of files for each day.
    # Ignore runs below a certain size (may have start up,
    # end of run or other issues for file to be so small)
    catch {unset day}
    catch {unset dayIndex}
    set rejectRun [list]
    foreach r [lsort [array names run]] {
        set n [::jobCreate::sumList $run($r)]
    #    $tf.f insert end "$r:  $n"
        set d $runDay($r)
        if {$n < 1000} {
            lappend rejectRun $r
        } else {
            lappend day($d) "$n [::jobCreate::reduceList $run($r) 1]"
        }
    }
    if {[llength $rejectRun] > 0} {
        $tf.t insert end "Rejecting the following runs as having too few events\n"
        foreach r $rejectRun {
            $tf.t insert end "    $r\n"
        }
    }

    # Calculate number of events per day.
    # Ignore days with few events.
    catch {unset period}
    set rejectDay [list]
    foreach d [lsort -index 0 [array names day]] {
        set n [::jobCreate::sumList $day($d)]
        $tf.t insert end "  day $d has $n events\n"
        if {$n < 20000} {
            lappend rejectDay $d
            lappend period 0
        } else {
            lappend period $n
        }
    }
    if {[llength $rejectDay] > 0} {
        $tf.t insert end "Rejecting the following days as having too few events\n"
        foreach d $rejectDay {
            $tf.t insert end "    $d\n"
        }
    }

    # We want to split files for each day into individual jobs, but
    # need to calculate how many jobs that will be.
    catch {unset dayJobs}
    catch {unset delta}
    set eventsPerJob [expr ([join $period +])/$nJobs]
    set i 0
    foreach ed $period {
        set nED [expr $ed/$eventsPerJob]
        if {$ed == 0} {
            lappend dayJobs 0
            lappend delta 0
        } elseif {$nED < 1} {
            lappend dayJobs 1
            lappend delta "0 $i"
        } else {
            lappend dayJobs $nED
            lappend delta "[expr (0.0+$ed)/$eventsPerJob-$nED] $i"
        }
        incr i
    }

    # Sum of numbers of jobs per day should be less than
    # total number of jobs we can have. Distribute extra
    # jobs to the biggest days.
    set delta [lsort -decreasing -index 0 $delta]
    set nDelta [expr $nJobs-([join $dayJobs +])]
    for {set i 0} {$i < $nDelta} {incr i} {
        set j [lindex $delta $i 1]
        set nD [lindex $dayJobs $j]
        incr nD
        set dayJobs [lreplace $dayJobs $j $j $nD]
    }

    catch {unset targetJobs}
    foreach nD $dayJobs nE $period {
        lappend targetJobs [expr $nE/($nD+0.01)]
    }

    # Now split each day into individual jobs.
    # Want files sorted by run number, then by sequence within the rin.
    set job -1
    catch {unset jobList}
    catch {unset dayList}
    catch {unset jobSum}
    foreach d [lsort -index 0 [array names day]] dJobs $dayJobs nT $targetJobs {
        if {$dJobs > 0} {
            # Create list of files for each run for this day
            catch {unset runSeq}
            foreach l [join [::jobCreate::reduceList $day($d) 1]] {
                if {[regexp {_([0-9]+)_raw_([0-9]+)} $fileLine($l) m rn en]} {
                    lappend runSeq($rn) "$en $l"
                }
            }
            # Sort each run according to sequence number, append all files for day
            set sum 0
            foreach rs [lsort [array names runSeq]] {
                foreach s [lsort -index 0 $runSeq($rs)] {
                    foreach {en l} $s {break}
                    regexp {([0-9]+):*$} $fileLine($l) m n
                    lappend dayList($d) "$fileLine($l) $n"
                    incr sum $n
                }
            }
            # Split into dJobs trying to keep number of events per job as nT
            incr job
            set nDayJobs 0
            set jobSum($job) 0
            set dayTot 0
            foreach j $dayList($d) {
                foreach {l n} $j {break}
                if {($nDayJobs < $dJobs-1) && ($n/2+$dayTot > $nT*($nDayJobs+1))} {
                    incr job
                    incr nDayJobs
                    set jobSum($job) 0
                }
                lappend jobList($job) $l
                incr jobSum($job) $n
                incr dayTot $n
            }
        }
    }

    # Now write out each jobList separated by = line.
    set dataset [open [file rootname $tag].newDataset w]
    puts $dataset ":::::::::::::::::::::::::::::::::::::::::::::::::::::::"
    for {set j 0} {$j <= $job} {incr j} {
        foreach l $jobList($j) {
            puts $dataset $l
        }
        puts $dataset ":::::::::::::::::::::::::::::::::::::::::::::::::::::::"
    }
    close $dataset

    # mv tag to oldDataset, newDataset to dataset.
    file rename $tag [file rootname $tag].oldDataset
    file rename [file rootname $tag].newDataset $tag

    $tf.t insert end "I have renamed $tag to [file rootname $tag].oldDataset"
    $tf.t insert end " and created a new version of the dataset."
    $tf.t insert end " If you see odd results you should check these files by hand.\n"

    $tf.t conf -cursor $currCurs
    $tf.b.action configure -text "finished filtering filelist"

    cd $pwd
}
proc ::jobCreate::checkListWindow {} {
    if {![winfo exists .filterList]} {
        toplevel .filterList
        grid [label  .filterList.l -text "Filtering file list input file"]
        text .filterList.t -yscrollcommand {.filterList.y set} \
                         -xscrollcommand {.filterList.x set} -wrap char \
                         -undo true
        scrollbar .filterList.y -command {.filterList.t yview}
        scrollbar .filterList.x -command {.filterList.t xview} -orient horizontal
        grid .filterList.t .filterList.y
        grid .filterList.x x
        grid .filterList.t -sticky news
        grid .filterList.y -sticky ns
        grid .filterList.x -sticky we
        grid [frame  .filterList.b]
        grid [button .filterList.b.action -text Cancel \
                -command "destroy .filterList"]
        grid columnconfigure .filterList 0 -weight 1
        grid rowconfigure    .filterList 1 -weight 1

        bind .filterList <Control-w> {destroy .filterList}

        return .filterList
    }
}
################################################################################
# startJobMonitor sources and start a program that monitors running and
# output of batch jobs. We assume it is in the same directory as jobCreate.
################################################################################
# Put jobMonitor into slave interpreters.
# Can start multiple job monitors from jobCreate and they don't interfere with each other.
proc ::jobCreate::startJobMonitor {} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]

    incr ::jobCreate::slaveInterpCount
    interp create slave$::jobCreate::slaveInterpCount
    # Slave interpreter will be create with its own environment.
    # Need to set auto_path on rcf or it won't find packages.
    if {[file exists /star/u/prindle/bin/lib]} {
        set i_auto_path [interp eval slave$::jobCreate::slaveInterpCount {set auto_path}]
        if {[lsearch $i_auto_path /star/u/prindle/bin/lib] < 0} {
            interp eval slave$::jobCreate::slaveInterpCount {
                lappend auto_path /star/u/prindle/bin/lib
            }
        }
    }
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
       $menu entryconfigure 3 -state normal
       $menu entryconfigure 4 -state normal
    } else {
       $menu entryconfigure 2 -state disabled
       $menu entryconfigure 3 -state disabled
       $menu entryconfigure 4 -state disabled
    }
}
################################################################################
# addHistograms parses output directory for histograms and adds them
# in hopefully appropriate ways.
# I have added a fair amount of user feed-back and control over histogram
# adding process. Not fully debugged at this point (Nov. 6, 2007)
################################################################################
#>>>>> Files to add box gives incorrect feedback.
#      Changed labels to indicate we are excluding files, but perhaps should actually
#      indicate new files to add, files already included, and files to ignore. Tri-state button?
proc ::jobCreate::addHistograms {} {
    if {![winfo exists .addHistograms]} {
        toplevel .addHistograms
        wm title .addHistograms "Adding histograms from job output"
        set ctl [frame .addHistograms.control]
        set dataFrame [labelframe .addHistograms.data -text "dataHists_M{cent}_*.root to add"]
        text .addHistograms.t -yscrollcommand {.addHistograms.y set} \
                              -xscrollcommand {.addHistograms.x set} -wrap word
        scrollbar .addHistograms.y -command {.addHistograms.t yview}
        scrollbar .addHistograms.x -command {.addHistograms.t xview} -orient horizontal
        grid $ctl -sticky w
        grid $dataFrame -sticky w
        grid .addHistograms.t .addHistograms.y
        grid .addHistograms.x x
        grid .addHistograms.t -sticky news
        grid .addHistograms.y -sticky ns
        grid .addHistograms.x -sticky we
        grid [frame  .addHistograms.b]
        set b1 [button .addHistograms.b.action -text "Add em up" -state disabled]
        set b2 [button .addHistograms.b.batch  -text "Add using batch" -state disabled]
        grid $b1 $b2
        grid columnconfigure .addHistograms 0 -weight 1
        grid rowconfigure    .addHistograms 1 -weight 1
        grid rowconfigure    .addHistograms 2 -weight 1
        .addHistograms.t tag configure input        -foreground black
        .addHistograms.t tag configure normalOutput -foreground blue
        .addHistograms.t tag configure errorOutput  -foreground red

        checkbutton $ctl.cbQA   -text "add QA histograms"    -variable ::jobCreate::addQAHistograms
        checkbutton $ctl.cbCuts -text "add Cuts histograms"  -variable ::jobCreate::addCutsHistograms
        pack $ctl.cbQA -anchor w
        pack $ctl.cbCuts -anchor w

        # Only want to allow destroying window when we have finished an atomic hadd.
        # Have to think about how to insure this.
        # bind .addHistograms <Control-w> {destroy .addHistograms}
    } else {
        set dataFrame .addHistograms.data
        .addHistograms.b.action configure -text "Add em up" -state disabled
        .addHistograms.b.batch  configure -text "Add using batch" -state disabled
        raise .addHistograms
    }
    set ::jobCreate::stopAddHistograms false

    foreach w [winfo children $dataFrame] {destroy $w}

    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName analysisType]
    set aType [$node text]

    set fileList [glob -nocomplain [file join $path data dataHists_*.root]]
    set top [file tail [lindex [lsort -dictionary $fileList] end]]
    if {$aType eq "StEStructCorrelation"} {
        if {![regexp {([0-9]+)_([0-9]+)} $top m nCents nJobs]} {
            .addHistograms.t insert end "Did not find a file that looked like \
                a correlation histogram file in directory \
                [file join $path data ]  \n" errorOutput
            .addHistograms.t see end
            return
        }
        incr nCents
        set nCentralities $nCents
        label $dataFrame.centrality  -text "Cent. bin"
        label $dataFrame.avgFileSize -text "Avg. file size"
        label $dataFrame.numPerSet   -text "Number per hadd"
        label $dataFrame.addedFiles  -text "Added files"
        label $dataFrame.filesToAdd  -text "Files to add"
        grid $dataFrame.centrality $dataFrame.avgFileSize $dataFrame.numPerSet \
             $dataFrame.addedFiles $dataFrame.filesToAdd -sticky w

        # Create one line per centrality for controlling how histograms are added.
        for {set i 0} {$i < $nCentralities} {incr i} {
            # If log file exists for this centrality scan it for histograms already added.
            set ::jobCreate::alreadyAdded($i) [list]
            set logFile [file join $path data addedFileNames${i}Log]
            if {[file exists $logFile]} {
                set fh [open $logFile]
                while {[gets $fh line] >= 0} {
                    set ll [split $line]
                    if {[lindex $line 0] eq "hadd"} {
                        foreach f [lrange $ll 2 end] {
                            # The match was dataHists_M(\d+)_(\d+).root
                            # This didn't work with my splitting script. Question is if we
                            # now allow undesirable matches.
                            if {[regexp {dataHists_M(\d+)_(.+).root} $f m c v]} {
                                if {$c == $i} {
                                    lappend ::jobCreate::alreadyAdded($i) $f
                                }
                            }
                        }
                    }
                }
                close $fh
            }

            # Get all files for centrality and remove already added and zero size files.
            set fileList [glob -nocomplain [file join $path data dataHists_M${i}_*.root]]
            set fileList [lremove $fileList $::jobCreate::alreadyAdded($i)]
            set zeros [list]
            foreach f $fileList {
                if {[file size $f] == 0} {
                    lappend zeros $f
                }
            }
            set fileList [lremove $fileList $zeros]
            set ::jobCreate::filesToAdd($i) [lsort -dictionary $fileList]

            # Calculate average file size to estimate how many we can combine at once.
            set nFiles [llength $fileList]
            set numPerSet 25
            set avgSize 0.0
            if {$nFiles > 0} {
                foreach f $fileList {
                    set avgSize [expr $avgSize + [file size $f]]
                }
                set avgSize [expr $avgSize/($nFiles*1024*1024)]
                if {$avgSize > 30.0} {
                    set numPerSet 10
                }
            }

            # Now add row of widgets to dataFrame.
            # Checkbox to add up this centrality.
            # Label showing average file size.
            # numberic entry for number of files to add.
            # button to pop up list of already added files
            # button to show (and deselect) files to be added.
            if {![info exists ::jobCreate::addCentrality($i)]} {
                set ::jobCreate::addCentrality($i) 1
                set ::jobCreate::numPerSet($i) $numPerSet
            }
            checkbutton $dataFrame.centrality$i  -text "bin $i" -variable ::jobCreate::addCentrality($i)
            label $dataFrame.avgFileSize$i -text [format %.2fMB $avgSize]
            entry $dataFrame.numPerSet$i   -textvariable ::jobCreate::numPerSet($i)
            button $dataFrame.addedFiles$i  -text [llength $::jobCreate::alreadyAdded($i)] \
                    -command [namespace code [list showAddedFiles $i]]
            button $dataFrame.filesToAdd$i  -text [llength $::jobCreate::filesToAdd($i)] \
                    -command [namespace code [list editFilesToAdd $i]]
            grid $dataFrame.centrality$i $dataFrame.avgFileSize$i $dataFrame.numPerSet$i \
                 $dataFrame.addedFiles$i $dataFrame.filesToAdd$i -sticky w

            if {[llength $::jobCreate::filesToAdd($i)] == 0} {
                set ::jobCreate::addCentrality($i) 0
                $dataFrame.centrality$i configure -selectcolor green
            }
        }
    } elseif {$aType eq "StEStructFluctuation"} {
        if {![regexp {_([0-9]+).} $top m nJobs]} {
            .addHistograms.t insert end "Did not find a file that looked like \
                a fluctuation histogram file in directory \
                [file join $path data ]  \n" errorOutput
            .addHistograms.t see end
            return
        }
        set nCentralities ""
        label $dataFrame.avgFileSize -text "Average file size"
        label $dataFrame.numPerSet   -text "number per hadd"
        label $dataFrame.addedFiles  -text "Previously added files"
        label $dataFrame.filesToAdd  -text "New files to add"
        grid $dataFrame.avgFileSize $dataFrame.numPerSet \
             $dataFrame.addedFiles  $dataFrame.filesToAdd -sticky w

        # If log file already exists scan it for already added files.
        set ::jobCreate::alreadyAdded [list]
        set logFile [file join $path data addedFileNamesLog]
        if {[file exists $logFile]} {
            set fh [open $logFile]
            while {[gets $fh line] >= 0} {
                set ll [split $line]
                if {[lindex $line 0] eq "hadd"} {
                    set ::jobCreate::alreadyAdded [concat $::jobCreate::alreadyAdded [lrange $ll 2 end]]
                }
            }
            close $fh
        }

        # Get all fluctuation files, remove already added and zero size files
        set fileList [glob -nocomplain [file join $path data dataHists_*.root]]
        set fileList [lremove $fileList $::jobCreate::alreadyAdded]
        set zeros [list]
        foreach f $fileList {
            if {[file size $f] == 0} {
                lappend zeros $f
            }
        }
        set fileList [lremove $fileList $zeros]
        set ::jobCreate::filesToAdd [lsort -dictionary $fileList]

        # Calculate average file size to estimate how many we can combine at once.
        set nFiles [llength $fileList]
        set numPerSet 25
        set avgSize 0.0
        if {$nFiles > 0} {
            foreach f $fileList {
                set avgSize [expr $avgSize + [file size $f]]
            }
            set avgSize [expr $avgSize/($nFiles*1024*1024)]
            if {$avgSize > 30.0} {
                set numPerSet 10
            }
        }

        # Now add row of widgets to dataFrame.
        # Checkbox to add up this centrality.
        # Label showing average file size.
        # numberic entry for number of files to add.
        # button to pop up list of already added files
        # button to show (and deselect) files to be added.
        if {![info exists ::jobCreate::addCentrality]} {
            set ::jobCreate::addCentrality 1
            set ::jobCreate::numPerSet 1
        }
        checkbutton $dataFrame.centrality0  -text "bin 0" -variable ::jobCreate::addCentrality
        label $dataFrame.avgFileSize0 -text  [format %.2fMB $avgSize]
        entry $dataFrame.numPerSet0   -textvariable ::jobCreate::numPerSet
        button $dataFrame.addedFiles0  -text [llength $::jobCreate::alreadyAdded] \
                 -command [namespace code [list showAddedFiles 0]]
        button $dataFrame.filesToAdd-  -text [llength $::jobCreate::filesToAdd] \
                 -command [namespace code [list editFilesToAdd 0]]
        grid $dataFrame.centrality0 $dataFrame.avgFileSize0 $dataFrame.numPerSet0 \
             $dataFrame.addedFiles0 $dataFrame.filesToAdd0 -sticky w
    }
    .addHistograms.b.action configure -state normal \
            -command [namespace code [list startAddingHistograms $nCentralities]]
    .addHistograms.b.batch configure -state normal \
            -command [namespace code [list submitAddingHistograms $nCentralities]]
}
################################################################################
# showAddedFiles simply shows the files that have already been added.
################################################################################
proc ::jobCreate::showAddedFiles {centrality} {
    if {![winfo exists .showAddedFiles]} {
        toplevel .showAddedFiles
        text .showAddedFiles.t -yscrollcommand {.showAddedFiles.y set} \
                              -xscrollcommand {.showAddedFiles.x set} -wrap word
        scrollbar .showAddedFiles.y -command {.showAddedFiles.t yview}
        scrollbar .showAddedFiles.x -command {.showAddedFiles.t xview} -orient horizontal
        grid .showAddedFiles.t .showAddedFiles.y
        grid .showAddedFiles.x x
        grid .showAddedFiles.t -sticky news
        grid .showAddedFiles.y -sticky ns
        grid .showAddedFiles.x -sticky we
        grid columnconfigure .showAddedFiles 0 -weight 1
        grid rowconfigure    .showAddedFiles 0 -weight 1
        grid rowconfigure    .showAddedFiles 1 -weight 1
        bind .showAddedFiles <Control-w> {destroy .showAddedFiles}
    } else {
        .showAddedFiles.t delete 0.0 end
        raise .showAddedFiles
    }
    if {$centrality ne ""} {
        wm title .showAddedFiles "Files that have already been summed for centrality $centrality"
        foreach f $::jobCreate::alreadyAdded($centrality) {
            .showAddedFiles.t insert end "[file tail $f]\n"
        }
    } else {
        wm title .showAddedFiles "Files that have already been summed"
        foreach f $::jobCreate::alreadyAdded {
            .showAddedFiles.t insert end "[file tail $f]\n"
        }
    }
}
################################################################################
# editFilesToAdd allows one to remove files from list to add.
################################################################################
proc ::jobCreate::editFilesToAdd {centrality} {
    if {![winfo exists .editFilesToAdd]} {
        toplevel .editFilesToAdd
        set sw  [ScrolledWindow .editFilesToAdd.sw -relief sunken -borderwidth 2]
        set sff [ScrollableFrame .editFilesToAdd.sw.f]
        $sw setwidget $sff
        set sf  [$sff getframe]
        pack $sw -fill both -expand true
        bind .editFilesToAdd <Control-w> {destroy .editFilesToAdd}
    } else {
        set sff .editFilesToAdd.sw.f
        set sf  [$sff getframe]
        foreach w [winfo child $sf] {
            destroy $w
        }
        raise .editFilesToAdd
    }
    if {$centrality ne ""} {
        wm title .editFilesToAdd "Files to ignore in sum for centrality $centrality"
        set i 0
        foreach f $::jobCreate::filesToAdd($centrality) {
            checkbutton $sf.f$i -text [file tail $f] -command [namespace code [list toggleFileToAdd $f $centrality]]
            pack $sf.f$i -anchor w
            incr i
        }
    } else {
        wm title .editFilesToAdd "Files that will be ignored"
        set i 0
        foreach f $::jobCreate::filesToAdd {
            checkbutton $sf.f$i -text [file tail $f] -command [namespace code [list toggleFileToAdd $f {}]]
            pack $sf.f$i -anchor w
            incr i
        }
    }
}
################################################################################
# toggleFileToAdd removes or adds file to list to be added
################################################################################
proc ::jobCreate::toggleFileToAdd {f centrality} {
    if {$centrality ne ""} {
        if {[lsearch $::jobCreate::filesToAdd($centrality) $f] >= 0} {
            set ::jobCreate::filesToAdd($centrality) [lremove $::jobCreate::filesToAdd($centrality) $f]
        } else {
            lappend ::jobCreate::filesToAdd($centrality) $f
            set ::jobCreate::filesToAdd($centrality) [lsort -dictionary $::jobCreate::filesToAdd($centrality)]
        }
    } else {
        if {[lsearch $::jobCreate::filesToAdd $f] >= 0} {
            set ::jobCreate::filesToAdd [lremove $::jobCreate::filesToAdd $f]
        } else {
            lappend ::jobCreate::filesToAdd $f
            set ::jobCreate::filesToAdd [lsort -dictionary $::jobCreate::filesToAdd]
        }
    }
}
################################################################################
# addHistograms parses output directory for histograms and adds them
# in hopefully appropriate ways. Need to open status window for
# user feedback since addition of data files can take a long time.
################################################################################
proc ::jobCreate::startAddingHistograms {nCentralities} {
    .addHistograms.b.action configure -text Cancel \
            -command {set ::jobCreate::stopAddHistograms true; \
                      .addHistograms.b.action configure -text "...waiting for current hadd to finish"}

    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName analysisType]
    set aType [$node text]

    # Add QA files
    if {$::jobCreate::addQAHistograms} {
        .addHistograms.t insert end "About to sum QA files\n" input
        .addHistograms.t see end
        set fileList [lsort -dictionary [glob -nocomplain [file join $path QA QA_*.root]]]
        set sumName [file join $path QA QA.root]
        set QALogFile [file join $path QA addedQALog]
        jobCreate::addSmallHistograms $fileList $sumName $QALogFile .addHistograms.control.cbQA
    }

    # Add cuts files
    if {$::jobCreate::addCutsHistograms  && !$::jobCreate::stopAddHistograms} {
        .addHistograms.t insert end "About to sum Cuts files\n" input
        .addHistograms.t see end
        set fileList [lsort -dictionary [glob -nocomplain [file join $path cuts cutHists_*.root]]]
        set sumName [file join $path cuts Cuts.root]
        set cutsLogFile [file join $path cuts addedCutsLog]
        jobCreate::addSmallHistograms $fileList $sumName $cutsLogFile .addHistograms.control.cbCuts
    }

    # Now for data histograms we have given the user some control 
    # over what to add. Use variables 
    if {$nCentralities eq ""} {
        if {$::jobCreate::addCentrality  && !$::jobCreate::stopAddHistograms} {
            .addHistograms.t insert end "About to sum Fluctuation files\n" input
            .addHistograms.t see end
            set fl $::jobCreate::filesToAdd
            set nPerSet $::jobCreate::numPerSet
            addDataHistograms $fl $nPerSet {}
        }
    } else {
        for {set i 0} {$i < $nCentralities} {incr i} {
            if {$::jobCreate::addCentrality($i)  && !$::jobCreate::stopAddHistograms} {
                .addHistograms.t insert end "About to sum Correlation files for centrality $i\n" input
                .addHistograms.t see end
                set fl $::jobCreate::filesToAdd($i)
                set nPerSet $::jobCreate::numPerSet($i)
                addDataHistograms $fl $nPerSet $i
            }
        }
    }
    if {$::jobCreate::stopAddHistograms} {
        .addHistograms.t insert end "Aborted adding histogram files\n" input
        .addHistograms.t see end
        .addHistograms.b.action configure -state normal -text "Continue adding" \
                -command [namespace code [list startAddingHistograms $nCentralities]]
        set ::jobCreate::stopAddHistograms false
        return
    } else {
        .addHistograms.b.action configure -text Done -command {} -state disabled
    }
}
################################################################################
# submitAddHistograms parses output directory for histograms, creates *.csh
# scripts to add them then submits those scripts.
# Think I can do SGE (via qsub now). Need to figure out Condor next.
#>>>>> Need to check if qsub is available and use condor if it is not.
#>>>>> I don't know how to tell condor to wait until previous jobs are finished,
#>>>>>  but I am pretty sure it is possible.
################################################################################
proc ::jobCreate::submitAddingHistograms {nCentralities} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName analysisType]
    set aType [$node text]

    # Add QA files
    if {$::jobCreate::addQAHistograms} {
        set fileList  [lsort -dictionary [glob -nocomplain [file join $path QA QA_*.root]]]
        set sumName   [file join $path QA   QA.root]
        set QALogFile [file join $path QA   addedQALog]
        set batchLog  [file join $path logs HAddQA.log]

        .addHistograms.t insert end "About to sum QA files\n" input
        .addHistograms.t see end
        .addHistograms.t insert end "hadd -f $sumName $fileList\n" input
        .addHistograms.t see end

        set scriptFile [file join $path scripts schedHAddQA.csh]
        set script [open $scriptFile w]
        if {[catch {exec qstat -u $::env(USER)}]} {
            set condorFile [file join $path scripts schedHAddQA.condor]
            set condorLog  [file join $path scripts schedHAddQA.condor.log]
            set condorDir  [file join $path scripts]
            ::jobCreate::createCondorFile $condorFile $scriptFile $batchLog $condorLog $condorDir

            puts $script "#!/bin/csh"
            puts $script "# condor_submit $condorFile"
            puts $script "cd [file join $path QA]"
            puts $script "hadd -f $sumName $fileList"
            close $script
            file attributes $scriptFile -permissions +x
            catch {exec condor_submit $condorFile} res
        } else {
            puts $script "#!/bin/csh"
            puts $script "# qsub -o $batchLog -j y $scriptFile"
            puts $script "cd [file join $path QA]"
            puts $script "hadd -f $sumName $fileList"
            close $script
            set res [exec qsub -j y -o $batchLog $scriptFile]
        }
    }
    .addHistograms.control.cbQA configure -selectcolor blue

    # Add Cuts files
    if {$::jobCreate::addCutsHistograms} {
        set fileList    [lsort -dictionary [glob -nocomplain [file join $path cuts cutHists_*.root]]]
        set sumName     [file join $path cuts Cuts.root]
        set CutsLogFile [file join $path cuts addedCutsLog]
        set batchLog    [file join $path logs HAddCuts.log]

        .addHistograms.t insert end "About to sum cuts files\n" input
        .addHistograms.t see end
        .addHistograms.t insert end "hadd -f $sumName $fileList\n" input
        .addHistograms.t see end

        set scriptFile [file join $path scripts schedHAddCuts.csh]
        set script [open $scriptFile w]
        if {[catch {exec qstat -u $::env(USER)}]} {
            set condorFile [file join $path scripts schedHAddCuts.condor]
            set condorLog  [file join $path scripts schedHAddCuts.condor.log]
            set condorDir  [file join $path scripts]
            ::jobCreate::createCondorFile $condorFile $scriptFile $batchLog $condorLog $condorDir

            puts $script "#!/bin/csh"
            puts $script "# condor_submit $condorFile"
            puts $script "cd [file join $path cuts]"
            puts $script "hadd -f $sumName $fileList"
            close $script
            file attributes $scriptFile -permissions +x
            catch {exec condor_submit $condorFile} res
        } else {
            puts $script "#!/bin/csh"
            puts $script "# qsub -o $batchLog -j y $scriptFile"
            puts $script "cd [file join $path cuts]"
            puts $script "hadd -f $sumName $fileList"
            close $script
            set res [exec qsub -j y -o $batchLog $scriptFile]
        }
    }
    .addHistograms.control.cbCuts configure -selectcolor blue

    # For data histograms we have given the user some control 
    # over what to add. Use variables 
    if {$nCentralities eq ""} {
        # This is for fluctuations. Need to think about how to do this in the current way.
        if {$::jobCreate::addCentrality  && !$::jobCreate::stopAddHistograms} {
            .addHistograms.t insert end "About to sum Fluctuation files\n" input
            .addHistograms.t see end
            set fl $::jobCreate::filesToAdd
            set nPerSet $::jobCreate::numPerSet
            addDataHistograms $fl $nPerSet {}
        }
    } else {
        for {set ic 0} {$ic < $nCentralities} {incr ic} {
            if {$::jobCreate::addCentrality($ic)} {
                .addHistograms.t insert end "Creating csh scripts for centrality $ic\n" input
                .addHistograms.t see end

                # For correlations we are adding up histograms from a single centrality.
                .addHistograms.data.centrality$ic configure -selectcolor orange

                # If files to be added are too big root will crash. I don't
                # know how to figure out how many files I can add at once.
                # Let user decide (although we suggested defaults.)

                set fileList $::jobCreate::filesToAdd($ic)
                set nFiles [llength $fileList]
                set numPerSet $::jobCreate::numPerSet($ic)
                set nSets [expr $nFiles/$numPerSet]
                if {$nFiles%$numPerSet} {
                    incr nSets
                }
                set nTmp 0
                set firstPass [list]
                set tmpFiles  [list]
                for {set j 0} {$j < $nSets} {incr j} {
                    set logFile [file join $path data addedFileNames${ic}Log]
                    if {[file exists $logFile]} {
                        set addLog [open $logFile a]
                    } else  {
                        set addLog [open $logFile w]
                    }
                    set start [expr $j*$numPerSet]
                    set end   [expr $start+$numPerSet-1]
                    if {$end>=$nFiles} {
                        set end [expr $nFiles-1]
                    }
                    set sumFile [file join $path data Data${ic}_${nTmp}.root]
                    if {$start == $end} {
                        # If group has single file simply copy it, don't bother with batch submission.
                        puts $addLog "file copy [lindex $fileList $start] $sumFile"
                        if {[catch {file copy [lindex $fileList $start] $sumFile} mess]} {
                            .addHistograms.t insert end $mess errorOutput
                            .addHistograms.t insert end "\n\n" errorOutput
                            puts $addLog "$mess"
                        } else {
                            .addHistograms.t insert end $mess normalOutput
                            .addHistograms.t insert end "\n\n" normalOutput
                        }
                        lappend tmpFiles $sumFile
                        lappend ::jobCreate::alreadyAdded($ic) [lindex $fileList $start]
                        set ::jobCreate::filesToAdd($ic) [lremove $::jobCreate::filesToAdd($ic) [lindex $fileList $start]]
                    } else {
                        # Group has more than one file.
                        # Submit batch job to do actual hadd.
                        set scriptFile [file join $path scripts schedHAdding_M${ic}_${nTmp}.csh]
                        set batchLog   [file join $path logs    HAdding_M${ic}_${nTmp}.log]
                        while {[file exists $scriptFile]} {
                            incr nTmp
                            set scriptFile [file join $path scripts schedHAdding_M${ic}_${nTmp}.csh]
                            set batchLog   [file join $path logs    HAdding_M${ic}_${nTmp}.log]
                        }
                        set cmd "hadd $sumFile [lrange $fileList $start $end]"
                        puts $addLog "$cmd"
                        set script [open $scriptFile w]
                        # If we have SGE command sqtat available use qsub.
                        # Otherwise we use condor (which requires another file)
                        if {[catch {exec qstat -u $::env(USER)}]} {
                            set condorFile [file join $path scripts schedHAdding_M${ic}_${nTmp}.condor]
                            set condorLog  [file join $path scripts schedHAdding_M${ic}_${nTmp}.condor.log]
                            set condorDir  [file join $path scripts]
                            ::jobCreate::createCondorFile $condorFile $scriptFile $batchLog $condorLog $condorDir

                            puts $script "#!/bin/csh"
                            puts $script "# condor_submit $condorFile"
                            puts $script "cd [file join $path data]"
                            puts $script "$cmd"
                            close $script
                            file attributes $scriptFile -permissions +x
                            catch {exec condor_submit $condorFile} res
                        } else {
                            puts $script "#!/bin/csh"
                            puts $script "# qsub -o $batchLog -j y $scriptFile"
                            puts $script "cd [file join $path data]"
                            puts $script "$cmd"
                            close $script
                            set res [exec qsub -j y -o $batchLog $scriptFile]
                        }
                        lappend tmpFiles $sumFile
                        lappend firstPass [lindex $res 2]

                        .addHistograms.t insert end "Created script for batch submission of histogram addition:\n" input
                        .addHistograms.t insert end "    $script\n" input
                        .addHistograms.t insert end "by hand run: $cmd\n" input
                        .addHistograms.t see end

                        set ::jobCreate::alreadyAdded($ic) [concat $::jobCreate::alreadyAdded($ic) [lrange $fileList $start $end]]
                        set ::jobCreate::filesToAdd($ic) [lremove $::jobCreate::filesToAdd($ic) [lrange $fileList $start $end]]
                    }
                    close $addLog
                    incr nTmp
                    .addHistograms.data.addedFiles$ic configure -text [llength $::jobCreate::alreadyAdded($ic)]
                    .addHistograms.data.filesToAdd$ic configure -text [llength $::jobCreate::filesToAdd($ic)]
                }

                # Now add all of those groups together (for each centrality)
                # Submit a batch job that waits for all the previous jobs to end.
                # Corner case include:
                #  Might not have added any files.
                #    firstPass and tmpFiles will be empty lists.
                #  Might have only had one file we attempted to add.
                #    This would have been renamed and firstPass will be empty while tmpFiles has one entry.
                #  Might have only submitted one hadd group.
                #    firstPass and tmpFiles will have one entry each. Can rename (in a batch job so we can wait for the hadd to finish.)
                set nTmp 0
                set scriptFile [file join $path scripts schedHAddSums_M${ic}_${nTmp}.csh]
                set batchLog   [file join $path logs    HAddSums_M${ic}_${nTmp}.log]
                while {[file exists $scriptFile]} {
                    incr nTmp
                    set scriptFile [file join $path scripts schedHAddSums_M${ic}_${nTmp}.csh]
                    set batchLog   [file join $path logs    HAddSums_M${ic}_${nTmp}.log]
                }
                if {(0 == [llength $firstPass]) && (0 == [llength $tmpFiles])} {
                    continue
                } else {
                    #
                    .addHistograms.data.centrality$ic configure -selectcolor blue
                    set logFile [file join $path data addedFileNames${ic}Log]
                    set addLog [open $logFile a]
                    set sumFile [file join $path data Data${ic}.root]

                    if {[file exists $sumFile]} {
                        set tmpName [file join $path data Data${ic}Tmp.root]
                        puts $addLog "file rename $sumFile $tmpName"
                        file rename $sumFile $tmpName
                        lappend tmpFiles $tmpName
                        .addHistograms.t insert end "file rename $sumFile $tmpName\n" input
                        .addHistograms.t see end
                    }
                    if {[llength $tmpFiles] > 1} {
                        set cmd "hadd $sumFile $tmpFiles"
                        puts $addLog "$cmd"
                        .addHistograms.t insert end "$cmd\n" input
                        .addHistograms.t see end
                    } else {
                        set cmd "/bin/mv $tmpFiles $sumFile"
                        puts $addLog "$cmd"
                        .addHistograms.t insert end "file rename $tmpFiles $sumFile\n" input
                        .addHistograms.t see end
                    }
                    set script [open $scriptFile w]
                    if {[catch {exec qstat -u $::env(USER)}]} {
                        set condorFile [file join $path scripts schedHAddSums_M${ic}_${nTmp}.condor]
                        set condorLog  [file join $path scripts schedHAddSums_M${ic}_${nTmp}.condor.log]
                        set condorDir  [file join $path scripts]
                        ::jobCreate::createCondorFile $condorFile $scriptFile $batchLog $condorLog $condorDir true

                        puts $script "#!/bin/csh"
                        puts $script "# condor_submit $condorFile"
                        puts $script "cd [file join $path data]"
                        puts $script "$cmd"
                        foreach tmp $tmpFiles {
                            puts $script "/bin/rm $tmp"
                        }
                        close $script
                        file attributes $scriptFile -permissions +x
                        catch {exec condor_submit $condorFile} res
                    } else {
                        puts $script "#!/bin/csh"
                        puts $script "# qsub -o $batchLog -j y -hold_jid [join $firstPass ,] $scriptFile"
                        puts $script "cd [file join $path data]"
                        puts $script "$cmd"
                        foreach tmp $tmpFiles {
                            puts $script "/bin/rm $tmp"
                        }
                        close $script
                        set res [exec qsub -o $batchLog -j y -hold_jid [join $firstPass ,] $scriptFile]
                    }
                    close $addLog
                }

                # Done with this centrality
                .addHistograms.data.centrality$ic configure -selectcolor blue
            }
        }
    }
}
################################################################################
# createCondorFile is utility to create file controlling codor batch submission.
# I just copy this from what SUMS does, I don't understand it yet.
################################################################################
proc ::jobCreate::createCondorFile {fileName scriptFile outPut log dir {hold false}} {
    set condor [open $fileName w]
    puts $condor "Universe       = vanilla"
    puts $condor "Notification   = never"
    puts $condor "GetEnv         = true"
    puts $condor ""
    puts $condor "Executable     = $scriptFile"
    puts $condor "Output         = $outPut"
    puts $condor "Error          = [string map {.log .err} $outPut]"
    puts $condor "Requirements   = (CPU_Experiment == \"star\")"
    puts $condor "Log            = $log"
    puts $condor "Initialdir     = $dir"
    if {$hold} {
        puts $condor "Hold     = true"
    }
    puts $condor ""
    puts $condor "+Experiment     = \"star\""
    puts $condor "Priority        = +10"
    puts $condor "+Job_Type       = \"cas\""
    puts $condor "PeriodicRemove  = (JobStatus == 2 && (CurrentTime - JobCurrentStartDate > (54000)) && ((RemoteUserCpu+RemoteSysCpu)/(CurrentTime-JobCurrentStartDate) < 0.10)) || (((CurrentTime - EnteredCurrentStatus) > (2*24*3600)) && JobStatus == 5) || (JobRunCount >= 1 && JobStatus == 1)"
    puts $condor "Queue"
    close $condor
}
################################################################################
# addSmallHistograms takes list from addHistograms and adds them together.
# This is usually quick, so don't look in previous log but just add everything up again.
################################################################################
proc ::jobCreate::addSmallHistograms {fileList sumName logName b} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]

    if {[file exists $logName]} {
        set log [open $logName a]
    } else  {
        set log [open $logName w]
    }
    $b configure -selectcolor yellow
    .addHistograms.t insert end "hadd $sumName $fileList\n" input
    .addHistograms.t see end

    set fid [open "|hadd -f $sumName $fileList"]
    fconfigure $fid -blocking false -buffering line
    fileevent $fid readable [namespace code [list addingReadable $fid $log]]
    set currCurs [.addHistograms.t cget -cursor]
    .addHistograms.t configure -cursor watch

    # Wait for the fileevents (i.e. hadd) to finish.
    vwait ::ADD-DONE

    # Close the pipe
    close $fid
    close $log
    $b configure -selectcolor green
    .addHistograms.t conf -cursor $currCurs
}
################################################################################
# addDataHistograms takes list from addHistograms and adds them together,
# keeping a log of what has already been added.
# For Fluctuation histograms centrality = "". Think everything just works.
################################################################################
proc ::jobCreate::addDataHistograms {fileList numPerSet centrality} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]

    # For correlations we are adding up histograms from a single centrality.
    # Need to add add files to the correct centrality log (hence the centrality argument.)

    # If user stopped adding of histograms by clicking on cancel button, then resumed
    # adding via ADDS menu there will be Data${centrality}_*.root files already exsiting.
    # The files that were added to create them will be properly accounted for in the
    # log files, so we want to include them in the grand sum.
    set tmpFiles [lsort -dictionary [glob -nocomplain [file join $path data Data${centrality}_*.root]]]
    set nTmp [llength $tmpFiles]

    .addHistograms.data.centrality$centrality configure -selectcolor yellow

    # If files to be added are too big root will crash. I don't
    # know how to figure out how many files I can add at once.
    # Let user decide (although we suggested defaults.)

    set nFiles [llength $fileList]
    set nSets [expr $nFiles/$numPerSet]
    if {$nFiles%$numPerSet} {
        incr nSets
    }
    for {set j 0} {$j < $nSets} {incr j} {
        if {$::jobCreate::stopAddHistograms} {
            return
        }
        set logFile [file join $path data addedFileNames${centrality}Log]
        if {[file exists $logFile]} {
            set addLog [open $logFile a]
        } else  {
            set addLog [open $logFile w]
        }
        set start [expr $j*$numPerSet]
        set end   [expr $start+$numPerSet-1]
        if {$end>=$nFiles} {
            set end [expr $nFiles-1]
        }
        set sumFile [file join $path data Data${centrality}_${nTmp}.root]
        if {$start == $end} {
            set currCurs [.addHistograms.t cget -cursor]
            .addHistograms.t configure -cursor watch
            puts $addLog "file copy [lindex $fileList $start] $sumFile"
            if {[catch {file copy [lindex $fileList $start] $sumFile} mess]} {
                .addHistograms.t insert end $mess errorOutput
                .addHistograms.t insert end "\n\n" errorOutput
                puts $addLog "$mess"
            } else {
                .addHistograms.t insert end $mess normalOutput
                .addHistograms.t insert end "\n\n" normalOutput
            }
            .addHistograms.t conf -cursor $currCurs
            lappend ::jobCreate::alreadyAdded($centrality) [lindex $fileList $start]
            set ::jobCreate::filesToAdd($centrality) [lremove $::jobCreate::filesToAdd($centrality) [lindex $fileList $start]]
        } else {
            set cmd "hadd $sumFile [lrange $fileList $start $end]"
            puts $addLog "$cmd"
            .addHistograms.t insert end "$cmd\n" input
            .addHistograms.t see end

            set fid [open "|$cmd"]
            fconfigure $fid -blocking false -buffering line
            fileevent $fid readable [namespace code [list addingReadable $fid $addLog]]
            set currCurs [.addHistograms.t cget -cursor]
            .addHistograms.t configure -cursor watch

            # Wait for the fileevents (i.e. hadd) to finish.
            vwait ::ADD-DONE

            # Close the pipe
            close $fid
            .addHistograms.t conf -cursor $currCurs
            set ::jobCreate::alreadyAdded($centrality) [concat $::jobCreate::alreadyAdded($centrality) [lrange $fileList $start $end]]
            set ::jobCreate::filesToAdd($centrality) [lremove $::jobCreate::filesToAdd($centrality) [lrange $fileList $start $end]]
        }
        close $addLog
        lappend tmpFiles $sumFile
        incr nTmp
        .addHistograms.data.addedFiles$centrality configure -text [llength $::jobCreate::alreadyAdded($centrality)]
        .addHistograms.data.filesToAdd$centrality configure -text [llength $::jobCreate::filesToAdd($centrality)]
    }

    # Now add all of those groups together (for each centrality)
    if {$::jobCreate::stopAddHistograms} {
        return
    }
    if {$nTmp > 0} {
        .addHistograms.data.centrality$centrality configure -selectcolor orange
        set logFile [file join $path data addedFileNames${centrality}Log]
        set addLog [open $logFile a]
        set sumFile [file join $path data Data${centrality}.root]

        if {[file exists $sumFile]} {
            set tmpName [file join $path data Data${centrality}Tmp.root]
            puts $addLog "file rename $sumFile $tmpName"
            file rename $sumFile $tmpName
            lappend tmpFiles $tmpName
            incr nTmp
            .addHistograms.t insert end "file rename $sumFile $tmpName\n" input
            .addHistograms.t see end
        }
        if {[llength $tmpFiles] > 1} {
            set cmd "hadd $sumFile $tmpFiles"
            puts $addLog "$cmd"
            .addHistograms.t insert end "$cmd\n" input
            .addHistograms.t see end

            set fid [open "|$cmd"]
            fconfigure $fid -blocking false -buffering line
            fileevent $fid readable [namespace code [list addingReadable $fid $addLog]]
            set currCurs [.addHistograms.t cget -cursor]
            .addHistograms.t configure -cursor watch

            # Wait for the fileevents (i.e. hadd) to finish.
            vwait ::ADD-DONE
            close $fid
            .addHistograms.t conf -cursor $currCurs
        } else {
            puts $addLog "file rename $tmpFiles $sumFile"
            .addHistograms.t insert end "file rename $tmpFiles $sumFile\n" input
            .addHistograms.t see end
            file rename $tmpFiles $sumFile
        }
        close $addLog
        foreach tmp $tmpFiles {
            catch {file delete $tmp}
        }
    }

    # Done with this centrality
    .addHistograms.data.centrality$centrality configure -selectcolor green
}
################################################################################
# addingReadable waits for output from various hadd commands and
#  puts it into a text widget.
################################################################################
proc ::jobCreate::addingReadable {fid fLog} {
    # The channel is readable; try to read it.
    set status [catch { gets $fid line } result]
    if { $status != 0 } {
        # Error on the channel
        puts $fLog ">>>>>error reading $fid: $result<<<<<"
        .addHistograms.t insert end "error reading $fid: $result\n"
        .addHistograms.t see end
        set ::ADD-DONE 2
    } elseif { [eof $fid] } {
        set ::ADD-DONE 1
    } elseif { $result >= 0 } {
        puts $fLog $line
        .addHistograms.t insert end "$line\n"
        .addHistograms.t see end
    } elseif { [fblocked $fid] } {
        # Read blocked.  Just return
    } else {
        # Something else
        puts $fLog ">>>>>Something impossible happened while reading output from hadd<<<<<"
        .addHistograms.t insert end "What did you do?? the impossible happened while reading $fid: $result\n"
        .addHistograms.t see end
        set ::ADD-DONE 3
    }
}
################################################################################
# combineCentralities takes Datan{0}.root through Datan{m}.root and combines them
# into one file called Sum{n1}_{nm}.root (we are assuming n1, n2, ... nm are consecutive
# using this naming convention, although they do not have to be.)
# Histograms that depend on z-vertex bins are renamed to have distinct names.
# The support code that combines histograms to make \Delta\rho/\sqrt(\rho) knows
# how to combine these.
#
# This proc only makes sense for Correlations (menu should be disabled for fluctuations.)
################################################################################
proc ::jobCreate::combineCentralities {} {
    if {![winfo exists .combineCentralities]} {
        set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
        set path [$node text]

        set fileList [list]
        foreach f [glob -nocomplain [file join $path data Data*.root]] {
            set fn [file tail $f]
            if {[regexp {Data(\d+).root} $fn ic]} {
                lappend fileList $f
            }
        }
        if {[llength $fileList] == 0} {
            tk_messageBox -type ok -icon warning \
                    -message "Did not find and Data{n}.root files. Perhaps you need to Add histograms first?"
             return
        }
        set cents [list]
        set fileList [lsort -dictionary $fileList]
        foreach f $fileList {
            set fn [file tail $f]
            regexp {Data(\d+).root} $fn match ic
            lappend cents $ic
        }

        toplevel .combineCentralities
        wm title .combineCentralities "Combine centralities"
        pack [labelframe .combineCentralities.found -text "Existing sum files"] -anchor w
        set sumFrame [labelframe .combineCentralities.centralities -text "Sum files to create"]
        pack $sumFrame -anchor w
        set tFrame [frame .combineCentralities.tFrame]
        pack $tFrame -fill both -expand true

        text .combineCentralities.tFrame.t -yscrollcommand {.combineCentralities.tFrame.y set} \
                              -xscrollcommand {.combineCentralities.tFrame.x set} -wrap word
        scrollbar .combineCentralities.tFrame.y -command {.combineCentralities.tFrame.t yview}
        scrollbar .combineCentralities.tFrame.x -command {.combineCentralities.tFrame.t xview} -orient horizontal
        grid .combineCentralities.tFrame.t .combineCentralities.tFrame.y
        grid .combineCentralities.tFrame.t -sticky news
        grid .combineCentralities.tFrame.y -sticky ns
        grid .combineCentralities.tFrame.x x -sticky we
        grid columnconfigure .combineCentralities.tFrame 0 -weight 1
        grid rowconfigure    .combineCentralities.tFrame 0 -weight 1

        .combineCentralities.tFrame.t tag configure input -foreground black
        .combineCentralities.tFrame.t tag configure normalOutput -foreground blue
        .combineCentralities.tFrame.t tag configure errorOutput -foreground red

        button .combineCentralities.addSum -text "Another sum file?" \
            -command [namespace code [list addSumFileLine $fileList $cents]]
        button .combineCentralities.addEm -text "Add em up" -state disabled \
            -command [namespace code [list sumFilesUp]]
        button .combineCentralities.batchAddEm -text "Add in batch" -state disabled \
            -command [namespace code [list submitSumFilesUp]]
        pack .combineCentralities.addSum .combineCentralities.addEm .combineCentralities.batchAddEm -side left

        # Create labels for previously summed files.
        set found [list]
        foreach f [glob -nocomplain [file join $path data Sum*.root]] {
            set fn [file tail $f]
            if {[regexp {Sum(\d+_\d+).root} $fn ic]} {
                lappend found [file rootname $fn]
            }
        }
        set found [lsort -dictionary $found]
        foreach fn $found {
            pack [label .combineCentralities.found.l$fn -text $fn] -side left
        }

        # Labels for centralities of Data{n} files
        label $sumFrame.sumFile  -text "Summed file"
        set labs [list]
        set nCentralities [llength $cents]
        for {set ic 0} {$ic < $nCentralities} {incr ic} {
            lappend labs [label $sumFrame.cent$ic -text "cent. $ic"]
        }
        eval grid $sumFrame.sumFile x $labs

        # Create a line for specifying centralities to sum.
        set n 1
        if {[info exists ::jobCreate::numberSumFiles]} {
            set n $::jobCreate::numberSumFiles
        }
        set ::jobCreate::numberSumFiles 0
        # May have opened window, modified widgets, then destroyed it.
        # Want to re-open to reflect previous state.
        for {set i 0} {$i < $n} {incr i} {
            addSumFileLine $fileList $cents
        }
    } else {
        raise .combineCentralities
    }
    set state disabled
    for {set j 0} {$j < $::jobCreate::numberSumFiles} {incr j} {
        if {[llength $::jobCreate::sumFileNumbers($j)] > 0} {
            set state normal
        }
    }
    .combineCentralities.addEm configure -state $state
    .combineCentralities.batchAddEm configure -state $state
}
################################################################################
# addSumFileLine creates a line of buttons to indicate which centralities
# should be added together.
################################################################################
proc ::jobCreate::addSumFileLine {centFiles centList} {
    set j $::jobCreate::numberSumFiles
    set sumFrame .combineCentralities.centralities
    if {![info exists ::jobCreate::sumFileName($j)]} {
        set ::jobCreate::sumFileName($j) "Sum"
    }
    label $sumFrame.sumFile$j -textvariable ::jobCreate::sumFileName($j)
    label $sumFrame.space$j -text "<-"
    if {![info exists ::jobCreate::sumFileList($j)]} {
        set ::jobCreate::sumFileList($j) [list]
        set ::jobCreate::sumFileNumbers($j) [list]
    }
    set cbs [list]
    foreach ic $centList fn $centFiles {
        if {![info exists ::jobCreate::sumFileCB($j,$ic)]} {
            set ::jobCreate::sumFileCB($j,$ic) 0
        }
        lappend cbs [checkbutton $sumFrame.cb${j}${ic} -text $ic -variable ::jobCreate::sumFileCB($j,$ic) \
                -command [namespace code [list includeSumFile $centList $j $ic $fn]]]
    }
    eval grid $sumFrame.sumFile$j $sumFrame.space$j $cbs
    incr ::jobCreate::numberSumFiles
}
################################################################################
# includeSumFile appends or removes a file from the list to be summed up.
################################################################################
proc ::jobCreate::includeSumFile {centList sFile ic fn} {
    if {$::jobCreate::sumFileCB($sFile,$ic)} {
        lappend ::jobCreate::sumFileList($sFile) $fn
        lappend ::jobCreate::sumFileNumbers($sFile) $ic
    } else {
        set ::jobCreate::sumFileList($sFile) [lremove $::jobCreate::sumFileList($sFile) $fn]
        set ::jobCreate::sumFileNumbers($sFile) [lremove $::jobCreate::sumFileNumbers($sFile) $ic]
    }

    set state disabled
    for {set j 0} {$j < $::jobCreate::numberSumFiles} {incr j} {
        if {[llength $::jobCreate::sumFileNumbers($j)] > 0} {
            set state normal
        }
    }

    set oList [lsort -dictionary $::jobCreate::sumFileNumbers($sFile)]
    set a [lindex $oList 0]
    set b [lindex $oList end]
    set ::jobCreate::sumFileName($sFile) "Sum${a}_$b"
    .combineCentralities.addEm configure -state $state
    .combineCentralities.batchAddEm configure -state $state
}
################################################################################
# submitSumFilesUp uses information from ::jobCreate::sumFileList to combine histogram centrality files.
# Create a csh script and submit to a batch node.
################################################################################
proc ::jobCreate::submitSumFilesUp {} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    # Assume addCentralities.C script is under the localDir job was submitted from.
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    set lDir [$node text]

    set logFile [file join $path data sumCentralitiesLog]
    if {[file exists $logFile]} {
        set sumLog [open $logFile a]
    } else  {
        set sumLog [open $logFile w]
    }

    for {set j 0} {$j < $::jobCreate::numberSumFiles} {incr j} {
        set outFile $::jobCreate::sumFileName($j)
        set numFiles [llength $::jobCreate::sumFileNumbers($j)]

        set nTmp 0
        set combineC   [file join $path scripts Combine_${j}_${nTmp}.C]
        set combinecsh [file join $path scripts schedCombine_${j}_${nTmp}.csh]
        set combineLog [file join $path logs         Combine_${j}_${nTmp}.log]
        while {[file exists $combineC]} {
            incr nTmp
            set combineC   [file join $path scripts Combine_${j}_${nTmp}.C]
            set combinecsh [file join $path scripts schedCombine_${j}_${nTmp}.csh]
            set combineLog [file join $path logs Combine_${j}_${nTmp}.log]
        }
        set C   [open $combineC w]
        set csh [open $combinecsh w]

        puts $sumLog "Creating C macro and csh script to combine centralities."
        puts $sumLog "Want to run following commands."
        puts $sumLog "    gROOT->LoadMacro(\"addCentralities.C\");"
        puts $sumLog "    int inFile\[\] = {[join $::jobCreate::sumFileNumbers($j) ,]};"
        puts $sumLog "    addCentralities(\"[file join $path data]\",\"Data\",\"$outFile\",inFile,$numFiles);"

        puts $C "void Combine_${j}_${nTmp} \(\) {"
        puts $C "    gROOT->LoadMacro(\"addCentralities.C\");"
        puts $C "    int inFile\[\] = {[join $::jobCreate::sumFileNumbers($j) ,]};"
        puts $C "    addCentralities(\"[file join $path data]\",\"Data\",\"$outFile\",inFile,$numFiles);"
        puts $C "};"
        close $C

#>>>>> Should be easy to check if qsub is available and use condor if it is not.
        if {[catch {exec qstat -u $::env(USER)}]} {
            set condorFile [file join $path scripts schedCombine_${j}_${nTmp}.condor]
            set condorLog  [file join $path scripts schedCombine_${j}_${nTmp}.condor.log]
            set condorDir  [file join $path scripts]
            ::jobCreate::createCondorFile $condorFile $combinecsh $combineLog $condorLog $condorDir

            puts $csh "#!/bin/csh"
            puts $csh "# condor_submit $condorFile"
            puts $csh "cd $lDir"
            puts $csh "root4star -q -b [file join $path scripts Combine_${j}_${nTmp}.C\\(\\)]"
            close $csh
            file attributes $combinecsh -permissions +x
            catch {exec condor_submit $condorFile}
        } else {
            puts $csh "#!/bin/csh"
            puts $csh "# qsub -o $combineLog -j y $combinecsh"
            puts $csh "cd $lDir"
            puts $csh "root4star -q -b [file join $path scripts Combine_${j}_${nTmp}.C\\(\\)]"
            close $csh
            exec qsub -o $combineLog -j y $combinecsh
        }

        foreach nf $::jobCreate::sumFileNumbers($j) {
            .combineCentralities.centralities.cb${j}${nf} configure -selectcolor blue
        }
    }
    close $sumLog
}
################################################################################
# sumFilesUp uses information from ::jobCreate::sumFileList to combine histogram centrality files.
################################################################################
proc ::jobCreate::sumFilesUp {} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [file join [$node text] data]
    # Assume addCentralities.C script is under the localDir job was submitted from.
    set pwd [pwd]
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    cd [$node text]

    set logFile [file join $path sumCentralitiesLog]
    if {[file exists $logFile]} {
        set sumLog [open $logFile a]
    } else  {
        set sumLog [open $logFile w]
    }

    set root [open "|root -l -b" r+]
    fconfigure $root -blocking false -buffering line -buffersize 1
    fileevent  $root readable [namespace code [list summingReadable $root $sumLog]]
    set currCurs [.combineCentralities.tFrame.t cget -cursor]
    .combineCentralities.tFrame.t configure -cursor watch
    for {set j 0} {$j < $::jobCreate::numberSumFiles} {incr j} {
        set outFile $::jobCreate::sumFileName($j)
        set numFiles [llength $::jobCreate::sumFileNumbers($j)]

        puts $sumLog "gROOT->LoadMacro(\"addCentralities.C\");"
        puts $sumLog "int inFile\[\] = {[join $::jobCreate::sumFileNumbers($j) ,]};"
        puts $sumLog "addCentralities(\"$path\",\"Data\",\"$outFile\",inFile,$numFiles);"

        puts $root "cout << \"PLEASE COLOR ME $j yellow\" << endl;"
        puts $root "gROOT->LoadMacro(\"addCentralities.C\");"
        puts $root "int inFile\[\] = {[join $::jobCreate::sumFileNumbers($j) ,]};"
        puts $root "addCentralities(\"$path\",\"Data\",\"$outFile\",inFile,$numFiles);"
        puts $root "cout << \"PLEASE COLOR ME $j green\" << endl;"
    }
    # I don't know how to get root to close the pipe by telling it to quit.
    # Instead make root ask for mercy.
    puts $root "cout << \"PLEASE KILL ME\" << endl;"

    vwait ::SUM-DONE
    # Not sure closing the root process really kills it.
    close $root
    close $sumLog
    .combineCentralities.tFrame.t conf -cursor $currCurs
    .combineCentralities.addEm configure -text "Done" -state disabled
    cd $pwd
}
################################################################################
# summingReadable waits for output from a root process and
#  puts it into a text widget.
################################################################################
proc ::jobCreate::summingReadable {fid fLog} {
    # The channel is readable; try to read it.
    set status [catch { gets $fid line } result]
    if { $status != 0 } {
        # Error on the channel
        puts $fLog ">>>>>error reading $fid: $result<<<<<"
        .combineCentralities.tFrame.t insert end "error reading $fid: $result\n"
        .combineCentralities.tFrame.t see end
        set ::SUM-DONE 2
    } elseif {[eof $fid]} {
        set ::SUM-DONE 1
    } elseif {$result >= 0} {
        if {$line eq "PLEASE KILL ME"} {
            set ::SUM-DONE 5
            return
        } elseif {[string first "PLEASE COLOR ME " $line] == 0} {
            foreach {j color} [string map {"PLEASE COLOR ME " ""} $line] {break}
            foreach nf $::jobCreate::sumFileNumbers($j) {
                .combineCentralities.centralities.cb${j}${nf} configure -selectcolor $color
            }
            if {![winfo exists .combineCentralities.found.l$::jobCreate::sumFileName($j)]} {
                pack [label .combineCentralities.found.l$::jobCreate::sumFileName($j) -text $::jobCreate::sumFileName($j)] -side left
            }
            return
        } else {
            puts $fLog $line
            .combineCentralities.tFrame.t insert end "$line\n"
            .combineCentralities.tFrame.t see end
        }
    } elseif { [fblocked $fid] } {
        set ::SUM-DONE 4
        # Read blocked.  Just return
    } else {
        # Something else
        puts $fLog ">>>>>Something impossible happened while reading output from hadd<<<<<"
        .combineCentralities.tFrame.t insert end "What did you do?? the impossible happened while reading $fid: $result\n"
        .combineCentralities.tFrame.t see end
        set ::SUM-DONE 3
    }
}
################################################################################
# selectAll executes the appropriate version of selectAll for the
# mode. Only available if analysisType is StEStructCorrelation.
# Looks for Data{n} and Sum{n1}_{n2} files to operate on.
################################################################################
proc ::jobCreate::selectAll {} {
    if {![winfo exists .selectAll]} {
        set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
        set mode [$node text]
        toplevel .selectAll
        wm title .selectAll "Run Select macro on selected histogram files"

        # Create menu bar for this window
        set m [menu .selectAll.menu]
        .selectAll configure -menu $m
        set file [menu $m.file]
        # File menu.
        $m add cascade -label File -menu $file
        $file add command -label "Scan directories" -command [namespace code scanSelectAll]
        #
        # Option to combine jobs
        #
        labelframe .selectAll.combineJobs -text "combine jobs"
        pack .selectAll.combineJobs -fill x
        ArrowButton .selectAll.combineJobs.ab -dir right -command [namespace code "toggleDisplay .selectAll.combineJobs"]
        pack .selectAll.combineJobs.ab -side left -fill x -anchor nw
        frame .selectAll.combineJobs.f
        set f1 [frame .selectAll.combineJobs.f.dirs]
        set f2 [frame .selectAll.combineJobs.f.actions]
        pack $f1 -fill both -expand true
        pack $f2 -fill x -expand true
        label       $f1.job1_label    -text "job 1 directory:" -justify left
        entry       $f1.job1Dir       -textvariable ::jobCreate::combinedJobs(1)
        button      $f1.selectJob1Dir -text "Browse" -command [namespace code "getJobDirectory 1"]
        button      $f1.deleteJob1Dir -text "x" -foreground red -state disabled -command [namespace code "deleteJobDirectory $f1 1"]
        label       $f1.job2_label    -text "job 2 directory:" -justify left
        entry       $f1.job2Dir       -textvariable ::jobCreate::combinedJobs(2)
        button      $f1.selectJob2Dir  -text "Browse" -command [namespace code "getJobDirectory 2"]
        button      $f1.deleteJob2Dir -text "x" -foreground red -state disabled -command [namespace code "deleteJobDirectory $f1 2"]
        label       $f1.jobSelectAll_label    -text "job selectAll directory:" -justify left
        entry       $f1.jobSelectAllDir       -textvariable ::jobCreate::combinedJobs(sum)
        button      $f1.selectJobSelectAllDir  -text "Browse" -command [namespace code "getJobDirectory sum"]
        grid $f1.job1_label $f1.job1Dir $f1.selectJob1Dir $f1.deleteJob1Dir
        grid $f1.job2_label $f1.job2Dir $f1.selectJob2Dir $f1.deleteJob2Dir
        grid $f1.jobSelectAll_label $f1.jobSelectAllDir $f1.selectJobSelectAllDir x
        grid columnconfigure $f1  1 -weight 1
        grid $f1.job1_label -sticky e
        grid $f1.job1Dir -sticky we
        grid $f1.selectJob1Dir -sticky w
        grid $f1.job2_label -sticky e
        grid $f1.job2Dir -sticky we
        grid $f1.selectJob2Dir -sticky w
        grid $f1.jobSelectAll_label -sticky e
        grid $f1.jobSelectAllDir -sticky we
        grid $f1.selectJobSelectAllDir -sticky w
        button $f2.createSelectAllDir -text "create Sum Directory" -command [namespace code createSelectAllDir] -state disabled
        button $f2.anotherJobDirectory   -text "another Job Directory" -command [namespace code anotherJobDirectory]
        button $f2.defaultValues      -text "guess directories" -command [namespace code defaultJobDirectories]
        grid $f2.createSelectAllDir $f2.anotherJobDirectory $f2.defaultValues x
        grid $f2.createSelectAllDir -sticky e
        grid $f2.defaultValues -sticky w

        trace add variable ::jobCreate::combinedJobs(1) write [namespace code checkJobDirs]
        trace add variable ::jobCreate::combinedJobs(1) unset [namespace code checkJobDirs]
        trace add variable ::jobCreate::combinedJobs(2) write [namespace code checkJobDirs]
        trace add variable ::jobCreate::combinedJobs(2) unset [namespace code checkJobDirs]
        trace add variable ::jobCreate::combinedJobs(sum) write [namespace code checkJobDirs]
        checkJobDirs ::jobCreate::combinedJobs(sum) {} write

        pack [labelframe .selectAll.found -text "Previously created files from selectAll$mode"] -anchor w
        set selFrame [labelframe .selectAll.files -text "Invoke selectAll$mode with..."]
        pack $selFrame -anchor w
        pack [labelframe .selectAll.foundHists -text "Previously created files from combineHistograms$mode"] -anchor w
        set hisFrame [labelframe .selectAll.hists -text "Group into \u0394\u03c1/\u221a\u03c1_ref histogram file (using combineHistograms${mode})"]
        pack $hisFrame -anchor w
        set tFrame [frame .selectAll.tFrame]
        pack $tFrame -fill both -expand true

        text .selectAll.tFrame.t -yscrollcommand {.selectAll.tFrame.y set} \
                              -xscrollcommand {.selectAll.tFrame.x set} -wrap word
        scrollbar .selectAll.tFrame.y -command {.selectAll.tFrame.t yview}
        scrollbar .selectAll.tFrame.x -command {.selectAll.tFrame.t xview} -orient horizontal
        grid .selectAll.tFrame.t .selectAll.tFrame.y
        grid .selectAll.tFrame.x x
        grid .selectAll.tFrame.t -sticky news
        grid .selectAll.tFrame.y -sticky ns
        grid .selectAll.tFrame.x -sticky we
        grid columnconfigure .selectAll.tFrame 0 -weight 1
        grid rowconfigure    .selectAll.tFrame 0 -weight 1
        .selectAll.tFrame.t tag configure input -foreground black
        .selectAll.tFrame.t tag configure normalOutput -foreground blue
        .selectAll.tFrame.t tag configure errorOutput -foreground red

        button .selectAll.runIt -text "Run selectAll" -state disabled \
            -command [namespace code [list runSelectAll]]
        button .selectAll.batchRunIt -text "Batch selectAll" -state disabled \
            -command [namespace code [list batchRunSelectAll]]
        button .selectAll.anotherGroup -text "Add \u0394\u03c1/\u221a\u03c1_ref file" \
            -command [namespace code [list addDeltaRhoFile]]
        button .selectAll.createDeltaRhoFile -text "Create \u0394\u03c1/\u221a\u03c1_ref files" -state disabled \
            -command [namespace code [list createDeltaRhoFile]]
        button .selectAll.batchCreateDeltaRhoFile -text "Batch \u0394\u03c1/\u221a\u03c1_ref files" -state disabled \
            -command [namespace code [list batchCreateDeltaRhoFile]]
        pack .selectAll.runIt .selectAll.batchRunIt .selectAll.anotherGroup .selectAll.createDeltaRhoFile .selectAll.batchCreateDeltaRhoFile -side left

        # Want to only allow window to be destroyed when not in action?
        #bind .selectAll <Control-w> {destroy .selectAll}

        scanSelectAll

    } else {
        raise .selectAll
    }
}
################################################################################
# scanSelectAll scans for files suitable for selectAll (and output from that macro)
#   and grouping into \Delta\rho/sqrt{\rho}
################################################################################
proc ::jobCreate::scanSelectAll {} {
    if {$::jobCreate::combinedJobs(sum) eq ""} {
        set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
        set path [file join [$node text] data]
    } else {
        set path [file join $::jobCreate::combinedJobs(sum) data]
    }
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]

    # Look for log file from selectAll and parse it for previous runs
    set logFile [file join $path selectAllLog]
    set found [list]
    if {[file exists $logFile]} {
        set log [open $logFile r]
        while {[gets $log line] >= 0} {
            if {[string first "Invoke selectAllM${mode}" $line] >= 0} {
                lappend found [lindex $line end]
            }
        }
        close $log
    }
    set found [lsort -unique -dictionary $found]
    foreach ch [winfo children .selectAll.found] {
        destroy $ch
    }
    foreach fn $found {
        pack [label .selectAll.found.l$fn -text $fn]  -side left
    }

    # Look for log file from combineHistograms and parse it for previous runs
    set logFile [file join $path combineLog]
    unset -nocomplain found
    if {[file exists $logFile]} {
        set log [open $logFile r]
        while {[gets $log line] >= 0} {
            if {[string first "Invoke combineHistogram$mode creating" $line] >= 0} {
                if {[regexp "Invoke combineHistogram$mode creating(.*)" $line m v]} {
                    set found([lindex $v 0]) [lrange $v 2 end]
                }
            }
        }
        close $log
    }
    foreach ch [winfo children .selectAll.foundHists] {
        destroy $ch
    }
    foreach fn [array names found] {
        pack [frame .selectAll.foundHists.f$fn] -anchor w
        pack [label .selectAll.foundHists.f$fn.l$fn -text $fn] -side left
        pack [label .selectAll.foundHists.f$fn.lArrow -text "<-"] -side left
        foreach f [lsort -dictionary $found($fn)] {
            pack [label .selectAll.foundHists.f$fn.l$f -text $f] -side left
        }
    }

    # Get Data{n}.root files
    set dList [list]
    foreach fName [glob -nocomplain [file join $path Data*.root]] {
        set f [file tail $fName]
        if {[regexp {Data(\d+).root} $f m v]} {
            lappend dList [file rootname $f]
        }
    }
    # Get Sum{n1}_{n2}.root files
    set sList [list]
    foreach fName [glob -nocomplain [file join $path Sum*.root]] {
        set f [file tail $fName]
        if {[regexp {Sum(\d+)_(\d+).root} $f m v1 v2]} {
            lappend sList [file rootname $f]
        }
    }
    if {[llength $dList] == 0 && [llength $sList] == 0} {
        .selectAll.tFrame.t insert end "Did not find a file that looked like \
            a sum of the correlation histogram files in directory $path \n" errorOutput
        .selectAll.tFrame.t see end
        return
    }
    set dList [lsort -dictionary $dList]
    set sList [lsort -dictionary $sList]

    set ::jobCreate::selectAllFiles [list]

    # Create row of buttons for Data{n} files.
    # In case window was created, modified, then destroyed look for variables
    # that would have been set and make widgets reflect those settings.
    foreach ch [winfo children .selectAll.files] {
        destroy $ch
    }

    set bdList [label .selectAll.files.dSelect -text "Data"]
    foreach d $dList {
        regexp {Data(\d+)} $d m i
        if {![info exists ::jobCreate::selectAllFiles$i]} {
            set ::jobCreate::selectAllFiles$i 1
        }
        if {[lsearch $::jobCreate::selectAllFiles $d] >= 0} {
            set ::jobCreate::selectAllFiles$i 1
        } elseif {[set ::jobCreate::selectAllFiles$i]} {
            lappend ::jobCreate::selectAllFiles $d
        }
        lappend bdList [checkbutton .selectAll.files.sel$i -text $i \
                -variable ::jobCreate::selectAllFiles$i \
                -command [namespace code [list includeInSelect $i $d]]]
    }
    eval grid $bdList -sticky w
    set nCols [llength $bdList]

    set bsList [label .selectAll.files.sSelect -text "Sum"]
    foreach s $sList {
        regexp {Sum(\d+_\d+)} $s m i
        if {![info exists ::jobCreate::selectAllFiles$i]} {
            set ::jobCreate::selectAllFiles$i 1
        }
        if {[lsearch $::jobCreate::selectAllFiles $s] >= 0} {
            set ::jobCreate::selectAllFiles$i 1
        } elseif {[set ::jobCreate::selectAllFiles$i]} {
            lappend ::jobCreate::selectAllFiles $s
        }
        lappend bsList [checkbutton .selectAll.files.sel$i -text $i \
                -variable ::jobCreate::selectAllFiles$i \
                -command [namespace code [list includeInSelect $i $s]]]
    }
    eval grid $bsList -sticky w
    if {[llength $bsList] > $nCols} {
        set nCols [llength $bsList]
    }
    for {set i 3} {$i < $nCols} {incr i} {
        grid columnconfigure .selectAll.files $i -uniform a
    }

    # Create lines for specifying files to combine in histograms.
    set n 1
    if {[info exists ::jobCreate::numberHistFiles]} {
        set n $::jobCreate::numberHistFiles
    }
    set ::jobCreate::numberHistFiles 0
    set ::jobCreate::histFileList(0) $dList
    for {set i 0} {$i < $n} {incr i} {
        addDeltaRhoFile $dList $sList
    }
    # Sum up Data{n} for one DeltaRho file by default.
    foreach d $dList {
        regexp {Data(\d+)} $d m ic
        set ::jobCreate::histFileCB(0,$ic) 1
    }

    if {[llength $::jobCreate::selectAllFiles] > 0} {
        .selectAll.runIt configure -state normal
        .selectAll.batchRunIt configure -state normal
    }
    .selectAll.anotherGroup configure -command [namespace code [list addDeltaRhoFile $dList $sList]]
}
################################################################################
# getJobDirectory invokes file requestor to get a directory for;
#   job1
#   job2
#   jobSelectAll
################################################################################
proc ::jobCreate::getJobDirectory {el} {
    set ::jobCreate::combinedJobs($el) [tk_chooseDirectory]
}
################################################################################
# anotherJobDirectory:
#   Create row of widgets for another job directory to sum.
#   When we have more than two directories enable deleteJobDirectory buttons.
#   In order to pack new line before sum line need to forget and re-grid sum.
################################################################################
proc ::jobCreate::anotherJobDirectory {} {
    set elList [lremove [array names ::jobCreate::combinedJobs] sum]
    set el [lindex [lsort $elList] end]
    incr el

    set f1 .selectAll.combineJobs.f.dirs

    label       $f1.job${el}_label    -text "job $el directory:" -justify left
    entry       $f1.job${el}Dir       -textvariable ::jobCreate::combinedJobs($el)
    button      $f1.selectJob${el}Dir -text "Browse" -command [namespace code "getJobDirectory $el"]
    button      $f1.deleteJob${el}Dir -text "x" -foreground red -command [namespace code "deleteJobDirectory $f1 $el"]

    grid forget $f1.jobSelectAll_label $f1.jobSelectAllDir $f1.selectJobSelectAllDir
    grid $f1.job${el}_label $f1.job${el}Dir $f1.selectJob${el}Dir $f1.deleteJob${el}Dir
    grid $f1.jobSelectAll_label $f1.jobSelectAllDir $f1.selectJobSelectAllDir x
    grid $f1.job${el}_label -sticky e
    grid $f1.job${el}Dir -sticky we
    grid $f1.selectJob${el}Dir -sticky w
    grid $f1.jobSelectAll_label -sticky e
    grid $f1.jobSelectAllDir -sticky we
    grid $f1.selectJobSelectAllDir -sticky w

    trace add variable ::jobCreate::combinedJobs($el) write [namespace code checkJobDirs]
    trace add variable ::jobCreate::combinedJobs($el) unset [namespace code checkJobDirs]

    if {$el > 2} {
        for {set i 1} {$i <= $el} {incr i} {
            $f1.deleteJob${i}Dir configure -state normal
        }
    }
    set ::jobCreate::combinedJobs($el) $::jobCreate::combinedJobs($el)
}
################################################################################
# deleteJobDirectory:
#   Delete row of widgets for job directory.
#   If this is not the last of the job directory rows we compact list.
#   If we are left with only two job directories we disable delete buttons.
################################################################################
proc ::jobCreate::deleteJobDirectory {f1 el} {
    set elList [lremove [array names ::jobCreate::combinedJobs] sum]
    set last [lindex [lsort $elList] end]

    for {set i $el} {$i < $last} {} {
        set ::jobCreate::combinedJobs($i) $::jobCreate::combinedJobs([incr i])
    }
    grid forget $f1.job${last}_label $f1.job${last}Dir $f1.selectJob${last}Dir $f1.deleteJob${last}Dir
    destroy $f1.job${last}_label
    destroy $f1.job${last}Dir
    destroy $f1.selectJob${last}Dir
    destroy $f1.deleteJob${last}Dir

    unset ::jobCreate::combinedJobs($last)
    if {[llength [array names ::jobCreate::combinedJobs]] < 4} {
        $f1.deleteJob1Dir configure -state disabled
        $f1.deleteJob2Dir configure -state disabled
    }
}
################################################################################
# defaultJobDirectories:
#   job1:  outputDir in job1
#   job2:  switch FullField and ReversedFullField of job1
#          if directory does not exist leave entry blank
#   jobSelectAll : If job1 and job2 are filled omit FullFiled and ReversFullField
#                  Otherwise leave blank
################################################################################
proc ::jobCreate::defaultJobDirectories {} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set job1 [$node text]
    set ::jobCreate::combinedJobs(1) $job1
    set ::jobCreate::combinedJobs(2) ""
    if {[string first FullField $job1] >= 0} {
        set job2 [string map {FullField ReversedFullField ReversedFullField FullField} $job1]
        if {[file exists $job2]} {
            set ::jobCreate::combinedJobs(2) $job2
        }
    }
    if {$::jobCreate::combinedJobs(2) ne ""} {
        set jobSelectAll [string map {FullField "" ReversedFullField ""} $job1]
        if {[file exists $jobSelectAll]} {
            if {[file isdirectory $jobSelectAll]} {
                if {[tk_messageBox -message "Destination directory \n $jobSelectAll \n exists. Should we use it?" -type yesno]} {
                    set ::jobCreate::combinedJobs(sum) $jobSelectAll
                }
            } else {
                tk_messageBox -message "Wanted to use \n $jobSelectAll \n as destination directory but it is a file" -type ok
            }
        } else {
            set ::jobCreate::combinedJobs(sum) $jobSelectAll
        }
    }
}
################################################################################
# createSelectAllDir:
#   Create directory if it doesn't exist
#   Sum QA/QA.root, cuts/Cuts.root, data/Datan.root and data/Sum_M_N.root from
#   job1Dir and job2Dir putting
################################################################################
proc ::jobCreate::createSelectAllDir {} {
    # Check that expected job1, job2 directories exist
    set elList [lremove [array names ::jobCreate::combinedJobs] sum]
    foreach el $elList {
        set dir $::jobCreate::combinedJobs($el)
        if {[file isdirectory $dir]} {
            foreach subDir [list QA cuts data] {
                if {![file isdirectory [file join $dir $subDir]]} {
                    return
                }
            }
        }
    }
    # If destination directory is a file we quit
    if {[file isfile $::jobCreate::combinedJobs(sum)]} {
        return
    }
    # Make destination directory if it doesn't exist.
    # Make sure it is populated with appropriate directories that are not files.
    if {![file exists $::jobCreate::combinedJobs(sum)]} {
        file mkdir $::jobCreate::combinedJobs(sum)
    }
    foreach subDir [list scripts QA cuts data logs] {
        set newDir [file join $::jobCreate::combinedJobs(sum) $subDir]
        if {[file isfile $newDir]} {
            return
        }
    }
    foreach subDir [list scripts QA cuts data logs] {
        set newDir [file join $::jobCreate::combinedJobs(sum) $subDir]
        if {![file exists $newDir]} {
            file mkdir $newDir
        }
    }

    # Open log file in logs sub-directory of combined directory
    set logFile [file join $::jobCreate::combinedJobs(sum) logs addJobsLog]
    set log [open $logFile w+]

    # Use hadd to sum QA and Cuts histogram files.
    # For Cuts.root files we may have different limits
    # for some histograms.
    # Copy original Cuts.root files (with a new name) and
    # catch the hadd command for Cuts.
    set QAList [list]
    foreach el $elList {
        set QAFile [file join $::jobCreate::combinedJobs($el) QA QA.root]
        lappend QAList $QAFile
    }
    set QA [file join $::jobCreate::combinedJobs(sum) QA QA.root]
    puts $log "eval exec hadd $QA $QAList"
    catch {eval exec hadd $QA $QAList}

    set cutsList [list]
    foreach el $elList {
        set CutsFile [file join $::jobCreate::combinedJobs($el) cuts Cuts.root]
        lappend cutsList $CutsFile
        set CutsCopy [file join $::jobCreate::combinedJobs(sum) cuts Cuts_[file tail $::jobCreate::combinedJobs($el)].root]
        puts $log "file copy $CutsFile $CutsCopy"
        file copy $CutsFile $CutsCopy
    }
    set cuts [file join $::jobCreate::combinedJobs(sum) cuts Cuts.root]
    puts $log "eval exec hadd $cuts $cutsList"
    catch {eval exec hadd $cuts $cutsList}

    # For data histograms we use addJobs macro to keep zBuffers independent until selectAll macro.
    foreach d [glob [file join $::jobCreate::combinedJobs(1) data Data*]] {
        if {[regexp {Data(\d+).root} $d m i]} {
            set dataList [list]
            foreach el $elList {
                lappend dataList [file join $::jobCreate::combinedJobs($el) data $m]
            }

            set outFile [file join $::jobCreate::combinedJobs(sum) data Data${i}.root]
            set mFile [file join $::jobCreate::combinedJobs(sum) scripts addJobs${i}.C]
            set mF [open $mFile w+]
            puts $mF "void addJobs${i} () {"
            puts $mF "    gROOT->LoadMacro(\"addJobs.C\");"
            puts $mF "    char *inFiles\[\] = {\"[join $dataList {", "}]\"};"
            puts $mF "    addJobs(\"$outFile\",inFiles,[llength $dataList]);"
            puts $mF "}"
            close $mF
            file attributes $mFile -permissions +x
            set cmd "root4star -q -b $mFile"

            puts $log "Starting addJobs for $m"
            puts $log $cmd
            .selectAll.tFrame.t insert end "$cmd\n" input
            .selectAll.tFrame.t see end
            set fid [open "|$cmd"]
            fconfigure $fid -blocking false -buffering line
            fileevent $fid readable [namespace code [list addJobsReadable $fid $log]]
            set currCurs [.selectAll.tFrame.t cget -cursor]
            .selectAll.tFrame.t configure -cursor watch
            vwait ::ADDJOBS-DONE
            catch {close $fid}
            .selectAll.tFrame.t conf -cursor $currCurs
        }
    }

    foreach s [glob [file join $::jobCreate::combinedJobs(1) data Sum*]] {
        if {[regexp {Sum(\d+_\d+).root} $s m i]} {
            set dataList [list]
            foreach el $elList {
                lappend dataList [file join $::jobCreate::combinedJobs($el) data $m]
            }


            set outFile [file join $::jobCreate::combinedJobs(sum) data Sum${i}.root]
            set mFile [file join $::jobCreate::combinedJobs(sum) scripts addJobs${i}.C]
            set mF [open $mFile w+]
            puts $mF "void addJobs${i} () {"
            puts $mF "    gROOT->LoadMacro(\"addJobs.C\");"
            puts $mF "    char *inFiles\[\] = {\"[join $dataList {", "}]\"};"
            puts $mF "    addJobs(\"$outFile\",inFiles,[llength $dataList]);"
            puts $mF "}"
            close $mF
            file attributes $mFile -permissions +x
            set cmd "root4star -q -b $mFile"

            puts $log "Starting addJobs for $m"
            puts $log $cmd
            .selectAll.tFrame.t insert end "$cmd\n" input
            .selectAll.tFrame.t see end
            set fid [open "|$cmd"]
            fconfigure $fid -blocking false -buffering line
            fileevent $fid readable [namespace code [list addJobsReadable $fid $log]]
            set currCurs [.selectAll.tFrame.t cget -cursor]
            .selectAll.tFrame.t configure -cursor watch
            vwait ::ADDJOBS-DONE
            catch {close $fid}
            .selectAll.tFrame.t conf -cursor $currCurs
        }
    }

    scanSelectAll
}
################################################################################
# addJobsReadable waits for output from root4star process running addJobs and
#  puts it into a text widget.
################################################################################
proc ::jobCreate::addJobsReadable {fid fLog} {
    # The channel is readable; try to read it.
    set status [catch { gets $fid line } result]
    if { $status != 0 } {
        # Error on the channel
        puts $fLog ">>>>>error reading $fid: $result<<<<<"
        .selectAll.tFrame.t insert end "error reading $fid: $result\n"
        .selectAll.tFrame.t see end
        set ::ADDJOBS-DONE 2
    } elseif { [eof $fid] } {
        set ::ADDJOBS-DONE 1
    } elseif { $result >= 0 } {
        puts $fLog $line
        .selectAll.tFrame.t insert end "$line\n"
        .selectAll.tFrame.t see end
    } elseif { [fblocked $fid] } {
        # Read blocked.  Just return
    } else {
        # Something else
        puts $fLog ">>>>>Something impossible happened while reading output from hadd<<<<<"
        .selectAll.tFrame.t insert end "What did you do?? the impossible happened while reading $fid: $result\n"
        .selectAll.tFrame.t see end
        set ::ADDJOBS-DONE 3
    }
}
################################################################################
# checkJobDirs:
#   Require ::jobCreate::combinedJobs(sum) and at least two ::jobCreate::combinedJobs(n).
#   to be set to some value for .selectAll.combineJobs.f.actions.createSelectAllDir to
#    be available
################################################################################
proc ::jobCreate::checkJobDirs {var el op} {
    if {![winfo exists .selectAll.combineJobs.f.actions.createSelectAllDir]} {
        return
    }
    .selectAll.combineJobs.f.actions.createSelectAllDir configure -state normal
    if {[llength [array names ::jobCreate::combinedJobs]] < 3} {
        .selectAll.combineJobs.f.actions.createSelectAllDir configure -state disabled
        return
    }
    foreach el [array names ::jobCreate::combinedJobs] {
        if {$::jobCreate::combinedJobs($el) eq ""} {
            .selectAll.combineJobs.f.actions.createSelectAllDir configure -state disabled
        }
    }
}
################################################################################
# addDeltaRhoFile creates a line of buttons to indicate which centralities
# should be added together.
# If window was deleted then recreated we want same buttons with same state.
# This is why we check existance of variables.
################################################################################
proc ::jobCreate::addDeltaRhoFile {dList sList} {
    set j $::jobCreate::numberHistFiles
    set hFrame .selectAll.hists
    if {![winfo exists $hFrame.histFile$j]} {
        lappend cbs [label $hFrame.histFile$j -text "DeltaRhoBySqrtRho_$j"]
        lappend cbs [label $hFrame.space$j -text "<-"]

        if {![info exists ::jobCreate::histFileList($j)]} {
            set ::jobCreate::histFileList($j) [list]
        }
        foreach d $dList {
            regexp {Data(\d+)} $d m ic
            if {![info exists ::jobCreate::histFileCB($j,$ic)]} {
                set ::jobCreate::histFileCB($j,$ic) 0
            }
            if {[lsearch $::jobCreate::histFileList($j) $d] >= 0} {
                set ::jobCreate::histFileCB($j,$ic) 1
            } elseif {$::jobCreate::histFileCB($j,$ic)} {
                lappend ::jobCreate::histFileList($j) $d
            }
            lappend cbs [checkbutton $hFrame.cb${j}${ic} -text $ic -variable ::jobCreate::histFileCB($j,$ic) \
                    -command [namespace code [list toggleHistFile $j $ic $d]]]
        }
        set nCols [llength $cbs]
        if {$nCols > 0} {
            eval grid $cbs -sticky w
        }

        set cbs [list x x]
        foreach s $sList {
            regexp {Sum(\d+_\d+)} $s m ic
            if {![info exists ::jobCreate::histFileCB($j,$ic)]} {
                set ::jobCreate::histFileCB($j,$ic) 0
            }
            if {[lsearch $::jobCreate::histFileList($j) $s] >= 0} {
                set ::jobCreate::histFileCB($j,$ic) 1
            } elseif {$::jobCreate::histFileCB($j,$ic)} {
                lappend ::jobCreate::histFileList($j) $s
            }
            lappend cbs [checkbutton $hFrame.cb${j}${ic} -text $ic -variable ::jobCreate::histFileCB($j,$ic) \
                    -command [namespace code [list toggleHistFile $j $ic $s]]]
        }
        set mCols [llength $cbs]
        if {$mCols > 2} {
            eval grid $cbs -sticky w
        }
        if {$mCols > $nCols} {
            set nCols $mCols
        }
        for {set i 3} {$i < $nCols} {incr i} {
            grid columnconfigure $hFrame $i -uniform a
        }
    }

    incr ::jobCreate::numberHistFiles
    set nCheck 0
    for {set j 0} {$j < $::jobCreate::numberHistFiles} {incr j} {
        incr nCheck [llength $::jobCreate::histFileList($j)]
    }
    if {$nCheck > 0} {
        .selectAll.createDeltaRhoFile configure -state normal
        .selectAll.batchCreateDeltaRhoFile configure -state normal
    }
}
################################################################################
# toggleHistFile removes(adds) file from(to) list.
################################################################################
proc ::jobCreate::toggleHistFile {j ic f} {
    if {$::jobCreate::histFileCB($j,$ic)} {
        lappend ::jobCreate::histFileList($j) $f
    } else {
        set ::jobCreate::histFileList($j) [lremove $::jobCreate::histFileList($j) $f]
    }
    set nCheck 0
    for {set j 0} {$j < $::jobCreate::numberHistFiles} {incr j} {
        incr nCheck [llength $::jobCreate::histFileList($j)]
    }
    if {$nCheck > 0} {
        .selectAll.createDeltaRhoFile configure -state normal
        .selectAll.batchCreateDeltaRhoFile configure -state normal
    }
}
################################################################################
# includeInSelect removes(adds) file from(to) list.
################################################################################
proc ::jobCreate::includeInSelect {i f} {
    set val [set ::jobCreate::selectAllFiles$i]
    if {$val} {
        lappend ::jobCreate::selectAllFiles $f
    } else {
        set ::jobCreate::selectAllFiles [lremove $::jobCreate::selectAllFiles $f]
    }
    if {[llength $::jobCreate::selectAllFiles] > 0} {
        .selectAll.runIt configure -state normal
        .selectAll.batchRunIt configure -state normal
    }
}
################################################################################
# batchRunSelectAll creates a csh script to executes the appropriate version of selectAll for the
# mode for all the files selected by selectAll.
################################################################################
proc ::jobCreate::batchRunSelectAll {} {
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]
    if {$::jobCreate::combinedJobs(sum) eq ""} {
        set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
        set path [$node text]
    } else {
        set path $::jobCreate::combinedJobs(sum)
    }
    # Assume selectAllM${mode}.C script is under the localDir job was submitted from.
    # Assume code compiled for starLibVersion
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    set lDir [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName starLibVersion]
    set sVer [$node text]

    set logFile [file join $path data selectAllLog]
    if {[file exists $logFile]} {
        set log [open $logFile a]
    } else  {
        set log [open $logFile w]
    }
    set selectC   [file join $path scripts SelectAll${mode}.C]
    if {![file exists $selectC]} {
        set C   [open $selectC w]
        puts $C "void SelectAll${mode} \(const char *dirName, const char *fileBase\) {"
        puts $C "    gROOT->LoadMacro\(\"selectAllM${mode}.C\"\);"
        puts $C "    selectAllM${mode}\(dirName,fileBase\);"
        puts $C "};"
        close $C
    }
    foreach s $::jobCreate::selectAllFiles {
        # Check if we understand file and get suffix for button name.
        if {[regexp {Data(\d+)} $s m i]} {
            set ok true
        } elseif {[regexp {Sum(\d+_\d+)} $s m i]} {
            set ok true
        }
        if {!$ok} {
            set abort [tk_messageBox -message "Problem parsing file name $f. Abort?" -type yesno \
                    -icon question -title "in runSelectAll"]
            if {$abort} {
                break
            } else {
                continue
            }
        }
#>>>>> Need to create a *.C file and a *.csh file for batch submission as was done for combine centralities.
        set nTmp 0
        set selectcsh [file join $path scripts schedSelect_${nTmp}.csh]
        set selectLog [file join $path logs         Select_${nTmp}.log]
        while {[file exists $selectcsh]} {
            incr nTmp
            set selectcsh [file join $path scripts schedSelect_${nTmp}.csh]
            set selectLog [file join $path logs Select_${nTmp}.log]
        }
        set csh [open $selectcsh w]

        puts $log "Creating C macro and csh script to run selectAll."
        puts $log "Invoke selectAllM$mode for [file rootname $s]"
        puts $log "Want to run following command."
        puts $log "    root4star -q -b selectAllM${mode}.C\(\"$path\",\"[file rootname $s]\"\);"


#>>>>> Should be easy to check if qsub is available and use condor if it is not.
        if {[catch {exec qstat -u $::env(USER)}]} {
            set condorFile [file join $path scripts schedSelect_${nTmp}.condor]
            set condorLog  [file join $path scripts schedSelect_${nTmp}.condor.log]
            set condorDir  [file join $path scripts]
            ::jobCreate::createCondorFile $condorFile $selectcsh $selectLog $condorLog $condorDir

            puts $csh "#!/bin/csh"
            puts $csh "# condor_submit $condorFile"
            puts $csh "cd $lDir"
            puts $csh "starver $sVer"
            puts $csh "root4star -q -b [file join $path scripts SelectAll${mode}.C\\(\\\"[file join $path data]\\\",\\\"[file rootname $s]\\\"\\)]"
            close $csh
            file attributes $selectcsh -permissions +x
            catch {exec condor_submit $condorFile}
        } else {
            puts $csh "#!/bin/csh"
            puts $csh "# qsub -o $selectLog -j y $selectcsh"
            puts $csh "cd $lDir"
            puts $csh "starver $sVer"
            puts $csh "root4star -q -b [file join $path scripts SelectAll${mode}.C\\(\\\"[file join $path data]\\\",\\\"[file rootname $s]\\\"\\)]"
            close $csh
            exec qsub -o $selectLog -j y $selectcsh
        }

        .selectAll.tFrame.t insert end "Creating C macro and csh script to run selectAll." input
        .selectAll.tFrame.t insert end "Want to run following command." input
        .selectAll.tFrame.t insert end "    root4star -q -b selectAllM${mode}.C\(\"$path\",\"[file rootname $s]\"\);" input
        .selectAll.tFrame.t see end

        set fn [file rootname $s]
        if {![winfo exists .selectAll.found.l$fn]} {
            pack [label .selectAll.found.l$fn -text $fn]  -side left
        }
        .selectAll.files.sel$i configure -selectcolor blue
    }
    close $log
}
################################################################################
# runSelectAll executes the appropriate version of selectAll for the
# mode for all the files selected by selectAll.
################################################################################
proc ::jobCreate::runSelectAll {} {
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]
    if {$::jobCreate::combinedJobs(sum) eq ""} {
        set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
        set path [file join [$node text] data]
    } else {
        set path [file join $::jobCreate::combinedJobs(sum) data]
    }
    # Assume selectAllM${mode}.C script is under the localDir job was submitted from.
    set pwd [pwd]
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    cd [$node text]

    set logFile [file join $path selectAllLog]
    if {[file exists $logFile]} {
        set log [open $logFile a]
    } else  {
        set log [open $logFile w]
    }
    set ::jobCreate::stopRunSelectAll 0
    .selectAll.runIt configure -text "Stop selectAll$mode" \
            -command {set ::jobCreate::stopRunSelectAll 1}
    foreach s $::jobCreate::selectAllFiles {
        if {$::jobCreate::stopRunSelectAll} {
            break
        }
        set cmd "root4star -q -b selectAllM${mode}.C\(\"$path\",\"[file rootname $s]\"\);"
        puts $log "Invoke selectAllM$mode for [file rootname $s]"
        puts $log $cmd
        .selectAll.tFrame.t insert end "$cmd\n" input
        .selectAll.tFrame.t see end

        if {[regexp {Data(\d+)} $s m i]} {
            set ok true
        } elseif {[regexp {Sum(\d+_\d+)} $s m i]} {
            set ok true
        }
        if {!$ok} {
            set abort [tk_messageBox -message "Problem parsing file name $f. Abort?" -type yesno \
                    -icon question -title "in runSelectAll"]
            if {$abort} {
                break
            } else {
                continue
            }
        }
        .selectAll.files.sel$i configure -selectcolor yellow

        set fid [open "|$cmd"]
        fconfigure $fid -blocking false -buffering line
        fileevent $fid readable [namespace code [list selectAllReadable $fid $log]]
        set currCurs [.selectAll.tFrame.t cget -cursor]
        .selectAll.tFrame.t configure -cursor watch

        # Wait for the fileevents (i.e. hadd) to finish.
        vwait ::SEL-DONE

        # Close the pipe
        close $fid
        set fn [file rootname $s]
        if {![winfo exists .selectAll.found.l$fn]} {
            pack [label .selectAll.found.l$fn -text $fn]  -side left
        }
        .selectAll.files.sel$i configure -selectcolor green
        .selectAll.tFrame.t conf -cursor $currCurs
    }
    if {$::jobCreate::stopRunSelectAll} {
        .selectAll.runIt configure -text "selectAll$mode was paused" \
                -command [namespace code [list runSelectAll]]
        set ::jobCreate::stopRunSelectAll 0
    } else {
        .selectAll.runIt configure -text Done -command "" -state disabled
    }
    close $log
    cd $pwd
}
################################################################################
# batchCreateDeltaRhoFile creates .C and .csh files to run combineHistogram{mode} in
# batch mode. This takes files produced by the selectAll{mode} macro and produces
# \Delta\rho/\rho_{ref} histograms in one file.
################################################################################
proc ::jobCreate::batchCreateDeltaRhoFile {} {
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]
    if {$::jobCreate::combinedJobs(sum) eq ""} {
        set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
        set path [$node text]
    } else {
        set path $::jobCreate::combinedJobs(sum)
    }
    # Assume combineHistograms.C script is under the localDir job was submitted from.
    # Assume code compiled for starLibVersion
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    set lDir [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName starLibVersion]
    set sVer [$node text]

    set logFile [file join $path data combineLog]
    if {[file exists $logFile]} {
        set log [open $logFile a]
    } else  {
        set log [open $logFile w]
    }

    for {set j 0} {$j < $::jobCreate::numberHistFiles} {incr j} {
        set fList [list]
        set fN [list]
        set icList [list]
        foreach h [lsort -dictionary $::jobCreate::histFileList($j)] {
            set ok false
            if {[regexp {Data(\d+)} $h m ic]} {
                set ok true
            } elseif {[regexp {Sum(\d+_\d+)} $h m ic]} {
                set ok true
            }
            if {!$ok} {
                set abort [tk_messageBox -message "Problem parsing file name $h. Abort?" -type yesno \
                        -icon question -title "in createDeltaRhoFile"]
                if {$abort} {
                    break
                } else {
                    continue
                }
            }
            lappend fList \"$h\"
            lappend fN $h
            lappend icList $ic
        }

        set outFile DeltaRhoBySqrtRho_$j
        set numFiles [llength $fList]
        if {0 == $numFiles} {
            continue
        }

#>>>>> Need to create a *.csh file for batch submission as was done for combine centralities.
        set nTmp 0
        set deltaRhoC   [file join $path scripts Combine${mode}_${nTmp}.C]
        set deltaRhocsh [file join $path scripts schedDeltaRho_${nTmp}.csh]
        set deltaRhoLog [file join $path logs         DeltaRho_${nTmp}.log]
        while {[file exists $deltaRhoC]} {
            incr nTmp
            set deltaRhoC   [file join $path scripts Combine${mode}_${nTmp}.C]
            set deltaRhocsh [file join $path scripts schedDeltaRho_${nTmp}.csh]
            set deltaRhoLog [file join $path logs DeltaRho_${nTmp}.log]
        }
        set csh [open $deltaRhocsh w]

        puts $log "Invoke combineHistogram$mode creating $outFile from $fN"
        puts $log "Creating C macro and csh script to run combineHistograms${mode}."
        puts $log "Want to run following commands."
        puts $log "    gROOT->LoadMacro(\"combineHistograms${mode}.C\");"
        puts $log "    char *inNames\[\] = {[join $fList ,]};"
        puts $log "    combineHistograms${mode}(\"[file join $path data]\",inNames,\"$outFile\",$numFiles);"

        set C   [open $deltaRhoC w]
        puts $C "void Combine${mode}_${nTmp} \(const char *dirName, const char *outName\) {"
        puts $C "    gROOT->LoadMacro(\"combineHistograms${mode}.C\");"
        puts $C "    char *inNames\[\] = {[join $fList ,]};"
        puts $C "    combineHistograms${mode}(dirName,inNames,outName,$numFiles);"
        puts $C "};"
        close $C

#>>>>> Should be easy to check if qsub is available and use condor if it is not.
        if {[catch {exec qstat -u $::env(USER)}]} {
            set condorFile [file join $path scripts schedDeltaRho_${nTmp}.condor]
            set condorLog  [file join $path scripts schedDeltaRho_${nTmp}.condor.log]
            set condorDir  [file join $path scripts]
            ::jobCreate::createCondorFile $condorFile $deltaRhocsh $deltaRhoLog $condorLog $condorDir

            puts $csh "#!/bin/csh"
            puts $csh "# condor_submit $condorFile"
            puts $csh "cd $lDir"
            puts $csh "starver $sVer"
            puts $csh "root4star -q -b [file join $path scripts Combine${mode}_${nTmp}.C\\(\\\"[file join $path data]\\\",\\\"$outFile\\\"\\)]"
            close $csh
            file attributes $deltaRhocsh -permissions +x
            catch {exec condor_submit $condorFile}
        } else {
            puts $csh "#!/bin/csh"
            puts $csh "# qsub -o $deltaRhoLog -j y $deltaRhocsh"
            puts $csh "cd $lDir"
            puts $csh "starver $sVer"
            puts $csh "root4star -q -b [file join $path scripts Combine${mode}_${nTmp}.C\\(\\\"[file join $path data]\\\",\\\"$outFile\\\"\\)]"
            close $csh
            exec qsub -o $deltaRhoLog -j y $deltaRhocsh
        }

        .selectAll.tFrame.t insert end "Creating C macro and csh script to run selectAll." input
        .selectAll.tFrame.t insert end "Want to run following commands." input
        .selectAll.tFrame.t insert end "    gROOT->LoadMacro(\"combineHistograms${mode}.C\");\n"
        .selectAll.tFrame.t insert end "    char *inFile\[\] = {[join $fList ,]}\n"
        .selectAll.tFrame.t insert end "    combineHistograms${mode}(\"$path\",inFile,\"$outFile\",$numFiles);\n"
        .selectAll.tFrame.t see end

        foreach ic $icList {
            .selectAll.hists.cb${j}${ic} configure -selectcolor blue
        }
    }
    close $log
}
################################################################################
# createDeltaRhoFile executes the combineHistogram{mode} which will read histograms
# produced by the selectAll{mode} macro, produce \Delta\rho/\rho_{ref} histograms
# and then write all of these to one file.
################################################################################
proc ::jobCreate::createDeltaRhoFile {} {
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]
    if {$::jobCreate::combinedJobs(sum) eq ""} {
        set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
        set path [file join [$node text] data]
    } else {
        set path [file join $::jobCreate::combinedJobs(sum) data]
    }
    # Assume combineHistograms.C script is under the localDir job was submitted from.
    set pwd [pwd]
    set node [$::jobCreate::jobInfo getElementsByTagName localDir]
    cd [$node text]

    set logFile [file join $path combineLog]
    if {[file exists $logFile]} {
        set log [open $logFile a]
    } else  {
        set log [open $logFile w]
    }
    set ::jobCreate::stopCombineHistograms 0
    .selectAll.createDeltaRhoFile configure -text "Stop combineHistograms$mode" \
            -command {set ::jobCreate::stopCombineHistograms 1}

    set root [open "|root -l -b" r+]
    fconfigure $root -blocking false -buffering line -buffersize 1
    fileevent  $root readable [namespace code [list selectAllReadable $root $log]]
    set currCurs [.selectAll.tFrame.t cget -cursor]
    .selectAll.tFrame.t configure -cursor watch

    for {set j 0} {$j < $::jobCreate::numberHistFiles} {incr j} {
        if {$::jobCreate::stopCombineHistograms} {
            break
        }
        set fList [list]
        set fN [list]
        foreach h [lsort -dictionary $::jobCreate::histFileList($j)] {
            set ok false
            if {[regexp {Data(\d+)} $h m ic]} {
                set ok true
            } elseif {[regexp {Sum(\d+_\d+)} $h m ic]} {
                set ok true
            }
            if {!$ok} {
                set abort [tk_messageBox -message "Problem parsing file name $h. Abort?" -type yesno \
                        -icon question -title "in createDeltaRhoFile"]
                if {$abort} {
                    break
                } else {
                    continue
                }
            }
            lappend fList \"$h\"
            lappend fN $h
        }

        set outFile DeltaRhoBySqrtRho_$j
        set numFiles [llength $fList]
        if {0 == $numFiles} {
            continue
        }

        .selectAll.tFrame.t insert end "gROOT->LoadMacro(\"combineHistograms${mode}.C\");\n"
        .selectAll.tFrame.t insert end "char *inFile\[\] = {[join $fList ,]}\n"
        .selectAll.tFrame.t insert end "combineHistograms${mode}(\"$path\",inFile,\"$outFile\",$numFiles);\n"
        .selectAll.tFrame.t see end
        foreach h $::jobCreate::histFileList($j) {
            if {[regexp {Data(\d+)} $h m ic]} {
                set ok true
            } elseif {[regexp {Sum(\d+_\d+)} $h m ic]} {
                set ok true
            }
            puts $root "cout << \"PLEASE COLOR ME ${j}${ic} yellow\" << endl;"
        }
        puts $log "Invoke combineHistogram$mode creating $outFile from $fN"
        puts $log "gROOT->LoadMacro(\"combineHistograms${mode}.C\");"
        puts $log "char *inFile\[\] = {[join $fList ,]};"
        puts $log "combineHistograms${mode}(\"$path\",inFile,\"$outFile\",$numFiles);"

        puts $root "gROOT->LoadMacro(\"combineHistograms${mode}.C\");"
        puts $root "char *inFile\[\] = {[join $fList ,]};"
        puts $root "combineHistograms${mode}(\"$path\",inFile,\"$outFile\",$numFiles);"

        foreach h $::jobCreate::histFileList($j) {
            if {[regexp {Data(\d+)} $h m ic]} {
                set ok true
            } elseif {[regexp {Sum(\d+_\d+)} $h m ic]} {
                set ok true
            }
            puts $root "cout << \"PLEASE COLOR ME ${j}${ic} green\" << endl;"
        }
        puts $root "cout << \"PLEASE UPDATE CREATED LABEL $outFile $fN\" << endl;"
    }
    # I don't know how to get root to close the pipe by telling it to quit.
    # Instead make root ask for mercy.
    puts $root "cout << \"PLEASE KILL ME\" << endl;"

    vwait ::SUM-DONE
    # Not sure closing the root process really kills it.
    close $root
    close $log
    .selectAll.tFrame.t conf -cursor $currCurs

    if {$::jobCreate::stopCombineHistograms} {
        .selectAll.createDeltaRhoFile configure -text "combineHistograms$mode was paused" \
                -command [namespace code [list createDeltaRhoFile]]
        set ::jobCreate::stopCombineHistograms 0
    } else {
        .selectAll.createDeltaRhoFile configure -text Done -command "" -state disabled
    }
    cd $pwd
}
################################################################################
# selectAllReadable waits for output from selectAll/histogram adding commands and
#  puts it into a text widget.
################################################################################
proc ::jobCreate::selectAllReadable {fid fLog} {
    # The channel is readable; try to read it.
    set status [catch { gets $fid line } result]
    if { $status != 0 } {
        # Error on the channel
        puts $fLog ">>>>>error reading $fid: $result<<<<<"
        .selectAll.tFrame.t insert end "error reading $fid: $result\n"
        .selectAll.tFrame.t see end
        set ::SEL-DONE 2
    } elseif { [eof $fid] } {
        set ::SEL-DONE 1
    } elseif { $result >= 0 } {
        if {$line eq "PLEASE KILL ME"} {
            set ::SUM-DONE 5
            return
        } elseif {[string first "PLEASE COLOR ME " $line] == 0} {
            foreach {ind color} [string map {"PLEASE COLOR ME " ""} $line] {break}
            .selectAll.hists.cb$ind configure -selectcolor $color
            return
        } elseif {[string first "PLEASE UPDATE CREATED LABEL " $line] == 0} {
            set l [string map {"PLEASE UPDATE CREATED LABEL " ""} $line]
            set fn [lindex $l 0]
            if {![winfo exists .selectAll.foundHists.f$fn]} {
                pack [frame .selectAll.foundHists.f$fn] -anchor w
            }
            if {![winfo exists .selectAll.foundHists.f$fn.l$fn]} {
                pack [label .selectAll.foundHists.f$fn.l$fn -text $fn] -side left
            }
            if {![winfo exists .selectAll.foundHists.f$fn.lArrow]} {
                pack [label .selectAll.foundHists.f$fn.lArrow -text "<-"] -side left
            }
            foreach f [lsort -dictionary [lrange $l 1 end]] {
                if {![winfo exists .selectAll.foundHists.f$fn.l$f]} {
                    pack [label .selectAll.foundHists.f$fn.l$f -text $f] -side left
                }
            }
        } else {
            puts $fLog $line
            .selectAll.tFrame.t insert end "$line\n"
            .selectAll.tFrame.t see end
        }
    } elseif { [fblocked $fid] } {
        # Read blocked.  Just return
    } else {
        # Something else
        puts $fLog ">>>>>Something impossible happened while reading output from hadd<<<<<"
        .selectAll.tFrame.t insert end "What did you do?? the impossible happened while reading $fid: $result\n"
        .selectAll.tFrame.t see end
        set ::SEL-DONE 3
    }
}
################################################################################
# pileupExtrapolation executes the appropriate version of subtractPileup for the
# mode. Only available if analysisType is StEStructCorrelation.
# Ask for directory, DeltRhoBySqrtRhoRef with and without pileup cuts, output file name
# and pileup rejection efficiency.
################################################################################
proc ::jobCreate::pileupExtrapolation {} {
    if {![winfo exists .pileupExtrapolation]} {
        set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
        set mode [$node text]
        toplevel .pileupExtrapolation
        wm title .pileupExtrapolation "Run pileup extrapolation macro"

        #
        # Need two input files, one output file and pileup rejection efficiency.
        #
        set f1 [frame .pileupExtrapolation.dirs]
        set ft [frame .pileupExtrapolation.text]
        set f2 [frame .pileupExtrapolation.actions]
        pack $f1 -fill x    -expand true
        pack $ft -fill both -expand true
        pack $f2 -fill x
        label       $f1.cut_label   -text "file with pileup cuts:" -justify left
        entry       $f1.cutFile     -textvariable ::jobCreate::pileupExtrapolate(1)
        button      $f1.findCutFile -text "Browse" -command [namespace code "getPileupFile 1"]

        label       $f1.noCut_label   -text "file without pileup cuts:" -justify left
        entry       $f1.noCutFile     -textvariable ::jobCreate::pileupExtrapolate(2)
        button      $f1.findNoCutFile -text "Browse" -command [namespace code "getPileupFile 2"]

        label       $f1.extrapolated_label   -text "extrapolated output file:" -justify left
        entry       $f1.extrapolatedFile     -textvariable ::jobCreate::pileupExtrapolate(extrapolated)
        button      $f1.findExtrapolatedFile -text "Browse" -command [namespace code "getPileupFile extrapolated"]
        grid $f1.cut_label          $f1.cutFile          $f1.findCutFile
        grid $f1.noCut_label        $f1.noCutFile        $f1.findNoCutFile
        grid $f1.extrapolated_label $f1.extrapolatedFile $f1.findExtrapolatedFile

        grid columnconfigure $f1  1 -weight 1
        grid $f1.cut_label -sticky e
        grid $f1.cutFile -sticky we
        grid $f1.findCutFile -sticky w

        grid $f1.noCut_label -sticky e
        grid $f1.noCutFile -sticky we
        grid $f1.findNoCutFile -sticky w

        grid $f1.extrapolated_label -sticky e
        grid $f1.extrapolatedFile -sticky we
        grid $f1.findExtrapolatedFile -sticky w

        text $ft.t -yscrollcommand "$ft.y set" \
                   -xscrollcommand "$ft.x set" -wrap word
        scrollbar $ft.y -command "$ft.t yview"
        scrollbar $ft.x -command "$ft.t xview" -orient horizontal
        grid $ft.t $ft.y
        grid $ft.x x
        grid $ft.t -sticky news
        grid $ft.y -sticky ns
        grid $ft.x -sticky we
        grid columnconfigure $ft 0 -weight 1
        grid rowconfigure    $ft 0 -weight 1
        $ft.t tag configure input -foreground black
        $ft.t tag configure normalOutput -foreground blue
        $ft.t tag configure errorOutput -foreground red

        button $f2.runSubtractPileup -text "run subtractPileup" -command [namespace code subtractPileup] -state disabled
        grid $f2.runSubtractPileup
        grid $f2.runSubtractPileup -sticky e

        trace add variable ::jobCreate::pileupExtrapolate(1) write [namespace code checkExtrapolateFiles]
        trace add variable ::jobCreate::pileupExtrapolate(1) unset [namespace code checkExtrapolateFiles]
        trace add variable ::jobCreate::pileupExtrapolate(2) write [namespace code checkExtrapolateFiles]
        trace add variable ::jobCreate::pileupExtrapolate(2) unset [namespace code checkExtrapolateFiles]
        trace add variable ::jobCreate::pileupExtrapolate(extrapolated) write [namespace code checkExtrapolateFiles]
        trace add variable ::jobCreate::pileupExtrapolate(extrapolated) unset [namespace code checkExtrapolateFiles]
        checkExtrapolateFiles ::jobCreate::pileupExtrapolate(extrapolated) {} write

        # Want to only allow window to be destroyed when not in action?
        #bind .selectAll <Control-w> {destroy .selectAll}

        scanExtrapolateFiles

    } else {
        wm deiconify .pileupExtrapolation
        raise .pileupExtrapolation
    }
}
################################################################################
# checkExtrapolateFiles:
#   Require ::jobCreate::combinedJobs(sum) and at least two ::jobCreate::combinedJobs(n).
#   to be set to some value for .selectAll.combineJobs.f.actions.createSelectAllDir to
#    be available
################################################################################
proc ::jobCreate::checkExtrapolateFiles {var el op} {
    if {![winfo exists .pileupExtrapolation.actions.runSubtractPileup]} {
        return
    }
    .pileupExtrapolation.actions.runSubtractPileup configure -state normal
    if {[llength [array names ::jobCreate::pileupExtrapolate]] < 3} {
        .pileupExtrapolation.actions.runSubtractPileup configure -state disabled
        return
    }
    foreach el [array names ::jobCreate::pileupExtrapolate] {
        if {$::jobCreate::pileupExtrapolate($el) eq ""} {
            .pileupExtrapolation.actions.runSubtractPileup configure -state disabled
        }
    }
}
################################################################################
# getPileupFile invokes file requestor to get a directory for;
#   file with pileup cuts
#   file without pileup cuts
#   extrapolated file
################################################################################
proc ::jobCreate::getPileupFile {el} {
    if {$el ne extrapolated} {
        set ::jobCreate::pileupExtrapolate($el) [tk_getOpenFile]
    } else {
        set ::jobCreate::pileupExtrapolate($el) [tk_getSaveFile]
    }
}
################################################################################
# scanExtrapolateFile scans for files suitable for subtractPileup (and output from that macro)
# Look in 
################################################################################
proc ::jobCreate::scanExtrapolateFiles {} {
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]

    # If this path has _noPileupCuts at the end assume it is the analysis with no pileup cuts.
    # Otherwise assume it has pileup cuts and look for parallel directory without.
    set cDir [file dirname $path]
    set dirCut [file tail $path]
    if {[regexp {(.+)_noPileupCuts} $dirCut m n]} {
        set dirCut $n
    }
    set dirNoCut ${dirCut}_noPileupCuts

    # Look for data/DeltaRhoBySqrtRho_0.root in dirCut and dirNoCut
    .pileupExtrapolation.actions.runSubtractPileup configure -state normal
    if {[file exists [file join $cDir $dirCut data DeltaRhoBySqrtRho_0.root]]} {
        set ::jobCreate::pileupExtrapolate(1) [file join $cDir $dirCut data DeltaRhoBySqrtRho_0.root]
    } else {
        .pileupExtrapolation.actions.runSubtractPileup configure -state disabled
    }
    if {[file exists [file join $cDir $dirNoCut data DeltaRhoBySqrtRho_0.root]]} {
        set ::jobCreate::pileupExtrapolate(2) [file join $cDir $dirNoCut data DeltaRhoBySqrtRho_0.root]
    } else {
        .pileupExtrapolation.actions.runSubtractPileup configure -state disabled
    }
    set ::jobCreate::pileupExtrapolate(extrapolated) [file join $cDir $dirCut data AuAu200GeV_12c.root]
}
################################################################################
# subtractPileupN 
################################################################################
proc ::jobCreate::subtractPileup {} {
    if {$::jobCreate::pileupExtrapolate(1) eq "" ||
        $::jobCreate::pileupExtrapolate(2) eq "" ||
        $::jobCreate::pileupExtrapolate(extrapolated) eq "" } {
        error "Need two input and one output files. At least one was missing."
    }
    set node [$::jobCreate::jobInfo getElementsByTagName cutMode]
    set mode [$node text]
    set cmd "root4star -q -b subtractPileup${mode}.C\(\"$::jobCreate::pileupExtrapolate(1)\",\"$::jobCreate::pileupExtrapolate(2)\",\"$::jobCreate::pileupExtrapolate(extrapolated)\"\);"

    set lDir [file dirname $::jobCreate::pileupExtrapolate(extrapolated)]
    set lDir [file dirname $lDir]
    set logFile [file join $lDir logs subtractPileupLog]
    set log [open $logFile w+]
    set ft .pileupExtrapolation.text.t
    puts $log "Starting subtractPileup"
    puts $log $cmd
    $ft insert end "$cmd\n" input
    $ft see end
    set fid [open "|$cmd"]
    fconfigure $fid -blocking false -buffering line
    fileevent $fid readable [namespace code [list subtractPileupReadable $fid $log]]
    set currCurs [$ft cget -cursor]
    $ft configure -cursor watch
    vwait ::SUBTRACTPILEUP-DONE
    catch {close $fid}
    $ft conf -cursor $currCurs
    close $log

    .pileupExtrapolation.actions.runSubtractPileup configure -text "Done" -command "wm withdraw .pileupExtrapolation"
}
################################################################################
# subtractPileupReadable waits for output from root4star process running subtractPileup and
#  puts it into a text widget.
################################################################################
proc ::jobCreate::subtractPileupReadable {fid fLog} {
    # The channel is readable; try to read it.
    set ft .pileupExtrapolation.text.t
    set status [catch { gets $fid line } result]
    if { $status != 0 } {
        # Error on the channel
        puts $fLog ">>>>>error reading $fid: $result<<<<<"
        $ft insert end "error reading $fid: $result\n"
        $ft see end
        set ::SUBTRACTPILEUP-DONE 2
    } elseif { [eof $fid] } {
        set ::SUBTRACTPILEUP-DONE 1
    } elseif { $result >= 0 } {
        puts $fLog $line
        $ft insert end "$line\n"
        $ft see end
    } elseif { [fblocked $fid] } {
        # Read blocked.  Just return
    } else {
        # Something else
        puts $fLog ">>>>>Something impossible happened while reading output from subtractPileup<<<<<"
        $ft insert end "What did you do?? the impossible happened while reading $fid: $result\n"
        $ft see end
        set ::SUBTRACTPILEUP-DONE 3
    }
}
################################################################################
# copyToHPSS uses htar to copy entire tree to HPSS.
################################################################################
proc ::jobCreate::copyToHPSS {} {
    if {![winfo exists .copyToHPSS]} {
        toplevel .copyToHPSS
        wm title .copyToHPSS "Copy job directory tree to HPSS"
        set tFrame [frame .copyToHPSS.tFrame]
        pack $tFrame -fill both -expand true

        text .copyToHPSS.tFrame.t -yscrollcommand {.copyToHPSS.tFrame.y set} \
                              -xscrollcommand {.copyToHPSS.tFrame.x set} -wrap word
        scrollbar .copyToHPSS.tFrame.y -command {.copyToHPSS.tFrame.t yview}
        scrollbar .copyToHPSS.tFrame.x -command {.copyToHPSS.tFrame.t xview} -orient horizontal
        grid .copyToHPSS.tFrame.t .copyToHPSS.tFrame.y
        grid .copyToHPSS.tFrame.x x
        grid .copyToHPSS.tFrame.t -sticky news
        grid .copyToHPSS.tFrame.y -sticky ns
        grid .copyToHPSS.tFrame.x -sticky we
        grid columnconfigure .copyToHPSS.tFrame 0 -weight 1
        grid rowconfigure    .copyToHPSS.tFrame 0 -weight 1
        .copyToHPSS.tFrame.t tag configure input -foreground black
        .copyToHPSS.tFrame.t tag configure normalOutput -foreground blue
        .copyToHPSS.tFrame.t tag configure errorOutput -foreground red

        button .copyToHPSS.runIt -text "Start Copy" -command [namespace code [list doCopyToHPSS]]
        pack .copyToHPSS.runIt -side left

        # Want to only allow window to be destroyed when not in action?
        #bind .copyToHPSS <Control-w> {destroy .copyToHPSS}
    } else {
        raise .copyToHPSS
    }
}
################################################################################
# doCopyToHPSS invokes htar.
################################################################################
proc ::jobCreate::doCopyToHPSS {} {
    global env
    global errorCode
    global errorInfo

    # Get output dir (root of tree for this job) and create HPSS name by stripping
    # $env(MYDATA) from start.
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set node [$::jobCreate::jobInfo getElementsByTagName jobName]
    set jobName [$node text]
    set myData \$env(MYDATA)
    if {![catch {subst $myData} v]} {
        set myData $v
    }
    set hPath [string map "$myData/ {}" $path]

    # The -P option will create the HPSS path if it doesn't exist (Couldn't find
    # this actually documented, but seems to work.)
    # Need to check if archive file already exists. Would be silently over-written.
    # hsi (at least via exec) writes to stderr but errorCode is "NONE" if there was really no erro.
    catch {exec hsi ls $hPath/$jobName.tar} result
    if {$errorCode eq "NONE"} {
        set msg "It appears that archive file
$hPath/$jobName.tar
already exists. Continue and overwrite?
        "
        set cont [tk_messageBox -message $msg -type yesno \
                -icon warning -title "Archive file already exists" -default no]
        if {!$cont} {
            return
        }
    }
        
    set logFile [file join $path copyToHPSSLog]
    if {[file exists $logFile]} {
        set log [open $logFile a]
    } else  {
        set log [open $logFile w]
    }

    # htar seems to require that pwd is in same file system as directory to archive?
    set dir [file tail $path]
    set currPwd [pwd]
    cd $path
    cd ..
    set cmd "htar -c -f $hPath/$jobName.tar -P $dir"

    puts $log "Starting to htar from disk directory $path to HPSS directory $hPath"
    puts $log $cmd
    .copyToHPSS.tFrame.t insert end "$cmd\n" input
    .copyToHPSS.tFrame.t see end

    # It appears that 
    set fid [open "|$cmd"]
    fconfigure $fid -blocking false -buffering line
    fileevent $fid readable [namespace code [list htarReadable $fid $log]]
    set currCurs [.copyToHPSS.tFrame.t cget -cursor]
    .copyToHPSS.tFrame.t configure -cursor watch

    # Closing the pipeline may kill htar. If not, I need to get the pid and exec a kill.
    .copyToHPSS.runIt configure -text "Stop copy to HPSS" \
            -command [namespace code [list stopHPSSCopy $fid $currCurs]]

    cd $currPwd

    # Wait for the fileevents (i.e. htar) to finish.
    vwait ::HTAR-DONE

    # Close the pipe
    catch {close $fid}

    .copyToHPSS.tFrame.t conf -cursor $currCurs
    .copyToHPSS.runIt configure -text Done -command "" -state disabled
}
################################################################################
# htarReadable waits for output from htar and
#  puts it into a text widget.
################################################################################
proc ::jobCreate::htarReadable {fid fLog} {
    # The channel is readable; try to read it.
    set status [catch { gets $fid line } result]
    if { $status != 0 } {
        # Error on the channel
        puts $fLog ">>>>>error reading $fid: $result<<<<<"
        .copyToHPSS.tFrame.t insert end "error reading $fid: $result\n"
        .copyToHPSS.tFrame.t see end
        set ::HTAR-DONE 2
    } elseif { [eof $fid] } {
        set ::HTAR-DONE 1
    } elseif { $result >= 0 } {
        puts $fLog $line
        .copyToHPSS.tFrame.t insert end "$line\n"
        .copyToHPSS.tFrame.t see end
    } elseif { [fblocked $fid] } {
        # Read blocked.  Just return
    } else {
        # Something else
        puts $fLog ">>>>>Something impossible happened while reading output from hadd<<<<<"
        .copyToHPSS.tFrame.t insert end "What did you do?? the impossible happened while reading $fid: $result\n"
        .copyToHPSS.tFrame.t see end
        set ::HTAR-DONE 3
    }
}
################################################################################
# stopHPSSCopy closes pipeline, hoping that is enough to kill htar.
#  Also resets cursor and button.
################################################################################
proc ::jobCreate::stopHPSSCopy {fid cursor} {
    catch {close $fid}
    .copyToHPSS.tFrame.t conf -cursor $currCurs
    .copyToHPSS.runIt configure -text "Copy cancelled" -command "" -state disabled
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
    $w insert end " Primarily for debugging/developing.\n" n
    $w insert end "- Read Job Description...\n" n
    $w insert end " Read in previously created xml description of a job. " n
    $w insert end " This file may have previously been created by this interface. " n
    $w insert end " Can also give this file name as argument when starting jobCreate.\n" n
    $w insert end "- Create Job Files\n" n
    $w insert end " Create output directory hierarchy and populate with files " n
    $w insert end " created by this interface. (Does not invoke the scheduler.)\n" n
    $w insert end "- Submit Job\n" n
    $w insert end " Create output directory hierarchy, populate it with files " n
    $w insert end " created by this interface, then invoke the scheduler.\n" n
    $w insert end "- Filter filelist\n" n
    $w insert end " Re-order *.dataset file (in scripts directory) so MuDst files are in " n
    $w insert end " sequential order. Omit MuDst files with few events and days with few events. " n
    $w insert end " Keep number of jobs the same as SUMS decided but divide into jobs with almost " n
    $w insert end " equal number of events, although don't mix MuDst files from different days. " n
    $w insert end " Note: I think this works but it has not been tested on all datasets. " n
    $w insert end "- Start jobMonitor\n" n
    $w insert end "Start a companion program, jobMonitor, which allows " n
    $w insert end "monitoring, killing, fixing and re-submitting batch jobs. " n
    $w insert end "(See that program for more detailed help on it.)\n" n
    $w insert end "- Exit (or ^C)\n" n
    $w insert end "Exit from jobCreate. If jobMonitor has been started from " n
    $w insert end "jobCreate we kill it too.\n" n

    $w insert end " o Apr\u00e8s batch\n" bullet
    $w insert end "- Add Histograms\n" n
    $w insert end "  For use after all jobs are finished (although we do keep track of which data histograms " n
    $w insert end "have been added so you can run this before all batch jobs are done, just to get a look). " n
    $w insert end "Add all cuts histograms to Cuts.root, all QA histograms " n
    $w insert end "to QA.root and all dataHistograms. For StEStructCorrelation " n
    $w insert end "we end up with one DataN.root histograms for each " n
    $w insert end "centrality N. For StEStructFluctuations we end up with one " n
    $w insert end "file, Data.root, containing all centralities internally. " n
    $w insert end "Because of root memory limitation, we add small groups into intermediate files, then " n
    $w insert end "sum all these groups together, cleaning up the intermediate " n
    $w insert end "files at the end.\n " n
    $w insert end " For each centrality you can select how many files to add as a subgroup.\n" n
    $w insert end " The \"Added files\" buttons shows which files have been added.\n" n
    $w insert end " The \"Files to add\" buttons allows you to exclude specific files from being added.\n" n
    $w insert end " The \"Add em up\" button at the bottom will use the interactive node you " n
    $w insert end "are on to add files sequentially (one centrality at a time).\n " n
    $w insert end "The \"Add using batch\" will create necessary files so we can run hadd in batch. " n
    $w insert end "You can monitor progress using the jobMonitor. I figured out how to hold and release " n
    $w insert end "the jobs to sum the sub-groups on SGE. For condor I don't know how to release " n
    $w insert end "those jobs when the sub-groups have finished adding. You must do that yourself " n
    $w insert end "(with the jobMonitor it is easy.) \n\n" n
    $w insert end "- Combine centralities\n" n
    $w insert end "  For StEStructCorrelation only. \n" n
    $w insert end "Allows user to specify which of the Data{n} files (where n " n
    $w insert end "is a centrality bin) to merge creating Sum{n1}_{n2}. " n
    $w insert end "Original Data{n} files are left. Merge is done by renaming " n
    $w insert end "histograms (in the Sum{n1}_{n2} file) to include the centrality bin in the " n
    $w insert end "the z vertex bin tag. \n" n
    $w insert end "  As in \"Add Histograms\" you can \"Add em up\" on the interactive node " n
    $w insert end "or \"Add in batch\" to submit as batch jobs. Again, use jobMonitor to see " n
    $w insert end "status of batch jobs \n\n" n
    $w insert end "- selectAll macro\n" n
    $w insert end "  For StEStructCorrelation only. \n" n
    $w insert end " There are three sections in this window.\n " n
    $w insert end "combine Jobs.\n" n
    $w insert end " This is for combining two or more sub-jobs. Useful when you analyse FullField and " n
    $w insert end "ReversedFullField separately, as an example. Specify the existing job directories " n
    $w insert end "to combine.\n " n
    $w insert end " create Sum Directory will create the new directory and merge appropriate files. " n
    $w insert end "It will leave the input directories alone (in case anything should go wrong).\n " n
    $w insert end " another Job Directory, in case you want to merge more than two.\n " n
    $w insert end " guess Directories looks for directory names with FullField and ReversedFullField in them.\n " n
    $w insert end "Invoke selectAll{m} with...\n " n
    $w insert end " allows user to speficy which Data{n} and Sum{n1}_{n2} files " n
    $w insert end "to run through selectAllM{mode} macro (via \"Run selectAll\" (interactively) " n
    $w insert end "or \"Batch selectAll\" (as batch jobs)). This macro combines cut bins " n
    $w insert end "but doesn't create \u0394\u03c1/\u221a\u03c1_ref histograms. \n" n
    $w insert end "Group into \u0394\u03c1/\u221a\u03c1_ref histogram file (using combineHistograms{m})\n " n
    $w insert end " (After the selectAllM{mode} macro has been run) you specify which " n
    $w insert end "files to combine into \u0394\u03c1/\u221a\u03c1_ref histograms " n
    $w insert end "via the combineHistograms{mode} macro. Output of this macro " n
    $w insert end "is a root histogram file that can be used without the STAR library. " n
    $w insert end "You may wish to create more than one \u0394\u03c1/\u221a\u03c1_ref histogram file " n
    $w insert end "(for example if you want minimum bias and centrality dependent histograms) " n
    $w insert end "and you can use \"Add \u0394\u03c1/\u221a\u03c1_ref file for this purpose. " n
    $w insert end "The combineHistograms{mode} macro can be run on the interactive node " n
    $w insert end "(via \"Create \u0394\u03c1/\u221a\u03c1_ref files\") or in batch " n
    $w insert end "(via \"Batch \u0394\u03c1/\u221a\u03c1_ref files\".)\n\n" n
    $w insert end "- copy to HPSS\n" n
    $w insert end "  Copy the output directory tree to HPSS using htar. \n" n
    $w insert end "This will result in a tar file named \$jobName.tar (jobName is described " n
    $w insert end "in the jobControl section below). The resulting HPSS directory (which will " n
    $w insert end "be created if it doesn't exist) is taken from outputDir (described in " n
    $w insert end "the jobControl section below) with the contents of the environment " n
    $w insert end "variable MYDATA stripped from the start. If the archive file already " n
    $w insert end "exists you will be warned and given options to over-write it or not. \n\n" n


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
    $w insert end "that are used in the pythia initialization call.\n" n

    $w insert end " o Therminator:\n" bullet
    $w insert end "- This allows selection from 'standard' Therminator " n
    $w insert end "parameter sets. The user will be able modify all of the Therminator " n
    $w insert end "parameters (that are accessible via therminator.in.)  \n" n
    $w insert end "NOTE: For each new parameter set Therminator will " n
    $w insert end "integrate over particle distributions and store results in " n
    $w insert end "fintegrandmax_*.txt and fmultiplicity_.txt files. " n
    $w insert end "I recommend either creating these files outside of ADDS " n
    $w insert end "or run a single job and wait for those files before " n
    $w insert end "submitting the rest of them.\n\n " n

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
    $w insert end "where the output root files are written. After the job finishes " n
    $w insert end "the files are copied to outputDir. " n
    $w insert end "We also use this as the name of the tar file when you " n
    $w insert end "use the \"copy to HPSS\" menu (see Menus/Apr\u00e8s batch section above). \n" n

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
    $w insert end "Information cannot be modified in this window, but changes " n
    $w insert end "made via other widgets are automatically shown.\n\n" n

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
    $w insert end "depend on whether you want to do a 2pt correlation or a fluctuation " n
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

    $w insert end " o keepZBuffers\n" bullet
    $w insert end "- Space separated list. For each non-zero value we write separate histograms " n
    $w insert end "for each z-buffer. This is important for 2pt analysis of central events. " n
    $w insert end "If this list does not have one value for each centrality bin we ignore it. " n
    $w insert end "This flag currently only makes sense for 2pt correlations.\n\n" n

    $w insert end " o analysisMode\n" bullet
    $w insert end "- Bit pattern passed to constructor of analsis object.\n\n" n

    $w insert end " o useGlobalTracks\n" bullet
    $w insert end "- If true MuDstReader will use global tracks instaed of primaries." n
    $w insert end "- We calculate eta, phi and impact parameter from outerHelix.\n\n" n

    $w insert end " o sortEvents\n" bullet
    $w insert end "- I am working on this option." n
    $w insert end "Currently we can sort Hijing events by multiplicity. " n
    $w insert end "I hope to be able to sort data by z-vertex position and " n
    $w insert end "multiplicity. This should minimize plaids and may allow " n
    $w insert end "us to do away with z-vertex binning of mixed events.\n\n" n

    $w insert end " o declareAnalysis, declareReader, allocateAnalysis, preLoop, preEvent, postEvent, postLoop\n" bullet
    $w insert end "- These buttons allow you to select colors. " n
    $w insert end "The code snippets associated with these elements can be seen " n
    $w insert end "and modified in the edit window\".\n\n" n

    $w insert end " o weightsFile\n" bullet
    $w insert end "- For use in reaction plane analyses. \n\n" n

    $w insert end " o doEStruc.C edit button\n" bullet
    $w insert end "- Complete doEStruct macro. Clicking on the button will bring up a " n
    $w insert end "window showing the text. It automatically reflects " n
    $w insert end "changes that modify its information (e.g. centralities). " n
    $w insert end "You can modify some of the code snippets in this window " n
    $w insert end "(see list above). Menus allow you to undo/redo as well as " n
    $w insert end "save/revert. Information is not actually used until saved.\n\n" n


    $w insert end "Next options are only in StEStructCorrelation:\n\n" n

    $w insert end " o cutMode\n" bullet
    $w insert end "- Flag that chooses actual cut binning mode. \n\n" n


    $w insert end "Next options are only in StEStructFluctuation:\n\n" n

    $w insert end " o ptCuts\n" bullet
    $w insert end "- In addition to doing analysis for complete pt range specified " n
    $w insert end "by track cuts we do analysis in these bins. \n\n" n

    $w insert end " o ptCentralities\n" bullet
    $w insert end "- All pt bins have same centrality selection but this is " n
    $w insert end "different than the cuts for the entire pt range. \n\n" n

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

    $w insert end " o pyTune\n" bullet
    $w insert end "- Chooses a group of options used to tune Pythia for a particular physics case.\n\n" n


    $w insert end "therminatorParams\n" header

    $w insert end " o thRandomize\n" bullet
    $w insert end "- Start each event with a new random seed taken from current time\n" n
    $w insert end " o thTableType\n" bullet
    $w insert end "- Only choice is SHARE (for now).\n" n
    $w insert end " o thOutputFile\n" bullet
    $w insert end "- Normally we don't want to save the generated files, so leave this blank. " n
    $w insert end "If you really want to save generated events in Therminator text format " n
    $w insert end "then put the entire path here. \n" n
    $w insert end " o thModel\n" bullet
    $w insert end "- Choice of blastwave or single freeze out.\n" n
    $w insert end " o thBWVt\n" bullet
    $w insert end "- Only used in blastwave model. Radial Flow velocity.\n" n
    $w insert end " o thTau\n" bullet
    $w insert end "- Proper time at freeze-out \[fm\]\n" n
    $w insert end " o thRhoMax\n" bullet
    $w insert end "- Maximum transverse radius \[fm\]\n" n
    $w insert end " o thTemperature\n" bullet
    $w insert end "- Temperature \[GeV\]\n" n
    $w insert end " o thMiuI\n" bullet
    $w insert end "- Chemical potential for isospin \[GeV\]\n" n
    $w insert end " o thMiuS\n" bullet
    $w insert end "- Chemical potential for strangeness \[GeV\]\n" n
    $w insert end " o thMiuB " bullet
    $w insert end "- Chemical potential for baryon \[GeV\]\n" n
    $w insert end " o thAlphaRange\n" bullet
    $w insert end "- Range of integration for z-variable\n" n
    $w insert end " o thRapidityRange\n" bullet
    $w insert end "- Range of integration for z-variable\n" n
    $w insert end " o thNumberOfIntegrateSamples " bullet
    $w insert end "- Number of samples used in multiplicity and max. integrand determination.\n" n



    $w config -state disabled
}
