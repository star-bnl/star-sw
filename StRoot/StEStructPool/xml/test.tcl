
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
    variable showingXML
    variable showingMain
    variable undone
    variable lastNode
    variable numberOfInstances
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
    if {[string equal [file extension $fileName] ".xml"]} {
        set fh [open $fileName]
        set domInfo [dom parse [read $fh]]
        close $fh
        set ::jobCreate::jobInfo [$domInfo documentElement]
    } elseif {[string equal [file extension $fileName] ".lis"]} {
        set fh [open $fileName]
        set dir [file dir $fileName]
        set fDom [open [file join $dir [gets $fh]]]
        set domInfo [dom parse [read $fDom]]
        close $fDom
        set ::jobCreate::jobInfo [$domInfo documentElement]
        while {[gets $fh mFile] >= 0} {
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
    if {[string equal [$n2 nodeType] "COMMENT_NODE"]} {
        return
    }
    set p  [$n2 toXPath]
    set n1 [$doc1 selectNodes $p]
    if {[string equal $n1 ""]} {
        # n2 not found in doc1. The asXML command will include all children
        # so we do not need to recurse for this node.
        set par2 [$n2 parentNode]
        set p [$par2 toXPath]
        set n1 [$doc1 selectNodes $p]
        set x [$n2 asXML]
        $n1 appendXML $x
        return
    }
    if {[string equal [$n2 nodeName] "eventCut"] ||
        [string equal [$n2 nodeName] "trackCut"]} {
        # There can be multiple cut nodes. Need to check cutName to
        # see if they are the same or different.
        mergeCuts $n2 $doc1
        return
    } else {
        # Copy n2 nodeValue and attributes to doc1.
        # Need to recurse to merge children of n2.
        if {[string compare [$n2 nodeValue] ""]} {
            $n1 nodeValue [$n2 nodeValue]
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
    if {[string equal $n1 ""]} {
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
    if {[string compare $v ""]} {
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
proc ::jobCreate::getNewSchema { {fileName {}} } {
    if {[string equal $fileName {}]} {
        set fileName [tk_getOpenFile -filetypes {{schema {.xsd}} {any {.*}}}]
    }
    if {[string compare $fileName ""]} {
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
    set  ::jobCreate::interfaceWindow [toplevel .interfaceWindow -width 400 -height 566]
    wm title $::jobCreate::interfaceWindow "STAR analyisis batch job submission interface"

    set m [menu $::jobCreate::interfaceWindow.menu]
    $::jobCreate::interfaceWindow configure -menu $m
    set file [menu $m.file]
    $m add cascade -label File -menu $file
    $file add command -label "New Schema File..." -command [namespace code getNewSchema]
    $file add command -label "Create Job Files" -command [namespace code createJobFiles]
    $file add command -label "Submit Job..." -command [namespace code submitJob]
    $file add command -label "Start jobMonitor" -command [namespace code startJobMonitor]
    $file add command -label Exit -command [namespace code exit]
    set edit [menu $m.edit]
    $m add cascade -label Edit -menu $edit
    $edit add command -label PlaceHolder
    set help [menu $m.help]
    $m add cascade -label Help -menu $help
    $help add command -label "Another PlaceHolder"
    pack [frame $::jobCreate::interfaceWindow.all]

    +JobDescriptionFrame $::jobCreate::interfaceWindow.all
}
################################################################################
# Creates buttons for choices offered in jobDescription
################################################################################
proc ::jobCreate::+JobDescriptionFrame {t} {
    set f [frame $t.jobDescription]
    pack $f -fill x
    foreach jNode [$::jobCreate::schemaInfo selectNodes //*\[@name='jobType'\]//xs:element\[@name\]] {
        set type [$jNode getAttribute name]
        button $f.b$type -text $type -command [namespace code [list +listDefaultXml $type $t $t.f.cSelectJobXml]]
        set cNode [$jNode selectNodes ./*/*\[@Comment\]]
        set comm [$cNode getAttribute Comment]
        DynamicHelp::register $f.b$type balloon $comm
        pack $f.b$type -side left -anchor nw
    }

    set f [frame $t.f]
    pack $f -fill x
    button $f.newfDefault -pady 1 -text Browse \
            -command [namespace code [list getJobXmlFile $t]]

    set ::jobCreate::jobXmlFile ""
    ComboBox::create $f.cSelectJobXml \
            -editable  false          \
            -textvariable ::jobCreate::jobXmlFile  \
            -width     30             \
            -modifycmd "$f.bSelectJobXml configure -state normal"
    button $f.bSelectJobXml -pady 1 \
            -text "Use selection"   \
            -state disabled         \
            -command [namespace code "+Type $t \[file join $::jobCreate::jobCreatePath jobFiles \[$f.cSelectJobXml get\]\]"]
    pack $f.newfDefault $f.cSelectJobXml $f.bSelectJobXml -side left -anchor nw
}
################################################################################
# Extract list of default xml job description files for selected type of job.
################################################################################
proc ::jobCreate::+listDefaultXml {type t c} {
    set vals [list]
    switch $type {
        Data {
            set vals [list dataAuAu19_2001_MinBias.lis \
                           dataAuAu62_2004_MinBias.lis \
                           dataAuAu130_2000_MinBias.lis \
                           dataAuAu200_2001_MinBiasVertex.lis \
                           dataAuAu200_2001_ProductionMinBias.lis]
        }
        GEANT {
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
        }
    }
    set ::jobCreate::jobXmlFile ""
    $c configure -values $vals
    $c setvalue first
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
proc ::jobCreate::getJobXmlFile {t} {
    set fileName [tk_getOpenFile -filetypes {{schema {.xml}} {any {.*}}}]
    if {[string compare $fileName ""]} {
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
        set ::jobCreate::jobXmlFile $fileName
        +Type $t $fileName
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
        if {![string equal $name "jobPurpose"]} {
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
        if {[string equal $el "command"]} {
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
        set sN [$::jobCreate::schemaInfo selectNodes //*\[@name='$name'\]]
        set cNode [$sN selectNodes .//*\[@name='Comment'\]]
        set comment [$cNode text]
        set wNode [$sN selectNodes .//*\[@name='widget'\]]
        set comment [$wNode getAttribute default]

        set node [$jNode getElementsByTagName $name]
        if {[lsearch [$node attributes] widget] >= 0} {
            set wType [$node getAttribute widget]
        }
        if {[string equal $wType "entry"]} {
            label $f.$name -text $name
            variable code${f}$name [$node text]
            entry $f.${name}Code                \
                    -textvariable ::jobCreate::code${f}$name \
                    -width 40                   \
                    -validate all               \
                    -vcmd [namespace code "modifyMacroNode $node %P %W %V"]
        } elseif {[string equal $wType "text"]} {
            label  $f.$name -text $name
            if {[string equal $name "main"]} {
                set bl "show"
            } else {
                set bl "edit"
            }
            button $f.${name}Code -text $bl -pady 1 \
                    -command [namespace code "displayCode $node {Part of doEStruct.C}
                              wm deiconify .fileText
                              raise .fileText"]
        } else {
            continue
        }
        ::DynamicHelp::register $f.$name balloon $comment
        grid $f.$name $f.${name}Code -sticky w
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
proc ::jobCreate::+pythiaMacro {f} {
}

################################################################################
# When entry is modified we want to update the dom.
# This routine checks that the new value is valid.
################################################################################
proc ::jobCreate::modifyNode {node val e cond} {
    set nChild [$node childNodes]
    set curr [$nChild nodeValue]
    $nChild nodeValue $val
    if {[catch {is-a [$node nodeName] [$node asList]}]} {
        $nChild nodeValue $curr
        $e configure -foreground red
        return true
    }
    $e configure -foreground black
    if {![string equal $cond "focusin"] && ![string equal $cond "focusout"]} {
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

    if {![string equal $cond "focusin"] && ![string equal $cond "focusout"]} {
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
    if {[string equal $type "eventCut"]} {
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
    if {[string equal $jNode ""]} {
        return
    }
    set node [$jNode getElementsByTagName triggerTag]
    if {[string compare $node ""]} {
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
    if {[string equal $cut triggerTag]} {
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
    if {[string equal $jNode ""]} {
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
    if {[string equal $cut triggerTag]} {
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
    if {[string equal $cut triggerTag]} {
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
            -modifycmd "modifyNode $node \[$f.combo$cut get\] $f.combo$cut %V"
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

        text .fileText.t -yscrollcommand {.fileText.y set} \
                         -xscrollcommand {.fileText.x set} -wrap none \
                         -undo true
        scrollbar .fileText.y -command {.fileText.t yview}
        scrollbar .fileText.x -command {.fileText.t xview} -orient horizontal
        grid .fileText.t .fileText.y
        grid .fileText.x ^
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
    if {[string equal "main" [$node nodeName]]} {
        wm title .fileText "doEStruct.C: After current substitutions."
        .fileText.t delete 0.0 end
        .fileText.t insert end [applyXsl doEStruct.xsl asText]
        set ::jobCreate::showingMain true
        .fileText.t configure -state disabled
    } elseif {[string equal "starSubmit" [$node nodeName]]} {
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
}
################################################################################
# checkFile is invoked when Edit menu is posted.
# It checks if the text has been modified and enables/disables
# undo/redo menus to reflect current state.
################################################################################
proc ::jobCreate::checkFile {m t} {
    if {$::jobCreate::showingMain || $::jobCreate::showingXML} {
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
        if {[string equal [$cn nodeName] "#text"]} {
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
    if {$::jobCreate::showingMain || $::jobCreate::showingXML} {
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
proc ::jobCreate::m+XmlAtts {f el node {label {}}} {
    set sNode [$::jobCreate::schemaInfo selectNodes //*\[@name='$el'\]]
    set atts [list]
    foreach tN [$sNode selectNodes ./*/xs:attribute] {
        lappend atts [$tN getAttribute name]
    }

    if {[string equal $label {}]} {
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
    if {[string equal $label {}]} {
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
    if {[string equal $el "starSubmit"]} {
        grid $f.del$att $f.name$att $f.e$att -sticky w
    } else {
        grid x $f.del$att $f.name$att $f.e$att -sticky w
    }

    set new [$::jobCreate::schemaInfo selectNode //*\[@name='$el'\]]
    if {[string equal [$new nodeName] "xs:element"]} {
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
            set title "Directory $path already exists"
            set msg   "The directory, $path, already exists. \
                     Do you want to brutally destroy it?"
            set cont [tk_messageBox -message $msg -type yesno \
                    -icon warning -title $title -default no]
            if {!$cont} {
                return false
            }
            file delete -force $path
        }
        if {[file isfile $path]} {
            set title "A file $path exists"
            set msg   "A file, $path, exists. \
                     Do you want to brutally destroy this \
                     file and replace it with a directory structure?"
            set cont [tk_messageBox -message $msg -type yesno \
                    -icon warning -title $title -default no]
            if {!$cont} {
                return false
            }
            file delete $path
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

    # Create CutsFile with event and track cuts.
    set f [open $path/scripts/CutsFile.txt w]
    puts $f [applyXsl eventCuts.xsl asText]
    puts $f [applyXsl trackCuts.xsl asText]
    close $f
    # Create doEStructMacro
    set f [open $path/scripts/doEStruct.C w]
    puts $f [applyXsl doEStruct.xsl asText]
    close $f
    # Create xml file for star-submit
    # First line of xml file should be <?xml ... ?> which I don't get from asXML
    set f [open $path/scripts/job.xml w]
    puts $f "<?xml version='1.0' encoding='utf-8' ?>"
    puts $f [applyXsl job.xsl asXML]
    close $f

    # Might have Hijing Parameters
    set hNode [$::jobCreate::jobInfo getElementsByTagName hijingParams]
    if {[string compare $hNode ""]} {
        set f [open $path/scripts/hijev.inp w]
        puts $f [applyXsl hijingParams.xsl asText]
        close $f
    }
    # Might have Pythia macro
    set pNode [$::jobCreate::jobInfo getElementsByTagName pythiaMacro]
    if {[string compare $pNode ""]} {
#>>>>>
        set f [open $path/scripts/pythiaMacro.C w]
        puts $f [applyXsl pythiaMacro.xsl asText]
        close $f
    }

    # Save the dom tree as an xml file
    # (use value of jobName for the file name.)
    set node [$::jobCreate::jobInfo selectNodes //jobName]
    set f [open $path/scripts/[$node text].xml w]
    puts $f "<?xml version='1.0' encoding='utf-8' ?>"
    puts $f [$::jobCreate::jobInfo asXML]
    close $f

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

    # Wait for the file events to finish
    vwait ::DONE

    # Close the pipe
    close $fid

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
proc ::jobCreate::startJobMonitor {} {
    source [file join $::jobCreate::jobCreatePath jobMonitor.tcl]
    ::jobMonitor::createWindow
    set node [$::jobCreate::jobInfo getElementsByTagName outputDir]
    set path [$node text]
    set ::jobMonitor::scriptDir [file join $path scripts]
    set ::jobMonitor::logDir    [file join $path logs]
}
################################################################################
# exit deletes the jobCreate main window and destroys the namespace.
################################################################################
proc ::jobCreate::exit {} {
    destroy $::jobCreate::interfaceWindow
    namespace delete ::jobCreate::
    ::exit
}
