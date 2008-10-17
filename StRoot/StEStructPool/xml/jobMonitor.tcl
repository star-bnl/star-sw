
package require Tk
package require BWidget

# agrep.tcl --
#    Script to emulate the UNIX grep command with a small GUI
#
# That was the starting point. I expanded the program a bit since then.

namespace eval ::jobMonitor:: {
    variable scriptDir
    variable logDir
    variable pattern ""
    variable matchResults
    variable matchFrame
    variable ignoreCase yes

    variable patternCount
    variable tagName
    variable tagColor   [list #ffff00 #ffcc00 #ccff00 #ff6600 #ff2200]
    variable LOGTYPE    .log
    variable bWindow
    variable jobList ""
    variable actionList
    variable viewMenu
    variable selectedJob ""
    variable anchorJob ""
    variable IOwnProcess false
}

################################################################################
# This lremove was extracted from tkcon because I got used to using the command
# and want it available when not using tkcon.
################################################################################
proc ::jobMonitor::lremove {args} {
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

# createWindow --
#    Create the main window
#
# Arguments:
#    None
# Result:
#    None
# Side effects:
#    Controls added to main window
#
# Wanted to allow multiple windows to exist (which I can do easily)
# but I also need to put each incarnation into a separate namespace.
proc ::jobMonitor::createWindow {{scriptDir ""} {logDir ""}} {
    set window .jobMonitor

    set ::jobMonitor::bWindow [toplevel $window]
    set m [menu $::jobMonitor::bWindow.menu]
    $::jobMonitor::bWindow configure -menu $m

    set file [menu $m.file]
    $m add cascade -label File -menu $file
#    $file add command -label "Select all jobs"           -command [namespace code selectAll]
    set d [menu $file.select -postcommand [namespace code [list countTypes $file.select selectJobs]]]
    $file add cascade -label "Select jobs" -menu $d
    $d add command -label all -command [namespace code [list selectJobs all]]
    set d [menu $file.deselect -postcommand [namespace code [list countTypes $file.deselect deselectJobs]]]
    $file add cascade -label "Deselect jobs" -menu $d
    $d add command -label all -command [namespace code [list deselectJobs all]]
    set d [menu $file.toggle -postcommand [namespace code [list countTypes $file.toggle toggleSelect]]]
    $file add cascade -label "Toggle select" -menu $d
    $d add command -label all -command [namespace code [list toggleSelect all]]

    $file add command -label "Submit selected jobs"      -command [namespace code submitSelected]
    $file add command -label "Kill selected jobs"        -command [namespace code killSelected]
    $file add command -label "Clear selected job errors" -command [namespace code clearErrors]
    $file add command -label "Update status"             -command [namespace code updateStatusIndicators]
    $file add command -label Exit -command [namespace code exit]

    set edit [menu $m.edit]
    $m add cascade -label Edit -menu $edit
    $edit add checkbutton -label "Ignore case"   -variable ::jobMonitor::ignoreCase
    $edit add command     -label Editor          -command [namespace code chooseEditor]
    $edit add command     -label "log file type" -command [namespace code setFileType]

    set help [menu $m.help]
    $m add cascade -label Help -menu $help
    $help add command -label Help -command [namespace code [list displayHelp ""]]

    # Create menu we will use in a popup to select a file to edit (or peek if job is running.)
    set ::jobMonitor::viewMenu [menu $::jobMonitor::bWindow.view -postcommand [namespace code checkPopup]]
    $::jobMonitor::viewMenu add command -label "edit csh"      -command [namespace code "editType script"]
    $::jobMonitor::viewMenu add command -label "edit log"      -command [namespace code "editType log"]
    $::jobMonitor::viewMenu add command -label "edit filelist" -command [namespace code "editType filelist"]

    #
    # Fill in fields
    #
    frame       $::jobMonitor::bWindow.f1
    label       $::jobMonitor::bWindow.f1.source_label    -text "scripts directory:" -justify left
    entry       $::jobMonitor::bWindow.f1.scriptDir       -textvariable ::jobMonitor::scriptDir
    button      $::jobMonitor::bWindow.f1.selectScriptDir -text "Browse" -command [namespace code getScriptDirectory]
    label       $::jobMonitor::bWindow.f1.result_label    -text "logs directory:" -justify left
    entry       $::jobMonitor::bWindow.f1.logDir          -textvariable ::jobMonitor::logDir
    button      $::jobMonitor::bWindow.f1.selectLogDir    -text "Browse" -command [namespace code getLogDirectory]

    label       $::jobMonitor::bWindow.f1.patt_label  -text "Regular expression:" -justify left
    entry       $::jobMonitor::bWindow.f1.pattern     -textvariable ::jobMonitor::pattern
    button      $::jobMonitor::bWindow.f1.search      -text "Search" -command [namespace code searchFiles]
    bind $::jobMonitor::bWindow.f1.pattern  <Return> [namespace code searchFiles]

    grid $::jobMonitor::bWindow.f1.source_label $::jobMonitor::bWindow.f1.scriptDir  $::jobMonitor::bWindow.f1.selectScriptDir -sticky we
    grid $::jobMonitor::bWindow.f1.result_label $::jobMonitor::bWindow.f1.logDir  $::jobMonitor::bWindow.f1.selectLogDir -sticky we
    grid $::jobMonitor::bWindow.f1.patt_label   $::jobMonitor::bWindow.f1.pattern    $::jobMonitor::bWindow.f1.search          -sticky we
    grid columnconfigure    $::jobMonitor::bWindow.f1 1 -weight 1

    pack $::jobMonitor::bWindow.f1 -side top -fill x

    #
    # Result window
    #
    frame $::jobMonitor::bWindow.f2
    text  $::jobMonitor::bWindow.f2.text -font "Courier 10" -wrap word \
                           -yscrollcommand {$::jobMonitor::bWindow.f2.y set}   \
                           -xscrollcommand {$::jobMonitor::bWindow.f2.x set}
    scrollbar $::jobMonitor::bWindow.f2.x -command {$::jobMonitor::bWindow.f2.text xview} -orient horizontal
    scrollbar $::jobMonitor::bWindow.f2.y -command {$::jobMonitor::bWindow.f2.text yview}

    grid $::jobMonitor::bWindow.f2.text $::jobMonitor::bWindow.f2.y
    grid $::jobMonitor::bWindow.f2.y -sticky ns
    grid $::jobMonitor::bWindow.f2.text -sticky news
    grid $::jobMonitor::bWindow.f2.x    x     -sticky we
    grid columnconfigure $::jobMonitor::bWindow.f2 0 -weight 1
    grid rowconfigure    $::jobMonitor::bWindow.f2 0 -weight 1

    pack $::jobMonitor::bWindow.f2 -side top -expand true -fill both

    #
    # add labels to display the number of matches.
    #
    set ::jobMonitor::matchResults "Files:"
    set ::jobMonitor::matchFrame [frame $::jobMonitor::bWindow.f3]
    label $::jobMonitor::bWindow.f3.matches -textvariable ::jobMonitor::matchResults -anchor w
    pack $::jobMonitor::bWindow.f3.matches -side left
    pack $::jobMonitor::bWindow.f3 -side left

    #
    # Just for the fun of it: define the styles for the "matched",
    # "error" and "fn" tags
    #
    $::jobMonitor::bWindow.f2.text tag configure "fn"      -underline 1 -background lightblue
    $::jobMonitor::bWindow.f2.text tag configure "error"   -background red
    # bind the filename tag to pop-up another window with
    # the complete text of that file.
    $::jobMonitor::bWindow.f2.text tag bind "fn" <Button-1> [namespace code {displayFile [getFileName]}]
    $::jobMonitor::bWindow.f2.text tag bind "fn" <Button-3> [namespace code {popupMenu %X %Y}]
    $::jobMonitor::bWindow.f2.text tag bind "fn" <Enter>    [namespace code "$::jobMonitor::bWindow.f2.text config -cursor left_ptr"]
    $::jobMonitor::bWindow.f2.text tag bind "fn" <Leave>    [namespace code "$::jobMonitor::bWindow.f2.text config -cursor [$::jobMonitor::bWindow.f2.text cget -cursor]"]


    # On startup display the help.
    displayHelp $::jobMonitor::bWindow.f2.text

    # Minimize possible confusion by disabling edits of text widget.
    $::jobMonitor::bWindow.f2.text config -state disabled

    # If scriptDir is passed in but not logDir assume scriptDir is parent
    # of script and logs directories.
    set title jobMonitor
    if {$scriptDir ne ""} {
        set title $scriptDir
        if {$logDir eq ""} {
            set logDir $scriptDir/logs
            set scriptDir $scriptDir/scripts
        }
    }
   set ::jobMonitor::scriptDir $scriptDir
   set ::jobMonitor::logDir    $logDir
    wm title $::jobMonitor::bWindow $title
}
# searchFiles --
#    Search for files in the current directory that match the given
#    mask
#
# Arguments:
#    None
# Result:
#    None
# Side effects:
#    Calls "searchPattern" to fill the result window
#
proc ::jobMonitor::searchFiles {} {
    if {[info exists ::jobMonitor::patternCount]} {
        unset ::jobMonitor::patternCount
    }
    if {[info exists ::jobMonitor::tagName]} {
        unset ::jobMonitor::tagName
    }

    $::jobMonitor::bWindow.f2.text config -state normal -wrap none
    # Delete old tags, create new tags and clear count of matches.
    $::jobMonitor::bWindow.f2.text tag delete [$::jobMonitor::bWindow.f2.text tag names]
    set iTag 0
    foreach pat $::jobMonitor::pattern {
        if {$iTag > [llength $::jobMonitor::tagColor]} {
            set col [lrange $::jobMonitor::tagColor end end]
        } else {
            set col [lrange $::jobMonitor::tagColor $iTag $iTag]
        }
        set ::jobMonitor::patternCount($pat) 0
        set ::jobMonitor::tagName($pat) tag$iTag
        $::jobMonitor::bWindow.f2.text tag configure tag$iTag -underline 1 -background $col
        incr iTag
    }

    #
    # Clear the result window, then get a list of files
    #
    $::jobMonitor::bWindow.f2.text delete 0.1 end

    if { $::jobMonitor::logDir == "" } {
       set ::jobMonitor::logDir [pwd]
    }

    # Get list of files.
    # For every job file in the scripts directory we look for
    # a corresponding log file in the logs directory.
    # If we find the log file we append it to the list.
    # Require log file to have size bigger than 0.
    # If we do not find the log file append the scripts file.
    set ::jobMonitor::jobList    [list]
    set ::jobMonitor::actionList [list]
    set fileList [list]
    foreach file [lsort -dictionary [glob $::jobMonitor::scriptDir/sched*.csh]] {
        if { ![file isdirectory $file] } {
            set fileName [file tail $file]
            set fileName [file rootname $fileName]
            set fileName [string map "sched {}" $fileName]
            lappend ::jobMonitor::jobList $fileName
            set logFile [file join $::jobMonitor::logDir $fileName$::jobMonitor::LOGTYPE]
            if {[file exists $logFile] && [file size $logFile] > 0} {
                lappend fileList $logFile
            } else {
                lappend fileList $file
            }
        }
    }
    set nFiles [llength $fileList]

    # Now we search each of the files for patterns.
    # During the search we insert the file name and any text matches
    # into the text widget.
    foreach w [winfo children $::jobMonitor::bWindow.f2.text] {
        destroy $w
    }
    foreach job $::jobMonitor::jobList  file $fileList {
        searchPattern $job $file $::jobMonitor::pattern $::jobMonitor::ignoreCase
    }
    set ::jobMonitor::matchResults "Files: $nFiles"
    foreach w [winfo children $::jobMonitor::matchFrame] {
        if {"$w" ne "$::jobMonitor::matchFrame.matches"} {
            destroy $w
        }
    }
    set iCol 0
    $::jobMonitor::bWindow.f2.text mark set insert 1.0
    foreach pat $::jobMonitor::pattern {
        ::jobMonitor::addMatchWidgets $pat $::jobMonitor::patternCount($pat) $iCol
        incr iCol
    }
    $::jobMonitor::bWindow.f2.text config -state disabled
}

# searchPattern --
#    Search for lines containing the given pattern in a file
#
# Arguments:
#    filename      Name of the file to be searched
#    pattern       Given regular expression
#    ignoreCase   Ignore the case or not
# Result:
#    None
# Side effects:
#    Creates a line with a checkbutton, text widget and filename.
#    Matches in the file are inserted on subsequent lines.
#    The checkbutton indicates including this job in an action.
#    The text widget is to indicate lsf (or sge) status
#
#
proc ::jobMonitor::searchPattern {jobName filename pattern ignoreCase} {
    if { [ catch {
        set infile [open $filename "r"]

        set file [file tail $filename]
        variable ::jobMonitor::cbVar$jobName 0
        set cb [checkbutton $::jobMonitor::bWindow.f2.text.cb$jobName \
                -variable ::jobMonitor::cbVar$jobName]
        bind $cb <Button-1> [namespace code "selectJob $jobName"]
        bind $cb <Shift-Button-1> [namespace code "selectJobRange $jobName"]
        bind $cb <Control-Button-1> [namespace code "toggleJob $jobName"]
        variable var$jobName
        set stat [button $::jobMonitor::bWindow.f2.text.stat$jobName \
                -textvariable ::jobMonitor::var$jobName -width 5 -anchor w \
                -padx 0 -pady 0 \
                -command [namespace code "updateStatusIndicators $jobName"]]
        colorStatusIndicator $stat [set var$jobName]

        $::jobMonitor::bWindow.f2.text window create end -window $cb
        $::jobMonitor::bWindow.f2.text window create end -window $stat
        $::jobMonitor::bWindow.f2.text insert end "$file" fn
        set posE [$::jobMonitor::bWindow.f2.text index end]

        # Read the file line by line.
        # As soon as we find a match add the line to the window.
        # After all lines have been added we search and tag them for ALL matches.
        while { [gets $infile line] >= 0 } {
            foreach pat $pattern {
                if { $ignoreCase } {
                    set match [regexp -nocase -indices -- $pat $line indices]
                } else {
                    set match [regexp -indices -- $pat $line indices]
                }
                if { $match } {
                    $::jobMonitor::bWindow.f2.text insert end "\n"
                    $::jobMonitor::bWindow.f2.text insert end $line
                    break
                }
            }
        }
        close $infile
        foreach pat $pattern {
            # posE seems to be one line too far.
            # I'm not sure why.
            set posM $posE-1l
            if { $ignoreCase } {
                while {[set pos [$::jobMonitor::bWindow.f2.text search -nocase -count length -regexp -- $pat $posM end]] > 0} {
                    incr ::jobMonitor::patternCount($pat)
                    $::jobMonitor::bWindow.f2.text tag add $::jobMonitor::tagName($pat) $pos $pos+${length}c
                    set posM $pos+1c
                }
            } else {
                while {[set pos [$::jobMonitor::bWindow.f2.text search -count length -regexp -- $pat $posM end]] > 0} {
                    incr ::jobMonitor::patternCount($pat)
                    $::jobMonitor::bWindow.f2.text tag add $::jobMonitor::tagName($pat) $pos $pos+${length}c
                    set posM $pos+1c
                }
            }
        }
    } msg ] } {
         $::jobMonitor::bWindow.f2.text insert end "$msg\n"
    }
    $::jobMonitor::bWindow.f2.text insert end "\n"
}
# addMatchWidgets --
#    For each search patter we add widgets to move to the previsou match,
#    indicate the number of matches or move to the next match.
#
# Arguments:
#    pattern      Regular expresion that was searched for
#    count        Number of times this expression matched.
#    iCol         Color index to assign to the match.
# Result:
#    None
# Side effects:
#    Creates a frame with a couple of arrowbuttons and a label
#
#
proc ::jobMonitor::addMatchWidgets {pattern count iCol} {
    if {$iCol > [llength $::jobMonitor::tagColor]} {
        set fgCol [lindex $::jobMonitor::tagColor end]
    } else {
        set fgCol [lindex $::jobMonitor::tagColor $iCol]
    }
    
    set f    [frame $::jobMonitor::matchFrame.f$pattern]
    set up   [ArrowButton $f.up -dir top -command [namespace code "findPrev {$pattern}"]]
    set lab  [label $f.count -text "$pattern: $count" -background $fgCol]
    set down [ArrowButton $f.down -dir bottom -command [namespace code "findNext {$pattern}"]]
    pack $up $lab $down -side left
    pack $f -side left
}
# findPrev --
#    Move text widget to previous match.
#
# Arguments:
#    pattern      Regular expresion that was searched for
# Result:
#    None
# Side effects:
#    Moves where we see in the text widget
#
#
proc ::jobMonitor::findPrev {pattern} {
    set t $::jobMonitor::bWindow.f2.text
    set prev [$t tag prevrange $::jobMonitor::tagName($pattern) insert]
    if {$prev eq ""} {
        $::jobMonitor::matchFrame.f$pattern.up configure -state disabled
    } else {
        $t mark set insert [lindex $prev 0]
        $t see [lindex $prev 0]
        $::jobMonitor::matchFrame.f$pattern.down configure -state normal
    }
}
# findNext --
#    Move text widget to next match.
#
# Arguments:
#    pattern      Regular expresion that was searched for
# Result:
#    None
# Side effects:
#    Moves where we see in the text widget
#
#
proc ::jobMonitor::findNext {pattern} {
    set t $::jobMonitor::bWindow.f2.text
    set next [$t tag nextrange $::jobMonitor::tagName($pattern) insert]
    if {$next eq ""} {
        $::jobMonitor::matchFrame.f$pattern.down configure -state disabled
    } else {
        $t mark set insert [lindex $next 1]
        $t see [lindex $next 0]
        $::jobMonitor::matchFrame.f$pattern.up configure -state normal
    }
}

# countTypes --
#    Invoked just before showing menu
#
# Arguments: menu
# Result:
# Side effects:
#    Remove buttons which have no jobs in their category.
#    Add buttons as needed for jobs that have moved into a new category.
#    Set selection indicators of all jobs that have a label matching
#    type in their indicator buttons.
#
proc ::jobMonitor::countTypes {m action} {
    for {set i [$m index end]} {$i > 1} {incr i -1} {
        $m delete $i
    }
    set all 0
    foreach job $::jobMonitor::jobList {
        set val [set ::jobMonitor::var$job]
        if {$val eq ""} {
            set val none
        }
        if {[info exists count($val)]} {
            incr count($val)
        } else {
            set count($val) 1
        }
        incr all
    }
    $m entryconfigure 1 -label "all ($all)" -command [namespace code [list $action all]]
    foreach el [lsort [array names count]] {
        $m add command -label "$el ($count($el))" -command [namespace code [list $action $el]]
    }
}

# selectJobs --
#    Invoked via menubutton
#
# Arguments: type
# Result:
# Side effects:
#    Set selection indicators of all jobs that have a label matching
#    type in their indicator buttons.
#    Set anchorJob to "" so range selection with mouse is disabled (until
#    a job selection with the mouse is done.)
#
proc ::jobMonitor::selectJobs {type} {
    set ::jobMonitor::anchorJob ""
    foreach job $::jobMonitor::jobList {
        set val [set ::jobMonitor::var$job]
        if {($type eq "all") || ($type eq $val)} {
            set ::jobMonitor::cbVar$job 1
            lappend ::jobMonitor::actionList $job
        }
    }
}
# deselectJobs --
#    Invoked via menubutton
#
# Arguments: type
# Result:
# Side effects:
#    Unset selection indicators of all jobs that have a label matching
#    type in their indicator buttons.
#    Set anchorJob to "" so range selection with mouse is disabled (until
#    a job selection with the mouse is done.)
#
proc ::jobMonitor::deselectJobs {type} {
    set ::jobMonitor::anchorJob ""
    foreach job $::jobMonitor::jobList {
        set val [set ::jobMonitor::var$job]
        if {($type eq "all") || ($type eq $val)} {
            set ::jobMonitor::cbVar$job 0
            set ::jobMonitor::actionList [lremove -all $::jobMonitor::actionList $job]
        }
    }
}
# toggleSelect --
#    Invoked via menubutton
#
# Arguments: type
# Result:
# Side effects:
#    Toggle the selection indicators of all jobs that have a label matching
#    type in their indicator buttons.
#    Set anchorJob to "" so range selection with mouse is disabled (until
#    a job selection with the mouse is done.)
#
proc ::jobMonitor::toggleSelect {type} {
    set ::jobMonitor::anchorJob ""
    foreach job $::jobMonitor::jobList {
        set val [set ::jobMonitor::var$job]
        if {($type eq "all") || ($type eq $val)} {
            if {[set ::jobMonitor::cbVar$job]} {
                set ::jobMonitor::cbVar$job 0
                set ::jobMonitor::actionList [lremove -all $::jobMonitor::actionList $job]
            } else {
                set ::jobMonitor::cbVar$job 1
                lappend ::jobMonitor::actionList $job
            }
        }
    }
}

# selectJob --
#    Invoked via binding for Button-1 on checkbutton
#
# Arguments: job name
# Result: none
# Side effects: De-select all jobs. If this job was selected leave it
#   un-selected. If this job was un-selected then select it.
#   Make this job the anchor job for range selection.
# Question: do we want to give a visual indication to the anchor button?
#
proc ::jobMonitor::selectJob {jobName} {
    foreach job $::jobMonitor::jobList {
        set ::jobMonitor::cbVar$job 0
    }
    set ::jobMonitor::actionList $jobName
    set ::jobMonitor::anchorJob  $jobName
#    set ::jobMonitor::cbVar$job  1
}
# selectJobRange --
#    Invoked via binding for Shift-Button-1 on checkbutton
#
# Arguments: job name.
# Result: none
# Side effects: Select all jobs between this button and anchor.
#   Add them to selected list. Leave all other job selections alone.
#   If anchorJob = "" this is a noop.
#
proc ::jobMonitor::selectJobRange {jobName} {
    if {$::jobMonitor::anchorJob eq ""} {
        return
    }
    set anchor [lsearch $::jobMonitor::jobList $::jobMonitor::anchorJob]
    set current [lsearch $::jobMonitor::jobList $jobName]
    if {$anchor > $current} {
        set first $current
        incr first
        set last  $anchor
    } else {
        set first $anchor
        set last  $current
        incr last -1
    }
    foreach job [lrange $::jobMonitor::jobList $first $last] {
        if {[lsearch $::jobMonitor::actionList $job] < 0} {
            lappend ::jobMonitor::actionList $job
            set ::jobMonitor::cbVar$job 1
        }
    }
    lappend ::jobMonitor::actionList [lindex $::jobMonitor::jobList $current]
}
# toggleJob --
#    Invoked via binding for Control-Button-1 on checkbutton
#
# Arguments: job name.
# Result: none
# Side effects: Toggle selection status of this job, leave all
#   other jobs alone.
#
proc ::jobMonitor::toggleJob {jobName} {
    if {[lsearch $::jobMonitor::actionList $jobName] < 0} {
        lappend ::jobMonitor::actionList   $jobName
        set ::jobMonitor::cbVar$jobName 0
    } else {
        set ::jobMonitor::actionList  [lremove -all $::jobMonitor::actionList $jobName]
        set ::jobMonitor::cbVar$jobName 1
    }
}

# getFileName --
#    Invoked via tag on the file name
#
# Arguments:
# Result:
#    Entire tag.
# Side effects:
#    None
#
proc ::jobMonitor::getFileName {} {
    set line [lindex [$::jobMonitor::bWindow.f2.text index current] 0]
    set line [lindex [split $line .] 0]
    set fileName [$::jobMonitor::bWindow.f2.text get $line.0 $line.end]
    if {[file extension $fileName] eq ".log"} {
        return [file join $::jobMonitor::logDir $fileName]
    } elseif {[file extension $fileName] eq ".csh"} {
        return [file join $::jobMonitor::scriptDir $fileName]
    }
}
# setSelectedJob --
#    Invoked via tag on the file name
#
# Arguments:
# Result:
# Side effects:
#    set variable selectedJob to job name that current taxt tag is part of.
#
proc ::jobMonitor::setSelectedJob {} {
    set line [lindex [$::jobMonitor::bWindow.f2.text index current] 0]
    set line [lindex [split $line .] 0]
    set fileName [$::jobMonitor::bWindow.f2.text get $line.0 $line.end]
    set ::jobMonitor::selectedJob [string map {sched ""} [file rootname $fileName]]
}
# getDirectory --
#    Invoked via button
#
# Arguments:
#    Variable to store file name.
# Result:
#    Allow user to select file name. Store result in variable.
# Side effects:
#    None
#
proc ::jobMonitor::getScriptDirectory {} {
    set ::jobMonitor::scriptDir [tk_chooseDirectory]
}
proc ::jobMonitor::getLogDirectory {} {
    global ::jobMonitor::logDir
    set ::jobMonitor::logDir [tk_chooseDirectory]
}
# displayFile --
#    Invoked via tag on the file name
#
# Arguments:
#    File name
# Result:
#    Display entire file in a toplevel widget.
#    If toplevel exists already reuse it. Else create a new toplevel.
# Side effects:
#    May create a new toplevel widget.
#
proc ::jobMonitor::displayFile {fileName} {
    setSelectedJob
    set f [open $fileName r]
    set t [read $f]
    close $f

    displayText $t $fileName
}
# displayFile --
#    Invoked via tag on the file name
#
# Arguments:
#    File name
# Result:
#    Display entire file in a toplevel widget.
#    If toplevel exists already reuse it. Else create a new toplevel.
# Side effects:
#    May create a new toplevel widget.
#
proc ::jobMonitor::displayText {text windowTag} {
    if {![winfo exists .monitorFileText]} {
        toplevel .monitorFileText
        text .monitorFileText.t -yscrollcommand {.monitorFileText.s set}
        scrollbar .monitorFileText.s -command {.monitorFileText.t yview}
        grid .monitorFileText.t .monitorFileText.s
        grid .monitorFileText.t -sticky news
        grid .monitorFileText.s -sticky ns
        grid columnconfigure .monitorFileText 0 -weight 1
        grid rowconfigure .monitorFileText 0 -weight 1
    }
    # Delete old tags and create new tags.
    .monitorFileText.t config -state normal
    .monitorFileText.t tag delete [.monitorFileText.t tag names]
    set iTag 0
    foreach pat $::jobMonitor::pattern {
        if {$iTag > [llength $::jobMonitor::tagColor]} {
            set col [lrange $::jobMonitor::tagColor end end]
        } else {
            set col [lrange $::jobMonitor::tagColor $iTag $iTag]
        }
        set ::jobMonitor::tagName($pat) tag$iTag
        .monitorFileText.t tag configure tag$iTag -underline 1 -background $col
        incr iTag
    }
    wm title .monitorFileText $windowTag
    wm deiconify .monitorFileText

    #
    # Put text into widget, then search through for each of the matches.
    .monitorFileText.t delete 0.0 end
    .monitorFileText.t insert end $text

    set posE 1.0
    foreach pat $::jobMonitor::pattern {
        set posM $posE
        if { $::jobMonitor::ignoreCase } {
            while {[set pos [.monitorFileText.t search -nocase -count length \
                             -regexp -- $pat $posM end]] > 0} {
                .monitorFileText.t tag add $::jobMonitor::tagName($pat) $pos $pos+${length}c
                set posM $pos+1c
            }
        } else {
            while {[set pos [.monitorFileText.t search -count length \
                             -regexp -- $pat $posM end]] > 0} {
                .monitorFileText.t tag add $::jobMonitor::tagName($pat) $pos $pos+${length}c
                set posM $pos+1c
            }
        }
    }
    .monitorFileText.t config -state disabled
    raise .monitorFileText
}
# popupMenu --
#    Invoked via tag on the file name
#
# Arguments:
#    File name, x, y positions
# Result:
#    None
# Side effects:
#    Displays an option menu allowing selection of editing csh, log or filelist.
#    If one of these is selected appropriate file is opened in an editor.
#
proc ::jobMonitor::popupMenu {x y} {
    setSelectedJob
    tk_popup $::jobMonitor::viewMenu $x $y
}

# editType --
#    Invoked via a popup menu
#
# Arguments:
#    Type of file to edit
# Result:
# Side effects:
#    Invoke an editor
#
proc ::jobMonitor::editType {type} {
    switch $type {
        "script"   {set fileName [file join $::jobMonitor::scriptDir sched$::jobMonitor::selectedJob.csh]}
        "log"      {set fileName [file join $::jobMonitor::logDir         $::jobMonitor::selectedJob.log]}
        "filelist" {set fileName [file join $::jobMonitor::scriptDir sched$::jobMonitor::selectedJob.list]}
        default         {return}
    }
    editFile $fileName
}
# checkPopup --
#    postcommand for viewMenu
#
# Arguments:
# Result:
# Side effects:
#    If status button claims job is in "RUN" or "SSUS" mode we include a peek option.
#    If there is no status (typical after job finishes in SGE) or indicates
#    job is done in SGE (DONE?) we include qacct information option.
#
proc ::jobMonitor::checkPopup {} {
    set job $::jobMonitor::selectedJob
    set status [set ::jobMonitor::var$job]
    if {($status eq "RUN") || ($status eq "SSUS")} {
        if {[catch {$::jobMonitor::viewMenu index bpeek} err]} {
            $::jobMonitor::viewMenu add command -label "bpeek" -command [namespace code peekAtJob]
        }
    } else {
        catch {$::jobMonitor::viewMenu delete "bpeek"}
    }
    set f [file join $::jobMonitor::logDir $job.log]
    if {[file exists $f] &&
       (($status eq "") || ($status eq "DONE"))} {
        if {[catch {$::jobMonitor::viewMenu index qacct} err]} {
            $::jobMonitor::viewMenu add command -label "qacct" -command [namespace code qacctJob]
        }
    } else {
        catch {$::jobMonitor::viewMenu delete "qacct"}
    }
}

# peekAtJob --
#    Invoked via a popup menu
#
# Arguments:
# Result:
# Side effects:
#    Find job id, invoke bpeek, put output into tk text widget.
#
proc ::jobMonitor::peekAtJob {} {
    set jobID -1
    set job sched$::jobMonitor::selectedJob.csh
    set jobName [getJobName [file join $::jobMonitor::scriptDir $job]]
    catch {eval exec bjobs -waJ $jobName} bResult
    set lines [split $bResult \n]
    foreach def [lindex $lines 0] val [lindex $lines 1] {
        if {$def eq "JOBID"} {
            set jobID $val
        }
    }
    if {$jobID < 0} {
        set text "Could not find JOBID for $::jobMonitor::selectedJob. \
                  Perhaps job is not currently running?"
    } else {
        catch {[eval exec bpeek $jobID]} text
    }
    displayText $text "bpeek $job: $jobID"
}

# qacctJob --
#    Invoked via a popup menu
#
# Arguments:
# Result:
# Side effects:
#    Look for accounting file and get relevant information for tthe selected job.
#    Display it in a popup window.
#
proc ::jobMonitor::qacctJob {} {
    global env

    set base $env(SGE_ROOT)/default/common/accounting.6.0u4
    set job $::jobMonitor::selectedJob
    set jobName sched$::jobMonitor::selectedJob.csh
    set f [file join $::jobMonitor::logDir $job.log]
    set mTime [file mtime $f]
    # mTime is last modified time. Accounting info may be in file corresponding
    # to a day before or after. Check both those days as well.
    set lTime $mTime
    lappend lTime [clock scan yesterday -base $mTime]
    lappend lTime [clock scan tomorrow -base $mTime]

    foreach t $lTime {
        set aTime [join [clock format $t -format "%Y %m %d"] .]
        # If accounting file not found then try no file (i.e. job ran today.)
        if {[catch {glob $base.$aTime*} aFile]} {
            if {![catch {exec qacct -o $env(USER) -j $jobName} text]} {
                displayText $text "qacct information for $job"
                return
            }
        } else {
            if {![catch {exec qacct -o $env(USER) -j $jobName -f $aFile} text]} {
                displayText $text "qacct information for $job"
                return
            }
        }
    }
    set text "Did not find information for job\n \
               $jobName.\n \
              We looked in accounting files for [clock format $t -format {%Y %m %d}] \
              as well as the days before and after. If you want to search on your \
              own you can try commands like\n \
              qacct -o $env(USER) -j $jobName -f $base.YYYY.mm.dd.X\n \
              where x is hours and minutes and you have to figure those out.\n \
              Good luck."
    displayText $text "Did not find qacct information for $job"
}

# submitSelected --
#    Invoked via menu
#
# Arguments:
# Result:
# Side effects:
#    For each job in actionList we extract and invoke bsub command or
#    the qsub command
#
proc ::jobMonitor::submitSelected {} {
    global env

    foreach job $::jobMonitor::actionList {
        set fName [file join $::jobMonitor::scriptDir sched$job.csh]
        set f [open $fName]
        set runCmd ""
        while { [gets $f line] >= 0 } {
            # Look for a line starting with a comment and having bsub
            # somewhere before -q.
            # No guarrantee this is the right command, but...
            if {[regexp {^#.*bsub.*-q.*} $line]} {
                set runCmd [string map {
                        "# "   ""
                        "["    "\\["
                        "]"    "\\]"
                        "("    "\\("
                        ")"    "\\)"
                         } $line]
                break
            }
            # If script is submitted to SGE the command is qsub.
            # Expect to have a -o switch. (Is this always true?)
            # No guarrantee this is the right command, but...
            if {[regexp {^#.*qsub.*-o.*} $line]} {
                set runCmd [string map {
                        "# "   ""
                        "["    "\\["
                        "]"    "\\]"
                        "("    "\\("
                        ")"    "\\)"
                         } $line]
                break
            }
            # If script is submitted to condor wwe actually have a simple submission command.
            # (Comment for previous section claimed that was condor, but I must have mis-understood.
            #  In any case it works at pdsf.)
            if {[regexp {^#.*condor_submit} $line]} {
                set runCmd [string map {"# "   ""} $line]
                break
            }
        }
        if {[string length $runCmd] == 0} {
            puts "Failed to find job submission command in file $fName!!!!"
        } else {
            # At rcf the scheduler splits the job submission into cd and then bsub
            # commands (as opposed to putting the path into the script name.)
            set start 0
            while {[set end [string first ";" $runCmd $start]] > 0} {
                set cmd [string range $runCmd $start $end]
                if {[catch {eval $cmd}]} {
                    eval exec $cmd
                }
                set start [expr $end+1]
            }
            eval exec [string range $runCmd $start end]
            set ::jobMonitor::var$job "SUBM"
            $::jobMonitor::bWindow.f2.text.stat$job configure -fg orange
        }
        close $f
    }
    if {[info exists env(DOMAINNAME)] && ($env(DOMAINNAME) eq "nersc.gov")} {
        catch {exec use_stardata.pl}
    }
}

# killSelected --
#    Invoked via menu
#
# Arguments:
# Result:
# Side effects:
#    Kill each job in actionList.
#
proc ::jobMonitor::killSelected {} {
    global env

    if {![catch {eval exec qstat_long -u $env(USER)} qstatOutput]} {
        set qstatList [split $qstatOutput \n]
    } else {
        set qstatList [list]
    }
    if {![catch {exec condor_q -submitter $env(USER) -format " %i " clusterid -format " %s " procId -format " %s " CMD} formatted]} {
        set condClustList $formatted
    } else {
        set condClustList [list]
    }
    foreach job $::jobMonitor::actionList {
        set jobName [getJobName [file join $::jobMonitor::scriptDir sched$job.csh]]
#        if {![catch {eval exec bjobs -waJ $jobName} bResult]} {
#            set lines [split $bResult \n]
#            foreach def [lindex $lines 0] val [lindex $lines 1] {
#                if {$def eq "JOBID"} {
#                    catch {eval exec bkill $val}
#                }
#            }
#        } else {
            set jName [string map {condor csh} $jobName]
            foreach {clusterId procId cmd} $condClustList {
                set f [file tail $cmd]
                if {$f eq $jName} {
                    #>>>>> condor_rm only works on node job was submitted from!!!!!
                    # Seems that condClustList may include jobs that are no longer running.
                    catch {exec condor_rm $clusterId.$procId}
                }
            }
            set f [file tail $jobName]
            foreach qj $qstatList {
                if {[lsearch $qj $f] >= 0} {
                    set jobID [lindex $qj 0]
                    catch {eval exec qdel $jobID}
                }
            }
#        }
    }
    updateStatusIndicators
}

# clearErrors --
#    Invoked via menu
#
# Arguments:
# Result:
# Side effects:
#    Clear error for each job in actionList.
#    Only know how to do this for sge. Not sure if lsf has the same issue.
#
proc ::jobMonitor::clearErrors {} {
    global env

    if {![catch {eval exec qstat_long -u $env(USER)} qstatOutput]} {
        set qstatList [split $qstatOutput \n]
    } else {
        return
    }
    foreach job $::jobMonitor::actionList {
        set jobName [getJobName [file join $::jobMonitor::scriptDir sched$job.csh]]
        set f [file tail $jobName]
        foreach qj $qstatList {
            if {[lsearch $qj $f] >= 0} {
                set jobID [lindex $qj 0]
                catch {eval exec qmod -c $jobID}
            }
        }
    }
    updateStatusIndicators
}

# updateStatusIndicators --
#    Invoked via menu
#
# Arguments:
# Result:
# Side effects:
#    Check all jobs to see if they are in the batch system.
#    If so extract the status and display that in the status box.
#
proc ::jobMonitor::updateStatusIndicators {{job ""}} {
    global env

    if {$job ne ""} {
        set jobs $job
    } else {
        set jobs $::jobMonitor::jobList
    }
    if {![catch {eval exec qstat_long -u $env(USER)} qstatOutput]} {
        set qstatList [split $qstatOutput \n]
    } else {
        set qstatList [list]
    }
    if {![catch {exec condor_q -submitter $env(USER) -format " %i " clusterid -format " %s " procId -format " %s " CMD -format " %d " JobStatus} formatted]} {
        set condClustList $formatted
    } else {
        set condClustList [list]
    }
    if {![catch {exec condor_q -submitter $env(USER)} unFormatted]} {
        set condJobList $unFormatted
    } else {
        set condJobList [list]
    }

    foreach job $jobs {
        set jobName [getJobName [file join $::jobMonitor::scriptDir sched$job.csh]]
#        if {![catch {eval exec bjobs -waJ $jobName} bResult]} {
#            set lines [split $bResult \n]
#            set ::jobMonitor::var$job UNKN
#            # For some reason I had been taking the last two lines and parsing them.
#            # When the same job has been re-run it appears multiple times, but it
#            # appears the labelling information is always on the first line and
#            # the job info is sorted by most recent first. May be some reason I did it
#            # the wrong way though (like it used to be the right way?? Or I screwed up??)
#            foreach def [lindex $lines 0] val [lindex $lines 1] {
#                if {$def eq "STAT"} {
#                    set ::jobMonitor::var$job $val
#                    colorStatusIndicator $::jobMonitor::bWindow.f2.text.stat$job $val
#                    break
#                }
#            }
#        } else {
            # Find job with cmd of jobName. Remember it's cluster.procid.
            # Use that to parse result and get status.
            # Would be so much easier if I could figure out how to specify status as an attribute
            # in the ClasAd system.
            set ::jobMonitor::var$job ""
            $::jobMonitor::bWindow.f2.text.stat$job configure -fg black
            set id ""
            # Replace .condor with .csh in jobName
            set jName [string map {condor csh} $jobName]
            foreach {clusterId procId cmd jobStat} $condClustList {
                if {[file tail $cmd] eq $jName} {
                    if {$jobStat == 1} {
                        set ST I
                    } elseif {$jobStat == 2} {
                        set ST R
                    } else {
                        set ST ""
                    }
                    set ::jobMonitor::var$job $ST
                    colorStatusIndicator $::jobMonitor::bWindow.f2.text.stat$job $ST
                }
            }
            set fileName [file tail $jobName]
            foreach ql $qstatList {
                set ind [lsearch $ql $fileName]
                if {$ind >= 0} {
                    incr ind 2
                    set val [lindex $ql $ind]
                    set ::jobMonitor::var$job $val
                    colorStatusIndicator $::jobMonitor::bWindow.f2.text.stat$job $val
                }
            }
#        }
    }
}
# colorStatusIndicator --
#    Invoked from updateStatusIndicators and searchPattern
#    At the moment there is no overlap between indicator text labels as
#    far as LSF and SGE go. If a conflict should arise plan is to use a
#    namespace variable, setting it in some routine where we had to make
#    the distinction. Not sure if we should allow every job to be 
#    handled by a different batch system. This tool should make the least
#    retrictions possible.
#
# Arguments:
# Result:
# Side effects:
#    Depending on button text we color the button for easier
#    identification of state.
#
proc ::jobMonitor::colorStatusIndicator {b v} {
    switch $v {
        UNKN {$b configure -fg yellow}
        SUBM {$b configure -fg orange}
        PEND {$b configure -fg blue}
        RUN  {$b configure -fg red}
        DONE {$b configure -fg green}
        SUSP {$b configure -fg yellow}
        Eqw  {$b configure -fg yellow}
        t    {$b configure -fg orange}
        qw   {$b configure -fg blue}
        r    {$b configure -fg red}
        I    {$b configure -fg blue}
        R    {$b configure -fg red}
        H    {$b configure -fg yellow}
        default {$b configure -fg gray}
    }
}

# getJobName --
#    
#
# Arguments: script file given to bsub.
# Result: 
#    Return full name of script file given to bsub.
# Side effects:
#    None
#
proc ::jobMonitor::getJobName {scriptFile} {
    set f [open $scriptFile]
    set runCmd ""
    while { [gets $f line] >= 0 } {
        # Look for a line starting with a comment and having bsub
        # somewhere before -q.
        # No guarrantee this is the right command, but...
        if {[regexp {^#.*bsub.*-q.*} $line]} {
            set runCmd [string map {
                    "# "   ""
                    "["    "\\["
                    "]"    "\\]"
                    "("    "\\("
                    ")"    "\\)"
                     } $line]
            break
        }
        if {[regexp {^#.*qsub.*-o.*} $line]} {
            set runCmd [string map {
                    "# "   ""
                    "["    "\\["
                    "]"    "\\]"
                    "("    "\\("
                    ")"    "\\)"
                     } $line]
            break
        }
        if {[regexp {^#.*condor_submit} $line]} {
            set runCmd [string map {"# "   ""} $line]
            break
        }
    }
    close $f
    if {[llength $runCmd] == 0} {
        error "Failed to find job submission command in file $fName!!!!"
    } else {
        return [lindex $runCmd end]
    }
}

# editFile --
#
# Arguments:
#    name of file to edit
# Result:
#    open a new editor window
# Side effects:
#    None
#
proc ::jobMonitor::editFile {fn} {
    global env

    if {[info exists env(EDITOR)]} {
    } elseif {![catch {exec which nedit}]} {
        set env(EDITOR) nedit
    } elseif {![catch {exec which emacs}]} {
        set env(EDITOR) emacs
    } else {
        tk_messageBox -icon error -type ok \
                -message "EDITOR environment variable not set. \
                          Could not find nedit or emacs in default path. \
                          Please set the editor before trying to edit."
        return
    }
    eval exec $env(EDITOR) $fn &
}
# chooseEditor --
#
# Arguments:
#    none
# Result:
#    open window requesting an editor
# Side effects:
#    Set env(EDITOR) to new value
#
proc ::jobMonitor::chooseEditor {} {
   global env

   set w [toplevel .editor]
   wm resizable $w 0 0
   wm title $w "Select Editor"
   label  $w.l -text "Enter the editor you want to use."
   entry  $w.e -textvar env(EDITOR)
   bind $w.e <Return> [namespace code "set done 1"]
   button $w.ok     -text OK     -command [namespace code "set done 1"]
   grid $w.l  -sticky news
   grid $w.e  -sticky news
   grid $w.ok 
   vwait done
   destroy $w
}
# chooseEditor --
#
# Arguments:
#    none
# Result:
#    open window requesting an editor
# Side effects:
#    Set env(EDITOR) to new value
#
proc ::jobMonitor::setFileType {} {
   set w [toplevel .logFileType]
   wm resizable $w 0 0
   wm title $w "Set log file type"
   label  $w.l1 -text "Enter extension type for your log files."
   label  $w.l2 -text "Normal type is out."
   entry  $w.e -textvar ::jobMonitor::LOGTYPE
   bind $w.e <Return> [namespace code "set done 1"]
   button $w.ok     -text OK     -command [namespace code "set done 1"]
   grid $w.l1 -sticky news
   grid $w.l2 -sticky news
   grid $w.e  -sticky news
   grid $w.ok
   vwait done
   destroy $w
}



# displayHelp --
#    Invoked via menu
#
# Arguments:
#    Widget to display help in. If empty create a toplevel named .help
#    containing a scrolled text widget.
# Result:
#    Display help text in a toplevel widget.
#    If toplevel exists already reuse it. Else create a new toplevel.
# Side effects:
#    May create a new toplevel widget.
#
proc ::jobMonitor::displayHelp {w} {
    if {$w eq ""} {
        catch {destroy .help}
        toplevel .help
        wm transient .help $::jobMonitor::bWindow
        wm title .help "jobMonitor Help"
        if {[regexp {(\+[0-9]+)(\+[0-9]+)$} [wm geom $::jobMonitor::bWindow] => wx wy]} {
            wm geom .help "+[expr {$wx+35}]+[expr {$wy+35}]"
        }
        set w .help.t
        text $w -wrap word -width 70 -height 28 -pady 10 \
                -yscrollcommand {.help.s set}
        scrollbar .help.s -command {.help.t yview}
        grid $w .help.s
        grid $w -sticky news
        grid .help.s -sticky ns
        button .help.quit -text Dismiss -command {catch {destroy .help}}
        grid .help.quit -
        grid columnconfigure .help 0 -weight 1
        grid rowconfigure .help 0 -weight 1
    }

    $w tag config header -justify center -font bold -foreground red
    $w tag config header2  -justify center -font bold
    set margin [font measure [$w cget -font] " o "]
    set margin2 [font measure [$w cget -font] " o - "]
    $w tag config bullet -lmargin2 $margin
    $w tag config bullet -font "[font actual [$w cget -font]] -weight bold"
    $w tag config n -lmargin1 $margin -lmargin2 $margin2

    $w insert end "jobMonitor" header "\nby Duncan Prindle\n\n" header2

    $w insert end " o What does this do?\n" bullet
    $w insert end "- The purpose of this program is to monitor the running " n
    $w insert end "and then browse the output of batch jobs. " n
    $w insert end "You can kill jobs you don't like, make minor changes " n
    $w insert end "in filelists or other control files, and resubmit jobs." n
    $w insert end "jobMonitor knows about LSF, SGE and condor so you don't have to.\n" n
    $w insert end "- The monitored jobs are those described by csh files " n
    $w insert end "in the script directory and/or the log files in " n
    $w insert end "the logs directory. These are not displayed in " n
    $w insert end "this browser until a Search is made. The search " n
    $w insert end "can be an empty string.\n\n" n

    $w insert end "Note that on starting the job monitor using a shell " n
    $w insert end "command line you can give a directory containing scripts and logs " n
    $w insert end "subdirectories on the command line.\n\n\n" n

    $w insert end "jobMonitor Window\n\n" header

    $w insert end " o script directory:\n" bullet
    $w insert end "- This is the directory containing the *.csh files " n
    $w insert end "created and submitted by the scheduler.\n\n" n

    $w insert end " o logs directory:\n" bullet
    $w insert end "- This is the directory containing the log files " n
    $w insert end "produced by the batch jobs. By default only files named " n
    $w insert end "*.log are examined. The extension can be changed with the " n
    $w insert end "'Edit->log file type' menu.\n\n" n

    $w insert end " o Regular expression:\n" bullet
    $w insert end "- This is a list of regular expressions. " n
    $w insert end "In its simplest form a regular expression is simply " n
    $w insert end "a text string, such as 'CPU' or 'segmentation'.\n" n
    $w insert end "- If there is a log file in the logs directory it will " n
    $w insert end "be searched. Otherwise the csh file will be searched. " n
    $w insert end "To include a space enclose the string in \"\" or \{\}. " n
    $w insert end "Every line of every file is matched against " n
    $w insert end "every regular expression in your list, so if you have big " n
    $w insert end "log files this may take a while.\n\n" n

    $w insert end " o Search:\n" bullet
    $w insert end "- Searching through the log files is done " n
    $w insert end "every time you push the Search button (or hit Enter " n
    $w insert end "when the search entry widget has focus.)\n\n" n

    $w insert end " o Results area\n" bullet
    $w insert end "- This contains the file names of all files that have " n
    $w insert end "been searched. After every file name are every line in " n
    $w insert end "the file that contains a match to any of your " n
    $w insert end "regular expressions. The matches are color highlighted. " n
    $w insert end "The first five regular expressions get different colors.\n" n
    $w insert end "- A checkbox on the line with the file will select/deselect " n
    $w insert end "that file. The state of the checkbox can be modified by " n
    $w insert end "menu items (see Menus below) and by mouse clicks on the " n
    $w insert end "indicators (see Selections below).\n" n
    $w insert end "- A button to the right of the checkbox will query the " n
    $w insert end "batch system for the current job status and display the " n
    $w insert end "status of that job. The actual status code depends on " n
    $w insert end "the batch system. The status for all jobs can be updated " n
    $w insert end "via a menu item (see Menus below.) \n" n
    $w insert end "- Left-Clicking on the filename will bring up a 'File " n
    $w insert end "Viewing Window' displaying the contents of that file.\n" n
    $w insert end "- Right clicking on the file name invokes a popup menu " n
    $w insert end "(which is described below.)\n\n" n

    $w insert end " o Status area:\n" bullet
    $w insert end "- The bottom of the main window contains a summary of searches. " n
    $w insert end "For each search item there is the string, the number of matches and " n
    $w insert end "buttons to bring the next/previous matching job into the visible " n
    $w insert end "region of the window. " n
    $w insert end "The search always starts at the location of the previous matching " n
    $w insert end "batch job (on the first line if no button has been clicked yet.)\n\n" n


    $w insert end "File Viewing Window\n\n" header

    $w insert end " o Log/csh file contents\n" bullet
    $w insert end "- The text of the selected file is displayed in the main " n
    $w insert end "area. The matches are colored as before. " n
    $w insert end "An empirical observation; when a job is run and the " n
    $w insert end "output file exists the new output " n
    $w insert end "is appended to the existing file. This is quite handy when " n
    $w insert end "it takes more than one attempt before the job succeeds. " n
    $w insert end "(I believe this behaiviour depends on the batch system " n
    $w insert end "so it may change in the future.)\n\n" n


    $w insert end "Menus\n\n" header

    $w insert end " o File\n" bullet
    $w insert end "- Select jobs: \n" n
    $w insert end "- Deselect jobs: \n" n
    $w insert end "- Toggle select: " n
    $w insert end "  These manipulate the selection status according to " n
    $w insert end "the values in the status indicator buttons. There is also an 'all' option. " n
    $w insert end "After each status option there is a number indicating how many " n
    $w insert end "batch jobs are in this category. \n" n
    $w insert end "- Submit selected jobs: " n
    $w insert end "  Scans the csh script for a jobs submission command and " n
    $w insert end "invokes it. For each line we look for an lsf command then " n
    $w insert end "an SGE command, then a condor command. As soon as we find one we use it.\n" n
    $w insert end "- Kill selected jobs: " n
    $w insert end " Hopefully only kills jobs that have been selected\n" n
    $w insert end "- Clear selected job errors: " n
    $w insert end " When an SGE job encounters certain types of errors " n
    $w insert end "(can't create ouput file for example (I think)) " n
    $w insert end "it suspends the job and won't restart it until " n
    $w insert end "the error is cleared.\n" n
    $w insert end "- Update status: " n
    $w insert end " Asks the batch system for the current job status and " n
    $w insert end "displays it in the button on the jobname line. When the " n
    $w insert end "job is done SGE returns no information on it (or more " n
    $w insert end "precisely, I haven't figured out a good way to get " n
    $w insert end "the information) while lsf will tell us the job is done " n
    $w insert end "(at least for a little while.)\n" n
    $w insert end "- Exit: I hope this is obvious. \n\n" n

    $w insert end " o Edit\n" bullet
    $w insert end "- Ignore case: Respect/Ignore case in regular expression match.\n" n
    $w insert end "- Editor: Set editor to use when editing csh, list files\n" n
    $w insert end "- log file type: File type to search for regular expression matches.\n\n" n

    $w insert end " o Help\n" bullet
    $w insert end "- Help: Bring up this text in its own window. \n\n" n

    $w insert end "Popup Menus\n\n" header

    $w insert end " o edit csh\n" bullet
    $w insert end "Edit csh script using selected editor.\n" n
    $w insert end " o edit log\n" bullet
    $w insert end "Edit log file using selected editor.\n" n
    $w insert end " o edit filelist\n" bullet
    $w insert end "Edit filelist file using selected editor. Sometimes " n
    $w insert end "when a single file causes the job to crash. " n
    $w insert end "it can be convenient to remove that data file " n
    $w insert end "and rerun the job.\n" n
    $w insert end " o bpeek\n" bullet
    $w insert end "When the job is currently running under lsf. \n" n
    $w insert end "This displays results returned from bpeek in the file viewing window." n
    $w insert end "(Note that with SGE you can look at the partial " n
    $w insert end "log file directly.)\n" n
    $w insert end " o qacct\n" bullet
    $w insert end "When the job ran under SGE and is no longer running. \n" n
    $w insert end "This displays the accounting information in the file viewing window.\n\n" n

    $w insert end "Selections\n\n" header

    $w insert end " o Button 1 mouse click\n" bullet
    $w insert end "(I.e. left button).\n" n
    $w insert end "- This will de-select all other jobs, select this job and set " n
    $w insert end "the selection anchor to this job. \n" n
    $w insert end " o Shift-Button 1 mouse click\n" bullet
    $w insert end "- This selects all jobs between the selection anchor and this job. " n
    $w insert end "If there is no selection anchor this will do nothing. " n
    $w insert end "The selection anchor is not changed.\n" n
    $w insert end " o Control-Button 1 mouse click\n" bullet
    $w insert end "- This will toggle the selection state of this job " n
    $w insert end "leaving the selection state of all other jobs and the " n
    $w insert end "selection anchor unchanged.\n\n" n

    $w insert end "Note that using any of the menus to modify job " n
    $w insert end "selection status will cause the selection anchor to become undefined.\n\n" n

    $w config -state disabled
}
################################################################################
# exit deletes the jobMonitor main window and destroys the namespace.
################################################################################
proc ::jobMonitor::exit {} {
    if {$::jobMonitor::IOwnProcess} {
        ::exit
    } else {
        destroy $::jobMonitor::bWindow
        namespace delete ::jobMonitor::
    }
}
