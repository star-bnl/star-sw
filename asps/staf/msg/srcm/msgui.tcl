global msgHelp
global env
global allPidsList
global LoadFrom
global ProcessID
global StoreTo
global ListInactives
global AutoUpdateSummary
global AutoUpdateSummaryCPU
global AutoUpdateActive
global AutoUpdatePeriodSecs
global Prefix
global Counts
global CountLimit
global AbortLimit
global AlarmLevel
global Active
global Counting
global Alarming
global Line
global PageLength
global NodeName
global Shmid

proc initMsgVariables {} {
	upvar #0 allPidsList allPidsList
	upvar #0 LoadFrom LoadFrom
	upvar #0 ProcessID ProcessID
	upvar #0 Prefix Prefix
	upvar #0 Counts Counts
	upvar #0 CountLimit CountLimit
	upvar #0 AbortLimit AbortLimit
	upvar #0 AlarmLevel AlarmLevel
	upvar #0 Active Active
	upvar #0 Counting Counting
	upvar #0 Alarming Alarming
	upvar #0 StoreTo StoreTo
	upvar #0 ListInactives ListInactives
	upvar #0 AutoUpdateActive AutoUpdateActive
	upvar #0 AutoUpdatePeriodSecs AutoUpdatePeriodSecs
	upvar #0 AutoUpdateSummary AutoUpdateSummary
	upvar #0 AutoUpdateSummaryCPU AutoUpdateSummaryCPU
	upvar #0 NodeName NodeName
	upvar #0 Shmid Shmid
	set allPidsList ""
	set LoadFrom default.msg
	set ProcessID 0
	set Prefix ""
	set Counts 0
	set CountLimit 0
	set AbortLimit 0
	set AlarmLevel 0
	set Active 0
	set Counting 0
	set Alarming 0
	set ListInactives 0
	set AutoUpdateActive 0
	set AutoUpdatePeriodSecs 10
	set AutoUpdateSummary 0
	set AutoUpdateSummaryCPU 0
	set StoreTo default.msg
	set NodeName "No Node Name"
	set Shmid    "-1"
}




proc MsgDo { root command } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 LoadFrom LoadFrom
	upvar #0 ProcessID ProcessID


	if { [string length $ProcessID] > 0 } {
	  catch { eval exec msgControl -m -p$ProcessID $command |& tee msg.err > msg.tmp } Error
	} else {
	  catch { eval exec msgControl -m -f$LoadFrom  $command |& tee msg.err > msg.tmp } Error
	}

	if { $Error != "" } {
	  set header "\n$Error"
	  $base.message#1 configure -text $header
	  if [file exists msg.err] {
	    listFill $base [exec -keepnewline cat msg.err]
	  }
	  return -1
	} else {
	  return 0
	}
}




proc MsgExtractNext {} {
	upvar #0 Line Line

	set Line [string trimleft $Line]
	set wordend [expr [string first " " $Line] - 1 ]
	if { $wordend < 0 } { set wordend [expr [string length $Line] - 1 ]  }
	set word [string range $Line 0 $wordend]
	set Line [string range $Line [expr $wordend + 1] end]
	return $word
}




proc MsgLoadFrom {root} {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 StoreTo StoreTo
	upvar #0 LoadFrom LoadFrom
	upvar #0 ProcessID ProcessID

	if { [string length $LoadFrom] <= 0 } {
	  set Error "You need to specify an Msg state file (path) to load the process from."
	  catch { rm msg.err }
	} elseif { [string length $ProcessID] > 0 } {
	  catch { exec msgControl -m -p$ProcessID load $LoadFrom |& tee msg.err > msg.tmp } Error
	} else {
	  set Error "You need to specify a process ID to be loaded from the Msg state file."
	  catch { rm msg.err } 
	}

	if { $Error != "" } {
	  set header "\n$Error"
	  $base.message#1 configure -text $header
	  if [file exists msg.err] {
	    listFill $base [exec -keepnewline cat msg.err]
	  }
	  update
	}



}




proc MsgNodeNameSet { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 NodeName NodeName
	MsgDo . "setnode $NodeName"
}




proc MsgPrefixDelete { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 LoadFrom LoadFrom
	upvar #0 ProcessID ProcessID
	upvar #0 Prefix Prefix
	upvar #0 Counts Counts
	upvar #0 CountLimit CountLimit
	upvar #0 AbortLimit AbortLimit
	upvar #0 AlarmLevel AlarmLevel
	upvar #0 Active Active
	upvar #0 Counting Counting
	upvar #0 Alarming Alarming
	upvar #0 Line Line

#	Clean up the specificed prefix:
	set Line $Prefix
	set Prefix [MsgExtractNext]

	if { [string length "$Prefix"] <= 0 } return

	MsgDo . "delete $Prefix"
	MsgSummary .
}




proc MsgPrefixGet { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 LoadFrom LoadFrom
	upvar #0 ProcessID ProcessID
	upvar #0 Prefix Prefix
	upvar #0 Counts Counts
	upvar #0 CountLimit CountLimit
	upvar #0 AbortLimit AbortLimit
	upvar #0 AlarmLevel AlarmLevel
	upvar #0 Active Active
	upvar #0 Counting Counting
	upvar #0 Alarming Alarming
	upvar #0 Line Line

#	Clean up the specificed prefix:
	set Line $Prefix
	set Prefix [MsgExtractNext]

	if { [string length "$Prefix"] <= 0 } return

	if { ![ MsgDo . "get $Prefix" ] } {
	  set Line [ exec cat msg.tmp ]
	}

	set Error ""
	set word [MsgExtractNext]
	if { $Prefix != $word } { set Error "MsgPrefixGet-F2 (msgui.tcl) prefix ($Prefix) unexpected.  Expected ($word)" }

	if { $Error != "" } {
	  set header "\n$Error"
	  $base.message#1 configure -text $header
	  if [file exists msg.err] {
	    listFill $base [exec -keepnewline cat msg.err]
	  }
	} else {

#  Extract the counts -- use [string wordend ...]:
	  set Counts [MsgExtractNext]

#  Extract the count limit:
	  set CountLimit [MsgExtractNext]

#  Extract the alarm level:
	  set AlarmLevel [MsgExtractNext]

#  Extract the abort limit:
	  set AbortLimit [MsgExtractNext]


#  Extract the active flag:
	  set Active 1
	  set word [MsgExtractNext]
	  if { [string match *F* $word] || [string match *f* $word] || [string match 0 $word] } { set Active 0;}

#  Extract the counting flag:
	  set Counting 1
	  set word [MsgExtractNext]
	  if { [string match *F* $word] || [string match *f* $word] || [string match 0 $word] } { set Counting 0;}

#  Extract the alarming flag:
	  set Alarming 1
	  set word [MsgExtractNext]
	  if { [string match *F* $word] || [string match *f* $word] || [string match 0 $word] } { set Alarming 0;}

#  Extract and display the ASCII state:
	  set word [MsgExtractNext]
	  $base.message#2 configure -text $word

	}
	update

}




proc MsgPrefixSet { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 LoadFrom LoadFrom
	upvar #0 ProcessID ProcessID
	upvar #0 Prefix Prefix
	upvar #0 Counts Counts
	upvar #0 CountLimit CountLimit
	upvar #0 AbortLimit AbortLimit
	upvar #0 AlarmLevel AlarmLevel
	upvar #0 Active Active
	upvar #0 Counting Counting
	upvar #0 Alarming Alarming
	upvar #0 Line Line

#	Clean up the specificed prefix:
	set Line $Prefix
	set Prefix [MsgExtractNext]

	if { [string length "$Prefix"] <= 0 } return

	if { ![ MsgDo . "set $Prefix=$CountLimit $AlarmLevel $AbortLimit $Active $Counting $Alarming" ] } {
	  set message [ exec cat msg.tmp ]
	  if { [string length "$message"] > 0 } {
	    set header "\n$message"
	    $base.message#1 configure -text $header
	    listFill $base ""
	    update
	  } else {
	    MsgPrefixGet .
	    MsgSummary .
	  }
	}

}




proc MsgHelp {root args} {

	upvar env env
	upvar msgHelp msgHelp

	if {$root == "."} {
		set base ""
	} else {
		set base $root
	}
#	set fullPath $env(HOME)msg.help
	set fullPath ${msgHelp}msg.help
	listFill $base [exec cat $fullPath]

}




proc MsgQuit {root args} {

	if {$root == "."} {
		set base ""
	} else {
		set base $root
	}

	exit

}




proc MsgRemoveShmid { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 ProcessID ProcessID
	if { $ProcessID > 0 } {
	  exec rmid $ProcessID >& msg.err
	  catch { rm msg.err } 
	}
}




proc MsgSelectProcess { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 ProcessID ProcessID
}




proc MsgSelectPrefix { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 Prefix Prefix
	upvar #0 Line Line
	set selection [$base.listbox#1 curselection  ]
	set Line      [$base.listbox#1 get $selection]
	set Prefix    [MsgExtractNext]

	MsgPrefixGet $base

}




proc MsgSetLines { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 PageLength PageLength
	MsgDo . "lines $PageLength"

}




proc MsgStoreTo {root} {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 StoreTo StoreTo
	upvar #0 LoadFrom LoadFrom
	upvar #0 ProcessID ProcessID

	if { [string length $StoreTo] <= 0 } {
	  set Error "You need to specify an Msg state file (path) in which to store the process's Msg state."
	  catch { rm msg.err } 
	} elseif { [string length $ProcessID] > 0 } {
	  catch { exec msgControl -m -p$ProcessID store $StoreTo |& tee msg.err > msg.tmp } Error
	} else {
	  set Error "You need to specify a process ID to have its msg state stored in the state file."
	  catch { rm msg.err } 
	}

	if { $Error != "" } {
	  set header "\n$Error"
	  $base.message#1 configure -text $header
	  if [file exists msg.err] {
	    listFill $base [exec -keepnewline cat msg.err]
	  }
	  update
	}
}




proc MsgSummary { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 ListInactives ListInactives
	upvar #0 AutoUpdateSummary AutoUpdateSummary
	upvar #0 AutoUpdateSummaryCPU AutoUpdateSummaryCPU
	upvar #0 AutoUpdateActive AutoUpdateActive
	upvar #0 AutoUpdatePeriodSecs AutoUpdatePeriodSecs
	upvar #0 PageLength PageLength
	upvar #0 NodeName NodeName
	upvar #0 Shmid Shmid

	set AutoUpdateSummaryCPU 0
	set AutoUpdateSummary    1

	set where [$base.listbox#1 yview]
        set where [string trimleft $where]
        set wordend [expr [string first " " $where] - 1 ]
        if { $wordend < 0 } { set wordend [expr [string length $where] - 1 ]  }
        set where [string range $where 0 $wordend]


	if { ![ MsgDo . "getLines" ] } {
	  set PageLength [exec cat msg.tmp]
	}
	MsgDo . "lines 0"

	if { ![ MsgDo . "-l$ListInactives list" ] } {
	  set header [exec head -3 msg.tmp | tail +2 ]
	  $base.message#1 configure -text $header
	  listFill $base [exec -keepnewline tail +4 msg.tmp]
	}

	if { ![ MsgDo . "getnode" ] } {
	  set NodeName [exec cat msg.tmp]
	  if { $NodeName == "" } { set NodeName "No Node Name" }
	}

	if { ![ MsgDo . "getshmid" ] } {
	  set Shmid [exec cat msg.tmp]
	  if { $Shmid == "" } { set Shmid "-1" }
	}

	MsgDo . "lines $PageLength"
	$base.listbox#1 yview moveto $where
	update

	if { $AutoUpdateActive == 1 } {
#	  Convert AutoUpdatePeriodSecs to milliseconds, for the "after" command:
#	  (And subtract off 3 secs of "overhead".)
	  set secs [expr $AutoUpdatePeriodSecs-3]
	  if { $secs < 1 }  {set secs 1}
	  set ms [expr $secs*1000]
	  after $ms {
	    if { $AutoUpdateSummary == 1 } {
#	      This might have been turned off during the wait:
	      MsgSummary .
	    }
	  }
	}
}




proc MsgSummaryCPU { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 ListInactives ListInactives
	upvar #0 AutoUpdateSummary AutoUpdateSummary
	upvar #0 AutoUpdateSummaryCPU AutoUpdateSummaryCPU
	upvar #0 AutoUpdateActive AutoUpdateActive
	upvar #0 AutoUpdatePeriodSecs AutoUpdatePeriodSecs
	upvar #0 PageLength PageLength
	upvar #0 NodeName NodeName
	upvar #0 Shmid Shmid

	set AutoUpdateSummary    0
	set AutoUpdateSummaryCPU 1

	set where [$base.listbox#1 yview]
        set where [string trimleft $where]
        set wordend [expr [string first " " $where] - 1 ]
        if { $wordend < 0 } { set wordend [expr [string length $where] - 1 ]  }
        set where [string range $where 0 $wordend]

	if { ![ MsgDo . "getLines" ] } {
	  set PageLength [exec cat msg.tmp]
	}
	MsgDo . "lines 0"

	if { ![ MsgDo . "-l$ListInactives cpu" ] } {
	  set header [exec head -3 msg.tmp | tail +2 ]
	  $base.message#1 configure -text $header
	  listFill $base [exec -keepnewline tail +4 msg.tmp]
	}

	if { ![ MsgDo . "getnode" ] } {
	  set NodeName [exec cat msg.tmp]
	  if { $NodeName == "" } { set NodeName "No Node Name" }
	}

	if { ![ MsgDo . "getshmid" ] } {
	  set Shmid [exec cat msg.tmp]
	  if { $Shmid == "" } { set Shmid "-1" }
	}

	MsgDo . "lines $PageLength"
	$base.listbox#1 yview moveto $where
	update

	if { $AutoUpdateActive == 1 } {
#	  Convert AutoUpdatePeriodSecs to milliseconds, for the "after" command:
#	  (And subtract off 3 secs of "overhead".)
	  set secs [expr $AutoUpdatePeriodSecs-3]
	  if { $secs < 1 }  {set secs 1}
	  set ms [expr $secs*1000]
	  after $ms {
	    if { $AutoUpdateSummaryCPU == 1 } {
#	      This might have been turned off during the wait:
	      MsgSummaryCPU .
	    }
	  }
	}
}




proc MsgAutoUpdateStart { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 AutoUpdateActive AutoUpdateActive
	upvar #0 AutoUpdateSummary AutoUpdateSummary
	upvar #0 AutoUpdateSummaryCPU AutoUpdateSummaryCPU

	if { $AutoUpdateActive == 0 } {
#	  It's been turned off.
	  set AutoUpdateSummary 0
	  set AutoUpdateSummaryCPU 0
	} elseif { $AutoUpdateSummary == 1 } {
	  set AutoUpdateSummary 0
	  MsgSummary .
	} elseif { $AutoUpdateSummaryCPU == 1 } {
	  set AutoUpdateSummaryCPU 0
	  MsgSummaryCPU .
	}
}




proc listFill { root wholetext } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}

	$base.listbox#1 delete 0 end

	set text $wholetext
	set next 1
	while {$next>=0} {
	  set next [string first "\n" $text]
	  if { $next > 0 } {
	    $base.listbox#1 insert end [string range $text 0 [expr $next-1 ] ]
	    set text [string range $text [expr $next+1] end]
#	    Skip leading spaces (apparently, the carriage return at the end of the
#	                         summary lines gets translated in 10 spaces!):
	    while {(  [string length $text] > 0             ) && (
	             ([string compare [string index $text 0] " "]==0) ) } {
	      set text [string range $text 1 end]
	    }
	  } elseif { $next == 0 } {
	    set text [string range $text [expr $next+1] end]
	  }
	}
	$base.listbox#1 selection clear 0 end

}




initMsgVariables
msgui_ui .
bind .listbox#1 <Double-Button-1> {MsgSelectPrefix .}
bind .entry#9 <Return> {MsgSetLines .}
bind .entry#10 <Return> {MsgNodeNameSet .}
