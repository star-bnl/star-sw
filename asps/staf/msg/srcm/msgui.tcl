global allPidsList
global LoadFrom
global Executable
global StoreTo
global ListInactives
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
	upvar #0 Executable Executable
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
	upvar #0 NodeName NodeName
	upvar #0 Shmid Shmid
	set allPidsList ""
	set LoadFrom default.msg
	set Executable "msgstest"
	set Prefix ""
	set Counts 0
	set CountLimit 0
	set AbortLimit 0
	set AlarmLevel 0
	set Active 0
	set Counting 0
	set Alarming 0
	set ListInactives 0
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
	upvar #0 Executable Executable

	if { [string length $Executable] > 0 } {
	  catch { eval exec msgControl -e$Executable $command | tee msg.err > msg.tmp } Error
	} else {
	  catch { eval exec msgControl -f$LoadFrom   $command | tee msg.err > msg.tmp } Error
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
	upvar #0 Executable Executable

	if { [string length $LoadFrom] <= 0 } {
	  set Error "You need to specify an Msg state file (path) to load the executable from."
	  catch { rm msg.err }
	} elseif { [string length $Executable] > 0 } {
	  catch { exec msgControl -e$Executable load $LoadFrom | tee msg.err > msg.tmp } Error
	} else {
	  set Error "You need to specify an executable file (path) to be loaded from the state file."
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
	upvar #0 Executable Executable
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
	upvar #0 Executable Executable
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
	if { $Prefix != $word } { set Error "MsgPrefixGet-F2 prefix unexpected" }

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
	upvar #0 Executable Executable
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
	upvar #0 Shmid Shmid
	if { $Shmid > 0 } { exec rmid $Shmid }
}




proc MsgSelectExecutable { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 Executable Executable
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
	upvar #0 Executable Executable

	if { [string length $StoreTo] <= 0 } {
	  set Error "You need to specify an Msg state file (path) in which to store the executable's Msg state."
	  catch { rm msg.err } 
	} elseif { [string length $Executable] > 0 } {
	  catch { exec msgControl -e$Executable store $StoreTo | tee msg.err > msg.tmp } Error
	} else {
	  set Error "You need to specify an executable file (path) to have its state stored in the state file."
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
	upvar #0 PageLength PageLength
	upvar #0 NodeName NodeName
	upvar #0 Shmid Shmid

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
	update
}




proc MsgSummaryCPU { root } {
	if {$root == "."} {
	  set base ""
	} else {
	  set base $root
	}
	upvar #0 ListInactives ListInactives
	upvar #0 PageLength PageLength
	upvar #0 NodeName NodeName
	upvar #0 Shmid Shmid

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
	update
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
	  } elseif { $next == 0 } {
	    set text [string range $text [expr $next+1] end]
	  }
	}
	$base.listbox#1 selection set 0

}




initMsgVariables
msgui_ui .
bind .listbox#1 <Double-Button-1> {MsgSelectPrefix .}
bind .entry#9 <Return> {MsgSetLines .}
bind .entry#10 <Return> {MsgNodeNameSet .}
