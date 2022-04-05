#!/usr/bin/wish
#!/usr/bin/tclsh
#!/usr/local/ActiveTcl/bin/wish 
#!/usr/local/ActiveTcl/bin/tclsh

set balloon .ballon
set my(expert) 0 ;# set it to 1 to turn off baloons
set janMail  "balewski@iucf.indiana.edu"
set allMailL "balewski@iucf.indiana.edu rfatemi@iucf.indiana.edu  sowinski@iucf.indiana.edu "
after 300000 { puts "TCL-die on time out"; exit }

#............. initialization based on input arguments ......
if { $argc<2 } { 
    puts nArg=$argc
    puts "give me: nBad & logFIle ";
    exit
}

set my(logFile) [lindex $argv 0]
set my(nBad)    [lindex $argv 1]
set my(logPath) "" 
puts "TCL-spyBox $my(logFile)  argc=$argc"

if { $argc==3 } { ;# alos path is supplied
    set my(logPath)   [lindex $argv 2] 
    puts "TCL-spyBox  path $my(logPath)"
}


#exit

#
# Example 30-1
# A text widget and two scrollbars.
#
#.................................................
proc Scrolled_Text { f args } {
        frame $f
        eval {text $f.text -wrap none \
                -xscrollcommand [list $f.xscroll set] \
                -yscrollcommand [list $f.yscroll set]} $args
        scrollbar $f.xscroll -orient horizontal \
                -command [list $f.text xview]
        scrollbar $f.yscroll -orient vertical \
                -command [list $f.text yview]
        grid $f.text $f.yscroll -sticky news
        grid $f.xscroll -sticky news
        grid rowconfigure $f 0 -weight 1
        grid columnconfigure $f 0 -weight 1
        return $f.text
}


# -------------------------------------------------------
#  HelpOnItem
# -------------------------------------------------------
proc HelpOnItem { target message } {
     global balloon my
     set w $balloon

     if { $my(expert) } return;

     set x [expr [winfo rootx $target] + ([winfo width $target]/2)]
     set y [expr [winfo rooty $target] + [winfo height $target] + 4]
     if { [ winfo exists $w ] } { return }
     toplevel $w -bg black
     wm overrideredirect $w 1
     label $w.l \
             -text $message -relief flat \
             -bg #ffffaa -fg black -padx 2 -pady 0 -anchor w
     pack $w.l -side left -padx 1 -pady 1
     wm geometry $w +${x}+${y}
     return
}


# -------------------------------------------------------
#  Window Info
# -------------------------------------------------------
proc WindowInfo { w msg } {
     bind $w <Enter> "HelpOnItem $w \"$msg\""
     bind $w <Leave> { global balloon; catch { destroy $balloon } }
}



#===================================
#===================================
proc sentMail { mailList x } {
    global my
    foreach addr  $mailList  {
	puts "send mail to $addr , file=$my(logFile)"
	catch { exec  /bin/mail -s "eeSpy nBad=$my(nBad) $x " $addr < $my(logPath)$my(logFile) } tmp2
	puts "tmp2=$tmp2"
    }
}



#------------------- GUI -------
set w .res0
frame $w -relief groove  -border 2 
pack $w 

label $w.tit1 -text "nBad=" 
label $w.body1  -relief sunken -text $my(nBad)
label $w.tit -text " LogFile=" 
label $w.body  -relief sunken -text $my(logFile)
pack $w.tit1 $w.body1  $w.tit $w.body -side left -expand yes -pady 2 -anchor w


button $w.quit -text "Ignore" -command  { exit}
button $w.m1 -text "mail Jan" -command  { sentMail $janMail 2jan; exit }
button $w.m2 -text "mail All" -command  { sentMail $allMailL 2all; exit }
pack $w.quit $w.m1 $w.m2  -side right
  
set t [Scrolled_Text .f -width 50 -height 10]
pack .f -side top -fill both -expand true
set in [open $my(logPath)$my(logFile)]
$t insert end [read $in]
close $in


#...... add baloons to some windows ...........
WindowInfo $w.tit1   "Total \# of alarms\n set by this scan"
WindowInfo $w.body   "name of file\n displayed below"
WindowInfo $w.m1   "sent mail\n only to Jan"
WindowInfo $w.quit  "close window w/o sending mail\nit will disolve in 5 minutes anyhow"
set x ""
foreach addr  $allMailL { set x "$x$addr\n" }
 WindowInfo $w.m2   "sents mail to:\n$x" 
