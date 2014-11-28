namespace eval Help {
    variable balloon

    set      balloon .balloon


    # -------------------------------------------------------
    #  HelpOnItem
    # -------------------------------------------------------
    proc onItem { target message } {
	variable balloon
	set w $balloon

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
    proc offItem { } { 
	variable balloon; 
	catch { destroy $balloon } 
    }

    
    
    # ---------------------------------------------------------
    # Help Manual button
    # ---------------------------------------------------------
    proc Manual  { title text { w .helpver } } {
	catch { destroy $w }
	toplevel    $w -class HelpVersion
	wm title    $w "$title Manual"
	wm iconname $w "$title Manual"
	
	frame   $w.label  -border 1
	frame   $w.text   -border 1
	frame   $w.button -border 1
	pack    $w.label $w.text $w.button -side top -expand yes -fill x
	
	label   $w.label.label  -text $title
	pack    $w.label.label -side top -expand yes -fill x -padx 10
	
	label   $w.text.l1 -text $text
	pack    $w.text.l1 -side top -expand yes -fill x -padx 5
	
	button  $w.button.ok     -width 10 -text OK     -command "destroy $w"
	pack    $w.button.ok     -side top -expand yes -fill x
	
	bind  $w <Key-Return> "$w.button.ok invoke"

	::tk::PlaceWindow $w widget .	
	::tk::SetFocusGrab $w .

	tkwait window $w

	::tk::RestoreFocusGrab $w .	

	return
    }


    # ---------------------------------------------------------
    # Help About button
    # ---------------------------------------------------------
    proc About { title longtitle msg logo { w .helpver } } {
	catch { destroy $w }
	toplevel    $w -class HelpVersion
	wm title    $w "About: $title"
	wm iconname $w "About: $title"

	frame   $w.label  -border 1
	frame   $w.main   -border 1
	frame   $w.button -border 1
	pack    $w.label $w.main $w.button -side top -expand yes -fill x
	
	
	label   $w.label.label -text "$longtitle"
	pack    $w.label.label -side top -expand yes -fill x -padx 10


	frame $w.main.text
	set i 0
	foreach item [ split $msg "\n" ] { 
	    label   $w.main.text.$i -anchor w  -text $item
	    pack    $w.main.text.$i -side top  -expand yes -fill x -padx 10
	    incr i
	}
	canvas $w.main.logo -width 128 -height 128 -highlightthickness 0
	image create photo ::help::iulogo  -file $logo
	$w.main.logo create image 0 0 -anchor nw -image ::help::iulogo	

	pack  $w.main.text $w.main.logo -side left -expand yes -fill both


	button  $w.button.ok     -width 10 -text OK    -command "destroy $w"
	pack    $w.button.ok     -side top -expand yes -fill x
	
	bind  $w <Key-Return> "$w.button.ok invoke"

	::tk::PlaceWindow $w widget .	
	::tk::SetFocusGrab $w .

	tkwait window $w

	::tk::RestoreFocusGrab $w .	
	
	return
    }


    # -------------------------------------------------------
    #  Window Info
    # -------------------------------------------------------
    proc WindowInfo { w msg } {
	bind $w <Enter> "Help::onItem $w \"$msg\""
	bind $w <Leave> "Help::offItem"
    }


    proc Init {} {
	option add *HelpVersion*font                -*-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*-1 userDefault
	option add *HelpVersion*foreground          yellow   userDefault
	option add *HelpVersion*background          \#b50000 userDefault
	option add *HelpVersion*activeForeground    white    userDefault
	option add *HelpVersion*activeBackground    green    userDefault
	option add *HelpVersion*text*foreground     white    userDefault
	option add *HelpVersion*button*foreground   black    userDefault
	option add *HelpVersion*button*background   gray85   userDefault
    }


}

