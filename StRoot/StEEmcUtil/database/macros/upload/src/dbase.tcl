

namespace eval DBase {
    variable dbname
    variable dbflavor
    variable dbversion
    variable dbtree
    variable dbwindows
    variable dbtable
    variable dbtableinfo
    variable dbstruct
    variable dbstructIdx
    variable dbcomment
    variable dbsecurepast

    set dbname          Calibrations_eemc     
    set dbversion       VerA
    set dbflavor        ofl

    set dbwindows(list) ""
    set dbwindows(text) ""
    set dbwindows(log)  ""

    set dbsecurepast  [ list CWchar PMTchar ]
    set dbstructIdx 0

    # ---------------------------------------------------------
    # 
    # ---------------------------------------------------------
    proc getTableInfo {cstack cdata saved_data cattr saved_attr args} {
	upvar  $saved_data sdata
	upvar  $saved_attr sattr
	variable dbtableinfo
	set      dbtableinfo $cdata
	return SXML_PURGE
    }
    
    proc getStructInfo {cstack cdata saved_data cattr saved_attr args} {
	upvar  $saved_data sdata
	upvar  $saved_attr sattr
	variable dbstruct
	variable dbstructIdx
	set cdata [ string trim $cdata ]
	if { [ string compare $cdata "comment" ] &&  [ string compare $cdata "dumm" ] } {
	    set    dbstruct($dbstructIdx,name) [ string range $cdata 0 7 ]
	    set    i  [ string last ":" $cstack ]
	    set    i  [ expr $i + 3 ] ; # get rid of ':db'
	    set    dbstruct($dbstructIdx,type) [ string range $cstack $i end ]
	    #puts  "$cdata -> $dbstruct($dbstructIdx,name) -> $dbstruct($dbstructIdx,type)"
	    incr dbstructIdx
	}
	return SXML_PURGE
    }
    

    proc printStructHeader {} {
	variable dbtableinfo
	variable dbstruct
	variable dbstructIdx

	set header "";
	for { set i 0 } { $i < $dbstructIdx } { incr i } {
	    append header "$dbstruct($i,name)\t";
	} 
	append header "\n";
	for { set i 0 } { $i < $dbstructIdx } { incr i } {
	    append header "\[$dbstruct($i,type)\]\t";
	} 
	return $header;
	
    }


    # ---------------------------------------------------------
    # 
    # ---------------------------------------------------------
    proc GetConfig { } {
	variable dbname
	variable dbversion
	variable dbtree
	variable dbexec
	
	catch { exec $dbexec -D $dbname -C -q } tmp
	set tmp [ split $tmp '\n' ]
	set configList ""
	foreach xc $tmp {
	    set c [ string first ":" $xc ]
	    if { $c > 1 } { 
		set   xc [ string trim [ string range $xc 0 [ expr $c - 1] ] ]
		lappend configList $xc
	    }
	}
	return $configList 
    }

    proc GetTree { } {
	variable dbname
	variable dbversion
	variable dbtree
	variable dbexec

	
	catch { exec $dbexec -D $dbname -p $dbversion -T -q } tmp
	set  i    [ string first $dbversion   $tmp ]
	set  tmp  [ string range $tmp $i end ]
	set  tmp  [ split $tmp "\n" ]
	# clear dbtree array
	array unset dbtree 
	foreach node $tmp {
	    set  level 1
	    for { set level 1 } { [ string first "\t" $node $level ] >= 0 }  { incr level } {
		continue
	    }
	    set node  [ string trimleft  $node "\t" ]
	    set top($level) $node
	    if { [ string last   "/"   $node  ] < 0 } { 
		set t ""
		for { set l 1 } { $l < $level } { incr l } {
		    append t $top($l)
		}
		set colon  [ string first ":" $node ]
		if { $colon > 0 } { 
		    set name   [ string range  $node 0                 [ expr $colon - 1 ] ]
		    set struct [ string range  $node [ expr $colon + 1 ]  end              ]
		    append  t $name
		    #lappend tree $t
		    set dbtree($t) $struct
		} 
	    }
	}
    }


    proc SetComment { comment_var { w .comment } } {
	global   tkPriv
	upvar    $comment_var comment

	toplevel    $w -class Dialog
	wm title    $w "SetComment"
	wm iconname $w "SetComment"
	wm protocol $w WM_DELETE_WINDOW { }

	frame   $w.label  -border 1
	frame   $w.text   -border 1
	frame   $w.entry  -border 1
	frame   $w.button -border 1
	pack    $w.label $w.text $w.entry $w.button -side top -expand yes -fill x

	frame   $w.label.l1
	#label   $w.label.l1.bitmap -bitmap question
	canvas  $w.label.l1.bitmap -width 32 -height 32 -highlightthickness 0
	$w.label.l1.bitmap create image 0 0 -anchor nw -image DBase::ballon1
	$w.label.l1.bitmap create image 0 0 -anchor nw -image DBase::ballon2
	$w.label.l1.bitmap create image 0 0 -anchor nw -image DBase::qmark

	label   $w.label.l1.text   -text "Do you really want to write to the database?"
	pack    $w.label.l1.bitmap $w.label.l1.text -side left   -expand yes -fill x -padx 10

	label   $w.label.l2 -text "Enter Comment and Press WRITE"
	pack    $w.label.l1 $w.label.l2  -side top   -expand yes -fill x -padx 10

	entry   $w.entry.e1 -textvariable $comment_var
	pack    $w.entry.e1 -side top -expand yes -fill x -padx 5
	
	button  $w.button.ok     -width 10 -text WRITE  -command [list set tkPriv(button) OK     ]
	button  $w.button.cancel -width 10 -text CANCEL -command [list set tkPriv(button) Cancel ]
	
	pack    $w.button.ok $w.button.cancel    -side left -expand yes -fill x
	

	::tk::PlaceWindow $w widget .	

	::tk::SetFocusGrab $w .

	tkwait variable tkPriv(button)

	::tk::RestoreFocusGrab $w .
	return $tkPriv(button)
    }
    


    proc Select { xmlpath } {
	global   db
	variable dbtree
	variable dbtable
	variable dbtableinfo
	variable dbstruct
	variable dbstructIdx

	catch { selection get } dbtable
	#puts   $dbtable;

	set path  $dbtable
	if { [ lsearch -exact [ array names dbtree ] "$path" ] >= 0 } { 
	    set struct $dbtree($path)

	    set id [sxml::init $xmlpath/$struct.xml  ]

	    if { $id > 0 } {
		sxml::register_routine $id "stardatabase:stdbtable:comment"  DBase::getTableInfo
		sxml::register_routine $id "stardatabase:stdbtable:dbfloat"  DBase::getStructInfo
		sxml::register_routine $id "stardatabase:stdbtable:dbint"    DBase::getStructInfo
		sxml::register_routine $id "stardatabase:stdbtable:dbshort"  DBase::getStructInfo
		sxml::register_routine $id "stardatabase:stdbtable:dbushort" DBase::getStructInfo
		sxml::register_routine $id "stardatabase:stdbtable:dbstring" DBase::getStructInfo
		set dbtableinfo ""
		
		foreach p [ array names dbstruct ] {
		    unset dbstruct($p)
		}
		set dbstructIdx 0
		set x [sxml::parse $id]
		sxml::end $id
		set dbtableinfo [ string range [ string trim $dbtableinfo ] 1 end ]
		set dbtableinfo "$path: $dbtableinfo"
		set db(tableinfo)    $dbtableinfo
		set db(tableheader)  [ printStructHeader ]
	    }
	}
	return $dbtable;
    }
    

    proc SetFlavor { flavor } {
    	variable dbflavor
	set dbflavor $flavor
    }
    
    proc IO { command { verbose 1 } } { 
	global   db 
	variable dbtree 
	variable dbname
	variable dbflavor
	variable dbversion
	variable dbwidget
	variable dbtable
	variable dbexec
	variable dbsecurepast


	set wr $dbwidget(text)
	set wl $dbwidget(log)

	set fileName "/tmp/[file tail $dbexec]-[pid].$command"
	
	set path     $dbtable

	set fullpath $dbversion/$path
	
	if {  [ string length "$path" ] <= 0 } { 
	    errMsg "select database table first!"
	    return
	}
	
	if { [ lsearch -exact [ array names dbtree ] "$path" ] < 0 } { 
	    errMsg "table '$path' is not valid"
	    return
	}
	
	switch -- $command {
	    READ    { 
		catch { exec $dbexec -q -t $db(timestamp) -D $dbname -p $fullpath -F $dbflavor -g -f $fileName } log
		$wr delete 0.0 end
		if { ! [ catch {  open $fileName } fd ] }  {
		    gets $fd db(key)
		    if { [ string length $db(key) ] <=0 } { 
			errMsg "No Data for Selected Time Stamp"
		    } 
		    while { ! [ eof $fd ] } {
			gets $fd tmp
			regsub -all " " $tmp "\t" line
			$wr insert end  "$line\n"
		    }
		    close $fd
		}
	    }
	    WRITE   {
		set  currts [ clock seconds                ]
		set  unixts [ clock scan    $db(timestamp) ]
		incr unixts ; # add one second
		if { $unixts < $currts } { 
		    foreach item $dbsecurepast { 
			if { [ string first $item $path ] >= 0 } { 
			    errMsg "cannot change past for table '$path'";
			    return
			}
		    }
		}
		set db(comment) ""
		while {  [ string length $db(comment) ] <= 0  }  {  
		    if { [ SetComment db(comment) ] != "OK" }  { return; };
		    set db(comment) [ string trim $db(comment) ]
		    if { [ string length $db(comment) ] <=0 } {
			errMsg "comment missing"
		    }
		}
			 
		#if { ! [ ackMsg "Do you really want to load database?" ] } { return;   } 
		set data [ $wr get 0.0 end ]
		set fd   [ open $fileName "w" ];
		if { $fd > 0 } { 
		    set db(key) "#$path"
		    puts $fd $db(key)
		    puts $fd $data
		  
		    close $fd
		    catch { \
				exec $dbexec -q -t $db(timestamp) -D $dbname -p $fullpath -F $dbflavor -s \
				-f $fileName -c "$db(comment)" } log
		}
		DBase::IO READ 0
	    }
	    
	    HISTORY {
		catch { exec $dbexec -t 2000-01-01 -H -D $dbname -p $fullpath -F $dbflavor -q -f $fileName} log
		catch { exec cat $fileName   } res
		append log "\n$res"
	    }

	}
	
	
	$wl config -state normal
	if { $verbose } { 
	    set date [ clock format [ clock seconds ] ]
	    $wl insert end "\n\n"
	    $wl insert end "####################################################################################################### \n"
	    $wl insert end "#  DBASE $command ACCESS TO TABLE $path AT $date \n"
	    $wl insert end "####################################################################################################### \n"
	    $wl insert end $log
	}
	$wl yview  end
	$wl config -state disabled
	catch { file delete -force $fileName }
    }

    proc GetDB { config_name {config_version ""} }  {
	variable dbname
	variable dbversion 
	variable dbtree
	variable dbwidget
	variable dbexec
	variable dbtable

	global   dbconf

	set dbname       $config_name
	set dbversion    $config_version

	set dbexec       $dbconf(exec)
	set dbsecurepast [ split $dbconf(securepast) ]
	set dbtable      ""
	if { [ string length $dbversion ] <= 0 } { 
	    set configList [ GetConfig ]
	    if { [ llength $configList ] > 0 } {
		set dbversion [ lindex $configList end ]
		set dbconf(version) $dbversion
	    } else {
		$dbwidget(log) insert end "\n *** no config found for $dbname *** "
		return
	    }
	}
	
	GetTree

	$dbwidget(list) delete 0 end
	if { [ array size dbtree ] > 0 } { 
	    foreach p [ lsort [ array names dbtree ] ] {
		$dbwidget(list) insert end $p
	    }
	} else {
		$dbwidget(log) insert end \
		    "\n *** no tables found for $dbname version $dbversion *** "
	}

	#puts [ GetConfig ]
    }


    proc Init { config_name config_flavor config_version list_widget text_widget log_widget}  {
	variable dbname
	variable dbflavor
	variable dbversion 
	variable dbtree
	variable dbwidget
	variable dbexec
	variable dbtable

	global   dbconf

	option add *DBaseDialog*foreground          yellow   userDefault
	option add *DBaseDialog*background          red      userDefault
	option add *DBaseDialog*activeForeground    white    userDefault
	option add *DBaseDialog*activeBackground    green3   userDefault
	option add *DBaseDialog*text*foreground     black    userDefault
	option add *DBaseDialog*button*foreground   black    userDefault
	option add *DBaseDialog*button*background   red      userDefault
	option add *DBaseDialog*entry*foreground    black    userDefault
	option add *DBaseDialog*entry*background    red      userDefault

	set dbwidget(list) $list_widget
	set dbwidget(text) $text_widget
	set dbwidget(log)  $log_widget

	SetFlavor $config_flavor
	GetDB $config_name $config_version

	image create bitmap DBase::ballon1 -foreground black \
	    -data "#define b1_width 32\n#define b1_height 32
    static unsigned char b1_bits[] = {
    0x00, 0xf8, 0x1f, 0x00, 0x00, 0x07, 0xe0, 0x00, 0xc0, 0x00, 0x00, 0x03,
    0x20, 0x00, 0x00, 0x04, 0x10, 0x00, 0x00, 0x08, 0x08, 0x00, 0x00, 0x10,
    0x04, 0x00, 0x00, 0x20, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
    0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80,
    0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80, 0x01, 0x00, 0x00, 0x80,
    0x01, 0x00, 0x00, 0x80, 0x02, 0x00, 0x00, 0x40, 0x02, 0x00, 0x00, 0x40,
    0x04, 0x00, 0x00, 0x20, 0x08, 0x00, 0x00, 0x10, 0x10, 0x00, 0x00, 0x08,
    0x60, 0x00, 0x00, 0x04, 0x80, 0x03, 0x80, 0x03, 0x00, 0x0c, 0x78, 0x00,
    0x00, 0x30, 0x04, 0x00, 0x00, 0x40, 0x04, 0x00, 0x00, 0x40, 0x04, 0x00,
    0x00, 0x80, 0x04, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x06, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
	image create bitmap DBase::ballon2 -foreground white \
	    -data "#define b2_width 32\n#define b2_height 32
    static unsigned char b2_bits[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x1f, 0x00, 0x00, 0xff, 0xff, 0x00,
    0xc0, 0xff, 0xff, 0x03, 0xe0, 0xff, 0xff, 0x07, 0xf0, 0xff, 0xff, 0x0f,
    0xf8, 0xff, 0xff, 0x1f, 0xfc, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x3f,
    0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f,
    0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f, 0xfe, 0xff, 0xff, 0x7f,
    0xfe, 0xff, 0xff, 0x7f, 0xfc, 0xff, 0xff, 0x3f, 0xfc, 0xff, 0xff, 0x3f,
    0xf8, 0xff, 0xff, 0x1f, 0xf0, 0xff, 0xff, 0x0f, 0xe0, 0xff, 0xff, 0x07,
    0x80, 0xff, 0xff, 0x03, 0x00, 0xfc, 0x7f, 0x00, 0x00, 0xf0, 0x07, 0x00,
    0x00, 0xc0, 0x03, 0x00, 0x00, 0x80, 0x03, 0x00, 0x00, 0x80, 0x03, 0x00,
    0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
	image create bitmap DBase::qmark -foreground blue \
	    -data "#define q_width 32\n#define q_height 32
    static unsigned char q_bits[] = {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x07, 0x00,
    0x00, 0x10, 0x0f, 0x00, 0x00, 0x18, 0x1e, 0x00, 0x00, 0x38, 0x1e, 0x00,
    0x00, 0x38, 0x1e, 0x00, 0x00, 0x10, 0x0f, 0x00, 0x00, 0x80, 0x07, 0x00,
    0x00, 0xc0, 0x01, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0xc0, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0xe0, 0x01, 0x00,
    0x00, 0xe0, 0x01, 0x00, 0x00, 0xc0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};"
    }

}
