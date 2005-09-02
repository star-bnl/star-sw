#!/usr/local/ActiveTcl/bin/tclsh
#!/usr/bin/wish

set my(inp) in/gainsT-allSect.dat 
set my(out) gainsT-iter3.dat 
set out(name) ""

#===================================
#===================================
proc readInpGain { } {

    global my out
    set fd0 [open $my(inp) "r"]

    set n1 0
    set out(head) ""

    while {[gets $fd0 line] >= 1} {
        if { [string index $line 0]=="\#" } {
	    set out(head) "$out(head)\n$line"
	    continue;# save comments
	}
        set count [scan $line "%s %f %s " pix gain erGain ]
        if {$count != 3} {
            puts "Error reading input - terminating script,count=$count";exit
        }
        incr n1
        #puts "$line [string is digit $erGain ] $erGain "
	if { [string first nan $erGain ]>=0 } { 
	    set erGain 888999 
	}
	if { [expr $erGain/2 ] > $gain } {
	    puts "$pix $gain $erGain"
	}
	set out($pix-g) $gain
	set out($pix-eg) $erGain
	set out($pix-all) $line
	set out(name) "$out(name) $pix"
    }
    puts " gains for  $n1 pixels "
    close $fd0
}

#===================================
#===================================
proc readOneSlope { pix } {

    global outB
    set fd0 [open $pix.slope "r"]

    set n1 0
    while {[gets $fd0 line] >= 1} {
        set count [scan $line "%s %f %f " pix sl erSl ]
        if {$count != 3} {
            puts "Error reading input - terminating script,count=$count";exit
        }
        incr n1
        #puts $line
        #puts "$pix $gain"
	set outB($pix-s) $sl
	set outB($pix-es) $erSl
    }
    puts " $n1 slopes for $pix"
    close $fd0
}

#===================================
#===================================
proc fixOneTile { pix0 } {
    
    global outB out my
    
    set tL { A B C D E }
    puts =$pix0=
    set sR 0
    set sN 0
    foreach x $tL {
	set pix [string replace $pix0 3 3 $x]
	#puts "$pix:[string first $pix $$my(pixL) ] "
	if { [string first $pix $$my(pixL) ]>=0 } continue
	set r [expr  $out($pix-g)*$outB($pix-s) ]
	set sR [expr $sR + $r ]
	incr sN
	#puts "$x=$pix= g=$out($pix-g) s=$outB($pix-s) r=$r "
    }
    set fac [expr $sR/$sN]
    puts "fac=$fac sN=$sN"
    set newG [expr $fac/$outB($pix0-s) ]
    puts "$pix sl=$outB($pix-s) nG=$newG"
    set out($pix0-g) [format %.3f $newG]
    set out($pix0-eg) 1.000
}


#===================================
#===================================
proc writeAll {  } {
    global my out
    set fd1 [open $my(out) "w"]
    puts $fd1 $out(head)
    puts $fd1 "\# some pixles corrected using slopes"

#    for {set sec 1} { $sec<=12 } { incr sec} {
#	puts "sec=$sec"
#    }

    foreach pix $out(name) {
	#puts "pix=$pix"
	puts $fd1 "$pix  $out($pix-g)  $out($pix-eg)"
    }

}


#===================================
#===================================
proc pr5 { pix0 } {
    global out
    set tL { A B C D E }
    foreach x $tL {
	set pix [string replace $pix0 3 3 $x]
	puts "$pix  $out($pix-g)  $out($pix-eg)"
    }
}



#===================================
#   M A I N 
#===================================
readInpGain 
exit
#set xpix "07TD03"
set my(pixL) " 08TB07 04TB07 06TA07 07TD03 08TE05 08TD07 08TD09 01TD12 03TE12"

foreach xpix $my(pixL) {
    #set xpix "04TB07"
    puts "fit slopes around $xpix ...."
    catch { exec root -b -q  fitSlope5.C\("$xpix\") } ret ;#puts "ret=$ret"
    readOneSlope $xpix 
    #pr5 $xpix
    fixOneTile $xpix
    pr5 $xpix
  #break
} 
   writeAll
exit



exit

#  catch {  exec mysql -h duvall.star.bnl.gov -s -e " connect operation; select numevt from DAQInfo where  (( status in (2,3) ) or ( xstatus1>0 ))  and runNumber=$runNo "  } ret
