#!/usr/bin/tclsh

#=================================
#           MAIN
#=================================
puts " prepares input files/path to load peds for subsequent runs"
puts " " ;

set eemcDbPath  yourPath/StRoot/StEEmcUtil/database/macros/upload/src/
set inpPath  /star/u/stevens4/ped12/offline200GeV/StatFiles
# list of runs to be uploaded with content formatted as follows
# FLLLLL,RMMMMMMMM,NNNNNNNNNNN 
# where LLLLL is fill #, MMMMMMMM is run number, and NNNNNNNNNNN is unix timestamp
set fd [open ./runList-pp200.txt "r"]

set nF 0
while {[gets $fd line] >= 1} {
    puts lll=$line
    if { [string index $line 0]=="\#" } continue;
    set x [split $line ","]
    incr  nF

    #### original 3 inputs
    set Ffill [lindex $x 0] 
    set Rrun  [lindex $x 1]
    set uTime [lindex $x 2]

    set uTime [expr $uTime-10] ;# subtract 10 seconds, to have room for fixes

    # edit comment here to match our upload
    puts "Fill=F$Ffill" 
    set com "$Ffill, from $Rrun, p+p 200 GeV 2012, Justin"    

    puts "=$com= ... working.... wait ~1 minute ..." ;
    
    set pathR $inpPath/$Rrun
    
    puts "pathR=$pathR" ;
    exit
    catch {  exec writePed+Status.sh  $uTime $com $pathR $eemcDbPath } ret
    puts  "=$ret="
    exit   
    puts " " ;
}

close $fd
exit

