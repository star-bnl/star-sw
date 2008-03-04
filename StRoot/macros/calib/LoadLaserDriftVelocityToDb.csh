#!/bin/csh

#
# Based on work by J. Castillo
# /star/u/jecc/tpcwrk/StarDb/Calibrations/tpc
#
# Modifieded on 31 Jan 2007 by G. Van Buren to use
# tags.root files with LoopOverLaserTree.C and
# handle possibility of additional files for
# laser runs already processed
#


set MKDIR=/bin/mkdir
set LS=/bin/ls
set TAR=/bin/tar
set CP=/bin/cp
set MV=/bin/mv
set RM=/bin/rm
set GREP=/bin/grep
set COLRM=/usr/bin/colrm
set DATEC=/bin/date
set TOUCH=/bin/touch
set SED=/bin/sed
set CAT=/bin/cat
set GZIP=/usr/bin/gzip
set SORT=/bin/sort
set FIND=/usr/bin/find

if( "$1" == "") then
    set  DIR=/star/institutions/bnl/genevb/DRIFTVEL/work
    set WDIR=/star/institutions/bnl/genevb/DRIFTVEL/
    echo "Oops !! Need argument 1 = path."
    echo "  Will use $DIR as default"
else
    set  DIR=$1
    set WDIR=/star/u/starreco/scripts
endif
if ( ! -d $DIR ) then
    echo "Sure !! $DIR not a directory ..."
    exit
endif


# will use in post-fix
set DATE=`$DATEC | $SED "s/ /_/g" | $SED "s/://g"`


echo "Starting tpcDriftVelocity at `$DATEC`"

set timeFile = $DIR/processingTime
if (! -e $timeFile) then
    $TOUCH $timeFile
    exit
endif

#
# Determine what new tags files exist
# copy them to $DIR and determine runs to process
#
set tagdirs = (/star/data09/reco/laser_rhicclock /star/data10/reco/laser_rhicclock)
set listOfRuns = ()
foreach tagdir ($tagdirs)
    cd $tagdir
    set tagfiles = `$FIND . -name "st_laser_*tags.root" -cnewer $timeFile `
    foreach tagfile ($tagfiles)
        set run = ${tagfile:h:t}
        if ($listOfRuns[$#listOfRuns] != $run) set listOfRuns = ($listOfRuns $run)
        set runDir = $DIR/runs/$run
        $MKDIR -p $runDir
        $CP $tagfile $runDir
    end
end
$TOUCH $timeFile

#
# Test if any new files and leave if not. 
# This will prevent empty dirs
#
if ($#listOfRuns == 0) exit


setenv STAR_LEVEL dev
source ${GROUP_DIR}/.stardev

# Location where the list of files to be uploaded will be kept
set listLaserFiles = "listOfLaserFiles$DATE.list"
set laserFiles = "$DIR/$listLaserFiles"
if ( -e $laserFiles ) $RM $laserFiles

#
# Produce tpcDriftVelocity macro file for each run
#
foreach run ($listOfRuns)
    set runDir = $DIR/runs/$run
    cd $runDir
    ${ROOTSYS}/bin/root.exe -b -l <<EOF 
.x $WDIR/LoopOverLaserTrees.C+("st_laser_*tags.root")
.q
EOF
    set laserMacro = `$LS tpcDriftVelocity*`
    if ( $#laserMacro != 0) then
        set macros = $runDir/macros
        $MKDIR -p $macros
        set fileToUpload = $laserMacro

        # Look for old macro files
        cd $macros
        set oldLaserMacro = `$LS -t -1 tpcDriftVelocity.*`
        if ( $#oldLaserMacro != 0) then
            set oldMacro = $oldLaserMacro[1]
            set oldDate = ${oldMacro:r:r:e}
            set oldTime = ${oldMacro:r:e}
            set oldHour = `echo $oldTime | $COLRM 3 `
            set oldMin = `echo $oldTime | $COLRM 1 2 | $COLRM 3 `
            set oldSec = `echo $oldTime | $COLRM 1 4 | $COLRM 3 `
            @ newSec = $oldSec + 1
            # format for date command to convert (handles date/time rollovers properly)
            set newDateTime = "$oldDate ${oldHour}:${oldMin}:${newSec}"
            set newDT = ` $DATEC --date="$newDateTime" '+%Y%m%d.%H%M%S' `
            set fileToUpload = "tpcDriftVelocity.$newDT.C"
        endif

        $CP $runDir/$laserMacro $DIR/$fileToUpload
        $MV $runDir/$laserMacro $macros/$fileToUpload
        $MV $runDir/LaserPlots.root $DIR/LaserPlots.$run.root
        $TOUCH $laserFiles
        echo $fileToUpload >> $laserFiles
    endif
end
  

#
# Prepare for uploading to DB
#
if ( ! -e $laserFiles ) exit

set tempfile = "/tmp/$listLaserFiles"
$SORT -u $laserFiles >! $tempfile
$RM $laserFiles
$MV $tempfile $laserFiles


#
# Create directories and perform backups
#


if( -e $DIR/Load ) then
    echo "Moving Load/ to LoadSavedOn$DATE/"
    $MV $DIR/Load $DIR/LoadSavedOn$DATE || exit
    $TAR -czf $DIR/LoadSavedOn$DATE.tar.gz $DIR/LoadSavedOn$DATE >&/dev/null &
endif
echo "Creating $DIR/Load"
$MKDIR $DIR/Load        || exit
echo "Creating $DIR/Load/Done"
$MKDIR $DIR/Load/Done   || exit
echo "Creating $DIR/Load/Failed"
$MKDIR $DIR/Load/Failed || exit
echo "Creating $DIR/Load/Others"
$MKDIR $DIR/Load/Others || exit

if( -e $DIR/Check ) then
    echo "Moving Check/ to CheckSavedOn$DATE/"
    $MV $DIR/Check $DIR/CheckSavedOn$DATE || exit
    $TAR -czf $DIR/CheckSavedOn$DATE.tar.gz $DIR/CheckSavedOn$DATE >&/dev/null &
endif
echo "Creating $DIR/Check"
$MKDIR $DIR/Check        || exit
echo "Creating $DIR/Check/BadRun"
$MKDIR $DIR/Check/BadRun || exit
echo "Creating $DIR/Check/VarSel"
$MKDIR $DIR/Check/VarSel || exit
echo "Creating $DIR/Check/VarOth"
$MKDIR $DIR/Check/VarOth || exit



cd $WDIR
echo `/bin/pwd`

echo "$DIR $listLaserFiles"
setenv DB_ACCESS_MODE write
$LS LoadLaserDriftVelocityToDb.C && $STAR/.$STAR_HOST_SYS/bin/root4star -b -l <<EOF 
.x $WDIR/LoadLaserDriftVelocityToDb.C("$DIR","$listLaserFiles","tpcDriftVelocity",1)
.q
EOF

# The directory structure is used internally to the .C
# We move now to target
if( -e $DIR/Load ) then
    $GZIP $DIR/Load/*.eps
    $MV $DIR/LaserPlots.*.root $DIR/Load
    echo "Moving Load/ to Load$DATE/"
    $MV $DIR/Load $DIR/Load$DATE || exit
endif

if( -e $DIR/Check ) then
    echo "Moving Check/ to Check$DATE/"
    $MV $DIR/Check $DIR/Check$DATE || exit
endif

#
# Cleanup of old laser runs
# => erase if more than 1 week old
#
cd $DIR/runs
set nowTime = `$DATEC --utc '+%s' `
foreach run (`ls -1`)
    set rfile = $DIR/runs/$run/first
    if ( -e $rfile) then
        set firstTime = `$CAT $rfile`
        @ tdif = $nowTime - $firstTime
        if ($tdif > 604800) $RM -rf $run
    else
        echo $nowTime > $rfile
    endif
end

echo "Done on `$DATEC `"





