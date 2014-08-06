#!/bin/csh

#
# Based on work by J. Castillo
# /star/u/jecc/tpcwrk/StarDb/Calibrations/tpc
#
# Modifieded on 31 Jan 2007 by G. Van Buren to use
# tags.root (now laser.root) files with LoopOverLaserTree.C and
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
set CUT=/usr/bin/cut
set DATEC=/bin/date
set TOUCH=/bin/touch
set SED=/bin/sed
set CAT=/bin/cat
set GZIP=/usr/bin/gzip
set SORT=/bin/sort
set FIND=/usr/bin/find
set MYSQL=/usr/bin/mysql
set MYDB="$MYSQL -h robinson.star.bnl.gov -P 3306 -C Calibrations_tpc"
set MYROOT="${ROOTSYS}/bin/root.exe -b -l"
set MYROOT4STAR="$STAR/.$STAR_HOST_SYS/bin/root4star -b -l"

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
# Determine what new laser.root files exist
#
set laserdirs = "/star/data{09,10,11,12}/reco"
set laserfiles = `$FIND $laserdirs -name "st_laser_*.laser.root" -cnewer $timeFile -wholename "*ield/dev/20*"`
$TOUCH $timeFile

#
# Test if any new files and leave if not. 
# This will prevent empty dirs
#
if ($#laserfiles == 0) exit

#
# copy new files to $DIR and determine runs to process
#
set listOfRuns = `echo ${laserfiles:gh:gt} | $SED "s/ /\n/g" | $SORT -n -u`
foreach run ($listOfRuns)
  set runDir = $DIR/runs/$run
  $MKDIR -p $runDir
end
foreach laserfile ($laserfiles)
  set runDir = $DIR/runs/${laserfile:h:t}
  $CP $laserfile $runDir
end

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
    $MYROOT <<EOF 
.x $WDIR/LoopOverLaserTrees.C+("st_laser_*.laser.root")
.q
EOF
    set laserMacro = `$LS tpcDriftVelocity*`
    if ( $#laserMacro != 0) then
        set macros = $runDir/macros
        $MKDIR -p $macros

        # Look for old macro files
        cd $macros
        set oldLaserMacro = `$LS -t -1 tpcDriftVelocity.*`
        if ( $#oldLaserMacro != 0) then

            set oldMacro = $oldLaserMacro[1]
            set oldDate = ${oldMacro:r:r:e}
            set oldTime = ${oldMacro:r:e}
            set oldYear = `echo $oldDate | $CUT -c 1-4 `
            set oldMonth = `echo $oldDate | $CUT -c 5-6 `
            set oldDay = `echo $oldDate | $CUT -c 7-8 `
            set oldHour = `echo $oldTime | $CUT -c 1-2 `
            set oldMin = `echo $oldTime | $CUT -c 3-4 `
            set oldSec = `echo $oldTime | $CUT -c 5-6 `
            set oldBeginTime = "${oldYear}-${oldMonth}-${oldDay} ${oldHour}:${oldMin}:${oldSec}"
            echo "Deactivating old DB entry with beginTime=${oldBeginTime}";
            $MYDB -e "UPDATE tpcDriftVelocity SET entryTime=entryTime,deactive=UNIX_TIMESTAMP(CURRENT_TIMESTAMP) WHERE beginTime='${oldBeginTime}' and deactive=0;"
            echo "UPDATE tpcDriftVelocity SET entryTime=entryTime,deactive=UNIX_TIMESTAMP(CURRENT_TIMESTAMP) WHERE beginTime='${oldBeginTime}' and deactive=0;"

            @ ndeactive = 1
            set deactiveLaserMacro = deactive_${oldMacro}_${ndeactive}
            while (-e $deactiveLaserMacro)
                @ ndeactive ++
                set deactiveLaserMacro = deactive_${oldMacro}_${ndeactive}
            end
            $MV $oldMacro $deactiveLaserMacro
        endif

        $CP $runDir/$laserMacro $DIR/$laserMacro
        $MV $runDir/$laserMacro $macros/$laserMacro
        $MV $runDir/LaserPlots.root $DIR/LaserPlots.$run.root
        $TOUCH $laserFiles
        echo $laserMacro >> $laserFiles
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
echo $PWD

echo "$DIR $listLaserFiles"
setenv DB_ACCESS_MODE write
$LS LoadLaserDriftVelocityToDb.C && $MYROOT4STAR <<EOF 
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
foreach run (`$LS -1`)
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





