#!/bin/csh

# /star/u/jecc/tpcwrk/StarDb/Calibrations/tpc


set MKDIR=/bin/mkdir
set LS=/bin/ls
set TAR=/bin/tar
set MV=/bin/mv
set RM=/bin/rm

if( "$1" == "") then
    echo "Oups !! Need argument 1 = path."
    echo "  Will use /star/u/jecc/tpcwrk/StarDb/Calibrations/tpc as default"
    set  DIR=/star/u/jecc/tpcwrk/StarDb/Calibrations/tpc
    set WDIR=/star/u/jecc/tpcwrk/
else
    set  DIR=$1
    set WDIR=/star/u/starreco/scripts
#    set WDIR=/star/u/hjort/tpcwrk2
endif
if ( ! -d $DIR ) then
    echo "Sure !! $DIR not a directory ..."
    exit
endif


# will use in post-fix
set DATE=`/bin/date | /bin/sed "s/ /_/g" | /bin/sed "s/://g"`


#
# Test if file exists and leave if not. 
# This will prevent empty dirs
#
echo "Starting tpcDriftVelocity at `date`"
cd $DIR

$LS tpcDriftVelocity* >&! listOfLaserMacros$DATE.list 
if ( `/bin/grep 'No match' listOfLaserMacros$DATE.list` != "") then
    $RM -f listOfLaserMacros$DATE.list 
    exit
endif


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



#set nLines='100'
#.x LoadLaserDriftVelocityToDb.C("$DIR","listOfLaserMacros$DATE.list",$nLines,"tpcDriftVelocity")

cd $WDIR
echo `pwd`
source ${GROUP_DIR}/.stardev

echo "$DIR listOfLaserMacros$DATE.list"
$LS LoadLaserDriftVelocityToDb.C && ${STAR_BIN}/root4star -b <<EOF 
.x LoadLaserDriftVelocityToDb.C("$DIR","listOfLaserMacros$DATE.list","tpcDriftVelocity")
.q
EOF

# The directory structure is used internally to the .C
# We move now to target
if( -e $DIR/Load ) then
    echo "Moving Load/ to Load$DATE/"
    $MV $DIR/Load $DIR/Load$DATE || exit
endif

if( -e $DIR/Check ) then
    echo "Moving Check/ to Check$DATE/"
    $MV $DIR/Check $DIR/Check$DATE || exit
endif

echo "Done on `date`"





