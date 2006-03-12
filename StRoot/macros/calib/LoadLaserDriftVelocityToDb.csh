#!/bin/csh

# /star/u/jecc/tpcwrk/StarDb/Calibrations/tpc

if( "$1" == "") then
    echo "Oups !! Need argument 1 = path."
    echo "  Will use /star/u/jecc/tpcwrk/StarDb/Calibrations/tpc as default"
    set DIR=/star/u/jecc/tpcwrk/StarDb/Calibrations/tpc
else
    set DIR=$1
endif
if ( ! -d $DIR ) then
    echo "Sure !! $DIR not a directory ..."
    exit
endif


set DATE=`/bin/date | /bin/sed "s/ /_/g" | /bin/sed "s/://g"`

if( -e $DIR/Load ) then
    echo "Moving Load/ to LoadSavedOn$DATE/"
    /bin/mv $DIR/Load $DIR/LoadSavedOn$DATE || exit
    /bin/tar -czf $DIR/LoadSavedOn$DATE.tar.gz $DIR/LoadSavedOn$DATE >&/dev/null &
endif
echo "Creating $DIR/Load"
/bin/mkdir $DIR/Load || exit
echo "Creating $DIR/Load/Done"
/bin/mkdir $DIR/Load/Done || exit
echo "Creating $DIR/Load/Failed"
/bin/mkdir $DIR/Load/Failed || exit
echo "Creating $DIR/Load/Others"
/bin/mkdir $DIR/Load/Others || exit

if( -e $DIR/Check ) then
    echo "Moving Check/ to CheckSavedOn$DATE/"
    /bin/mv $DIR/Check $DIR/CheckSavedOn$DATE || exit
    /bin/tar -czf $DIR/CheckSavedOn$DATE.tar.gz $DIR/CheckSavedOn$DATE >&/dev/null &
endif
echo "Creating $DIR/Check"
/bin/mkdir $DIR/Check || exit
echo "Creating $DIR/Check/BadRun"
/bin/mkdir $DIR/Check/BadRun || exit
echo "Creating $DIR/Check/VarSel"
/bin/mkdir $DIR/Check/VarSel || exit
echo "Creating $DIR/Check/VarOth"
/bin/mkdir $DIR/Check/VarOth || exit


echo "Starting tpcDriftVelocity at `date`"
cd $DIR
/bin/ls tpcDriftVelocity* >&! listOfLaserMacros$DATE.list
#set nLines='100'
#.x LoadLaserDriftVelocityToDb.C("$DIR","listOfLaserMacros$DATE.list",$nLines,"tpcDriftVelocity")
cd /star/u/jecc/tpcwrk/
source ${GROUP_DIR}/.stardev
/bin/ls LoadLaserDriftVelocityToDb.C
${STAR_BIN}/root4star -b <<EOF 
.x LoadLaserDriftVelocityToDb.C("$DIR","listOfLaserMacros$DATE.list","tpcDriftVelocity")
.q
EOF

if( -e $DIR/Load ) then
    echo "Moving Load/ to Load$DATE/"
    /bin/mv $DIR/Load $DIR/Load$DATE || exit
endif

if( -e $DIR/Check ) then
    echo "Moving Check/ to Check$DATE/"
    /bin/mv $DIR/Check $DIR/Check$DATE || exit
endif

echo "Done on `date`"





