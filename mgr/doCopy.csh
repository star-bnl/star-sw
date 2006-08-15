#
  if ( ! $#argv ) then
    echo "script doCopy.csh  copies ( and converts if needed) .root files"  
    echo "Example:"
    echo "doCopy.csh InputFile OutputDir startEvent nEvents"
    echo "Where for instance:"
    echo "InputFile == /star/rcf/data03/reco/minbias/P00hm/2000/08/st_physics_1231015_raw_0064.event.root"
    echo "OutputDir == myDir, if does not exist, will be created"
    echo "             if omited,  OUT.dir is assumed"
    echo "             it must NOT be your current directory"
    echo "startEvent == 123 (if omited,      1 is assumed)"
    echo "nEvents    == 10  (if omited, 999999 is assumed)"
  exit
  endif



  set DOCOPY = /afs/rhic.bnl.gov/star/packages/.eta3/StRoot/macros
  set file = $1
  echo "doCopy:InputFile = $file"
# set file = /star/rcf/data03/reco/minbias/P00hm/2000/08/st_physics_1231015_raw_0064.event.root

  set diro = $2
  if ( "$diro" == "" || "$diro" == "-") 		set diro = "OUT.dir"
  echo "doCopy:OutputDir = $diro"

  set startEvent = $3
  if ( "$startEvent" == "" || "$startEvent" == "-") 	set startEvent = 1
  echo "doCopy:startEvent = $startEvent"

  set nEvents    = $4
  if ( "$nEvents" == "" || "$nEvents" == "-" ) 		set nEvents = 999999
  echo "doCopy:nEvents    = $nEvents"

  mkdir $diro

  set diri = $diro/.doCopy$$.dir
  mkdir $diri
  starver .eta2

  root4star -b -q $DOCOPY/doCopy.C\($startEvent,$nEvents,\"$file\",\"$diri\"\)
  set filez = ( $diri/*.root )

  starver .eta3
  root4star -b -q $DOCOPY/doCopy.C\(1,$nEvents,\"$filez[1]\",\"$diro\"\)

#  rm -r $diri
#

    
