#
  set DOCOPY = /afs/rhic/star/packages/..dev/StRoot/macros
  set file = $1
  set file = /star/rcf/data03/reco/minbias/P00hm/2000/08/st_physics_1231015_raw_0064.event.root
  set diro = $2
  set diro = 2DIR
  mkdir $diro

  set diri = /tmp/doCopy$$
  set diri = 1DIR
  mkdir $diri
  starver .eta2
  root4star -b -q $DOCOPY/doCopy.C\(1,4,\"$file\",\"$diri\"\)
  set filez = ( $diri/*event.root )
  starver .eta3
  root4star -b -q $DOCOPY/doCopy.C\(1,4,\"$filez[1]\",\"$diro\"\)

#  rm -r $diri
#

    
