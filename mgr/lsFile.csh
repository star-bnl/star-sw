#
if ( $STAR != "/afs/rhic/star/packages/.DEV") star.dev
echo root.exe -b -q lsF.C\(\"$1\",\"$2\"\)
root.exe -b -q $STAR/mgr/lsF.C\(\"$1,\"$2\"\)
#
