setenv QTROOTSYSDIR `pwd`/rootsysdir
setenv LD_LIBRARY_PATH $QTROOTSYSDIR/lib:$LD_LIBRARY_PATH
setenv QTROOT_INCLUDE_PATH $QTROOTSYSDIR/include

echo ""
echo  Environment variables to back QtRoot packages:
echo QTROOTSYSDIR $QTROOTSYSDIR
echo LD_LIBRARY_PATH  $LD_LIBRARY_PATH
echo QTROOT_INCLUDE_PATH $QTROOT_INCLUDE_PATH
echo ----------------------------------------
echo ""
