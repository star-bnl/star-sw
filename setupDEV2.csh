setenv AFS_RHIC  /star/subsys/tpc/fisyak/STAR
setenv STAR_ROOT ${AFS_RHIC}
setenv OPTSTAR   ${STAR_ROOT}/opt
#setenv  GROUP_DIR /afs/rhic.bnl.gov/star/packages/.DEV2/group
#source ${GROUP_DIR}/setup    gcc482
#source ${GROUP_DIR}/setup    32b
#setup gcc492
#setup 32b
source ${GROUP_DIR}/.starver .DEV2
#setup   64b
#setup   32b
#if ($STAR_SYS == "x8664_sl5") 
#setup   gcc451
#setenv  PREFIX /afs/rhic.bnl.gov/star/users/fine/data08/STAR/OPTSTAR/.$STAR_HOST_SYS
#setenv  OPTSTAR $PREFIX
#setenv  LD_LIBRARY_PATH $OPTSTAR/lib:`dropit opt -p $LD_LIBRARY_PATH`
#setenv  PATH $OPTSTAR/Qt4/bin:$OPTSTAR/bin:`dropit opt -p $PATH`
#setenv  QTDIR $OPTSTAR/Qt4
#setenv  PATH $QTDIR/bin:$OPTSTAR/bin:$PATH
#setenv  LD_LIBRARY_PATH $QTDIR/lib:$LD_LIBRARY_PATH
#setenv  CXXFLAGS " -m32"
#setenv  CFLAGS  " -m32"
#setenv  LDFLAGS " -m32"
