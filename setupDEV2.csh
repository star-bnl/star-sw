if (-d /net/l402/data/fisyak/STAR) then
  setenv AFS_RHIC  /net/l402/data/fisyak/STAR
else 
  if (-d /star/subsys/tpc/fisyak/STAR) then 
    setenv AFS_RHIC  /star/subsys/tpc/fisyak/STAR
  else
    setenv AFS_RHIC  /afs/rhic.bnl.gov/star
  endif
endif
setenv STAR_ROOT ${AFS_RHIC}
setenv OPTSTAR   ${STAR_ROOT}/opt
setenv GROUP_DIR ${STAR_ROOT}/packages/.DEV2/group
#unsetenv STAR
unsetenv GROUP_PATH
unsetenv STAR_PATH
unsetenv ROOTROOT
unsetenv ROOTSYS
unsetenv ROOT_LEVEL
unsetenv CERN_LEVEL
source ${GROUP_DIR}/group_env.csh
source ${GROUP_DIR}/setup    gcc482
#source ${GROUP_DIR}/setup    32b
#setup gcc492
#setup 32b
source ${GROUP_DIR}/.starver ${STAR_LEVEL}
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
