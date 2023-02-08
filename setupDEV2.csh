if ($?GROUP_DIR) then
if (-e $GROUP_DIR/dropit) then
  setenv MANPATH `$GROUP_DIR/dropit subsys -p $MANPATH`
  setenv LD_LIBRARY_PATH `$GROUP_DIR/dropit subsys -p $LD_LIBRARY_PATH`
  setenv GROUPPATH `$GROUP_DIR/dropit subsys -p $GROUPPATH`
  if ($?PYTHONPATH) setenv PYTHONPATH `$GROUP_DIR/dropit subsys -p $PYTHONPATH`
  setenv PATH `$GROUP_DIR/dropit subsys`
endif
endif
unsetenv AFS_RHIC
unsetenv STAR_ROOT
unsetenv ROOT
unsetenv PROD_LOG
unsetenv GROUP_DIR
unsetenv optstar
unsetenv xoptstar
unsetenv STAF
unsetenv STAF_LIB
unsetenv STAF_BIN
unsetenv STAR
unsetenv STAR_LIB
unsetenv STAR_OBJ
unsetenv STAR_BIN
unsetenv STARL
unsetenv GARFIELD_HOME
unsetenv HEED_DATABASE
unsetenv DB_SERVER_LOCAL_CONFIG
unsetenv STAR_SCRIPTS
unsetenv STAR_CGI
unsetenv STAR_MGR
unsetenv STAR_PAMS
unsetenv TMVASYS
unsetenv CINTSYSDIR
unsetenv IVROOT
unsetenv CLHEP_BASE_DIR
unsetenv XOPTSTAR
unsetenv OPTSTAR
unsetenv QTDIR
unsetenv STAR_PATH
unsetenv GROUP_PATH
unsetenv ROOTROOT
unsetenv ROOTSYS
unsetenv SITE
if ($?NODEBUG) unsetenv NODEBUG
if (-d /net/l402/data/fisyak/STAR) then
  setenv AFS_RHIC  /net/l402/data/fisyak/STAR
    setenv SITE "HLT"
else 
  if (-d /gpfs01/star/subsys-tpc/fisyak/STAR) then 
    setenv AFS_RHIC  /gpfs01/star/subsys-tpc/fisyak/STAR
  else
    setenv AFS_RHIC  /afs/rhic.bnl.gov/star
  endif
endif
if (! $?PERL5LIB) setenv PERL5LIB ""
setenv STAR_ROOT ${AFS_RHIC}
setenv OPTSTAR   ${STAR_ROOT}/opt
setenv GROUP_DIR ${STAR_ROOT}/packages/.DEV2/group
#unsetenv STAR
source ${GROUP_DIR}/group_env.csh
#source ${GROUP_DIR}/setup    gcc482
#if (-d /net/l402/data/fisyak/STAR) then
#source ${GROUP_DIR}/setup    gcc521
#else
source ${GROUP_DIR}/setup    gcc
#endif
#source ${GROUP_DIR}/setup    32b
source ${GROUP_DIR}/setup    64b
#setup gcc492
#setup 32b
#source ${GROUP_DIR}/.starver ${STAR_LEVEL}
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
cat $AFS_RHIC/packages/.DEV2/TFG_Releases.md

